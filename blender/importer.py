import bpy, os, inspect, sys, math, mathutils, collections
from gi.repository import Cdn

import codyn

class CodynImport(bpy.types.Operator):
    """Import a codyn model"""
    bl_idname = "object.codyn_import"
    bl_label = "Import Codyn Model"
    bl_description = "Import a codyn model"

    filepath = bpy.props.StringProperty(subtype="FILE_PATH")

    def make_object(self, context, name, mesh=None):
        if name in bpy.data.objects:
            ret = bpy.data.objects[name]

            if ret.users != 0:
                return ret
            else:
                ret.name = '__{0}'.format(name)

        if not mesh is None:
            mesh = mesh()

        ret = bpy.data.objects.new(name, mesh)
        context.scene.objects.link(ret)

        if mesh is None:
            ret.empty_draw_size = 0.1

        return ret

    def inertia_is_diag(self, inertia):
        return inertia[0][1] == 0 and \
               inertia[0][2] == 0 and \
               inertia[1][0] == 0 and \
               inertia[2][0] == 0 and \
               inertia[1][2] == 0 and \
               inertia[2][1] == 0

    def inertia_like_sphere(self, m, inertia):
        if not self.inertia_is_diag(inertia) or m == 0:
            return False

        if inertia[0][0] == inertia[1][1] and \
           inertia[1][1] == inertia[2][2]:
            return math.sqrt(inertia[0][0] / (2.0 / 5.0 * m))

        return False

    def inertia_like_cylinder(self, m, inertia):
        if not self.inertia_is_diag(inertia) or m == 0:
            return False

        f = 1.0 / 12.0 * m

        a = inertia[0][0] / f
        b = inertia[1][1] / f
        c = inertia[2][2] / (0.5 * m)

        r2 = c
        h2 = a - (3 * r2)

        if a == b and a != c and h2 > 0:
            return math.sqrt(r2), math.sqrt(h2)

        return False

    def inertia_like_box(self, m, inertia):
        if not self.inertia_is_diag(inertia) or m == 0:
            return False

        f = 1.0 / 12.0 * m

        a = inertia[0][0] / f
        b = inertia[1][1] / f
        c = inertia[2][2] / f

        w2 = (c - a + b) / 2
        d2 = b - w2
        h2 = a - d2

        if w2 > 0 and d2 > 0 and h2 > 0:
            return math.sqrt(w2), math.sqrt(d2), math.sqrt(h2)

        return False

    def make_matrix(self, m):
        dim = m.dimension()
        v = m.get_flat()

        ret = []
        i = 0

        for c in range(0, dim.columns):
            for r in range(0, dim.rows):
                if c == 0:
                    ret.append([0] * dim.columns)

                ret[r][c] = v[i]
                i += 1

        return ret

    def try_make_shapes(self, context, body, shapes):
        retval = []
        name = '{0}_shape'.format(body.get_id())

        for shape in shapes:
            o = None

            if shape.find_object('self | first | has-template(physics.rendering.box)'):
                bpy.ops.mesh.primitive_cube_add()
                o = context.active_object

                size = shape.get_variable('size').get_values().get_flat()
                o.scale = mathutils.Vector(size)

                bpy.ops.object.transform_apply(scale=True)

                o.name = '{0}_{1}_box'.format(name, len(retval))

            elif shape.find_object('self | first | has-template(physics.rendering.sphere)'):
                r = shape.get_variable('radius').get_values().get_flat()[0]

                bpy.ops.mesh.primitive_ico_sphere_add(subdivisions=4, size=r)

                o = context.active_object
                o.name = '{0}_{1}_sphere'.format(name, len(retval))

            elif shape.find_object('self | first | has-template(physics.rendering.cylinder)'):
                r = shape.get_variable('radius').get_values().get_flat()[0]
                h = shape.get_variable('height').get_values().get_flat()[0]

                bpy.ops.mesh.primitive_cylinder_add(vertices=12, radius=r, depth=h)

                o = context.active_object
                o.name = '{0}_{1}_cylinder'.format(name, len(retval))

            elif shape.find_object('self | first | has-template(physics.rendering.plane)'):
                bpy.ops.mesh.primitive_plane_add()
                o = context.active_object

                o.scale = mathutils.Vector((1000, 1000, 0))
                bpy.ops.object.transform_apply(scale=True)

                o.name = '{0}_{1}_plane'.format(name, len(retval))

            if not o is None:
                tr = shape.get_variable('transform').get_values()
                m = codyn.matrix_to_mat4x4(tr)
                o.matrix_local = m

                col = shape.get_variable('color').get_values().get_flat()
                mat = bpy.data.materials.new('mat_' + o.name)
                mat.diffuse_color = col
                o.data.materials.append(mat)

                retval.append(o)

        return retval

    def try_make_shape(self, context, body, com):
        shapes = body.find_objects('has-template(physics.rendering.shape)')

        if len(shapes) > 0:
            return self.try_make_shapes(context, body, shapes)

        m = body.get_variable('m').get_value()
        I = body.get_variable('I').get_values()

        # Inertia matrix
        inertia = self.make_matrix(I)
        name = '{0}_shape'.format(body.get_id())

        r = self.inertia_like_sphere(m, inertia)

        if r:
            bpy.ops.mesh.primitive_ico_sphere_add(subdivisions=4, size=r)
            context.active_object.name = name
            context.active_object.location = com.get_values().get_flat()

            return [context.active_object]

        c = self.inertia_like_cylinder(m, inertia)

        if c:
            bpy.ops.mesh.primitive_cylinder_add(vertices=12, radius=c[0], depth=c[1])
            context.active_object.name = name
            context.active_object.location = com.get_values().get_flat()

            return [context.active_object]

        b = self.inertia_like_box(m, inertia)

        if b:
            bpy.ops.mesh.primitive_cube_add()
            context.active_object.name = name
            context.active_object.scale = mathutils.Vector(b)
            bpy.ops.object.transform_apply(scale=True)

            context.active_object.location = com.get_values().get_flat()

            return [context.active_object]
        else:
            return []

    def add_camera(self, context, body, camera):
        bpy.ops.object.camera_add()
        o = context.active_object

        o.parent = body
        o.matrix_local = codyn.matrix_to_mat4x4(camera.get_variable('transform').get_values())

        ortho = camera.get_variable('orthographic')

        if not ortho is None and ortho.get_value() != 0:
            o.data.type = 'ORTHO'

        scale = camera.get_variable('orthographicScale')

        if not scale is None:
            o.data.ortho_scale = scale.get_value()

        return o

    def has_applied_template(self, obj, template):
        templmap = {}
        templ = collections.deque(obj.get_applied_templates())

        while len(templ) > 0:
            t = templ.popleft()
            templmap[t] = True

            if t == template:
                return True

            for p in t.get_applied_templates():
                if not p in templmap:
                    templ.append(p)

        return False

    def import_system(self, cdnobj, context, system):
        bodies = system.find_objects('has-template(physics.body)')
        ret = []

        # Dummy object for system
        sysobj = self.make_object(context, system.get_id(), lambda: bpy.data.meshes.new(system.get_id()))
        sysobj.parent = cdnobj
        ret.append(sysobj)

        objmap = {}
        nodemap = {}

        shapes = system.find_objects('has-template(physics.rendering.shape)')
        shapes = self.try_make_shapes(context, system, shapes)

        for shape in shapes:
            shape.parent = sysobj
            ret.append(shape)

        for camera in system.find_objects('has-template(physics.rendering.camera)'):
            ret.append(self.add_camera(context, sysobj, camera))

        for body in bodies:
            # Center of mass
            com = body.get_variable("com")

            bid = body.get_id()
            obj = self.make_object(context, bid)

            if not 'cdn_node' in obj.game.properties:
                context.scene.objects.active = obj
                bpy.ops.object.game_property_new(type='STRING', name='cdn_node')

            obj.game.properties['cdn_node'].value = body.get_full_id()

            csid = '{0}_cs'.format(bid)

            cs = self.make_object(context, csid, lambda: bpy.data.meshes['coordinate_system'])
            cs.scale = [0.1, 0.1, 0.1]
            cs.parent = obj

            # Attach coordinate system visualization
            comid = '{0}_com'.format(bid)

            comobj = self.make_object(context, comid, lambda: bpy.data.meshes.new(comid))
            comobj.parent = obj

            shapes = self.try_make_shape(context, body, com)

            for shape in shapes:
                shape.parent = obj
                ret.append(shape)

            contacts = body.find_objects('has-template(physics.contacts.soft)')

            for c in contacts:
                # Add contact force vector at each location
                locs = c.get_variable('location').get_values()
                locvals = locs.get_flat()
                dim = locs.dimension()

                for cc in range(dim.columns):
                    bpy.ops.object.add_named(linked=True, name='force')
                    fobj = context.active_object

                    bpy.ops.object.add_named(linked=True, name='force_top')
                    fobjtop = context.active_object

                    bpy.ops.object.add_named(linked=True, name='force_bottom')
                    fobjbottom = context.active_object

                    fobj.name = '{0}_force_{1}'.format(bid, cc)
                    fobjtop.name = '{0}_force_top_{1}'.format(bid, cc)
                    fobjbottom.name = '{0}_force_bottom_{1}'.format(bid, cc)

                    fobjtop.parent = fobj
                    fobjbottom.parent = fobj

                    fobj.scale = [0.1, 0.1, 0.1]
                    fobj.parent = obj

                    istart = cc * dim.rows
                    fobj.location = locvals[istart:istart+3]

                    if not 'cdn_force' in obj.game.properties:
                        context.scene.objects.active = fobj
                        bpy.ops.object.game_property_new(type='STRING', name='cdn_force')

                    fobj.game.properties['cdn_force'].value = '{0}:{1}'.format(c.get_full_id(), cc)

                    ret.append(fobj)
                    ret.append(fobjtop)
                    ret.append(fobjbottom)

            ret.append(obj)
            ret.append(cs)
            ret.append(comobj)

            objmap[obj] = body
            nodemap[body] = obj

            codyn.data.blender_to_node[obj] = body
            codyn.data.node_to_blender[body] = obj

            codyn.sync_variables_to_blender(obj)
            comobj.location = com.get_values().get_flat()

        pedge = system.find_object('templates-root . physics . joint')

        for body in bodies:
            obj = nodemap[body]

            for edge in body.get_edges():
                hastempl = self.has_applied_template(edge, pedge)

                if hastempl and edge.get_output() == body:
                    parnode = edge.get_input()

                    if not parnode in nodemap:
                        break

                    parobj = nodemap[parnode]

                    obj.parent = parobj
                    break

            if not obj.parent:
                obj.parent = sysobj

            # Transform
            nodemap[body].matrix_local = codyn.local_blender_transform(body)

        return ret

    def link_library(self, context):
        filename = inspect.getframeinfo(inspect.currentframe()).filename
        dirname = os.path.dirname(filename)

        with bpy.data.libraries.load(os.path.join(dirname, 'data', 'library.blend')) as (data_from, data_to):
            for attr in ['materials', 'meshes', 'objects', 'groups']:
                setattr(data_to, attr, getattr(data_from, attr))

        sobjs = context.scene.objects
        bobjs = bpy.data.objects

        if (not 'force' in sobjs) and ('force' in bobjs):
            nm = ['force', 'force_top', 'force_bottom']

            for n in nm:
                sobjs.link(bobjs[n])

            for n in nm:
                sobjs[n].layers[1] = True
                sobjs[n].layers[0] = False

            sobjs['force'].location = mathutils.Vector((0, 0, 0))

        context.scene.update()

    def execute(self, context):
        context.scene.cursor_location = [0, 0, 0]

        self.link_library(context)

        path = self.filepath

        try:
            network = Cdn.Network.new_from_path(path)
        except Exception as e:
            self.report({'ERROR'}, 'Failed to load network: ' + str(e))
            return {'FINISHED'}

        err = Cdn.CompileError()

        if not network.compile(None, err):
            self.report({'ERROR'}, 'Failed to compile network: ' + err.get_formatted_string())
            return {'FINISHED'}

        objmap = {}
        nodemap = {}
        objs = []

        name = os.path.basename(path)
        cdnobj = self.make_object(context, name)
        cdnobj['cdn_filename'] = path

        context.scene.objects.active = cdnobj

        if not 'init_sensor' in cdnobj.game.sensors:
            bpy.ops.logic.sensor_add(type='ALWAYS', name='init_sensor')

            inits = cdnobj.game.sensors['init_sensor']
            inits.use_tap = True

            bpy.ops.logic.controller_add(type='PYTHON', name='init_controller')

            ctrl = cdnobj.game.controllers['init_controller']
            ctrl.mode = 'MODULE'
            ctrl.module = 'blender_codyn.simulator_init'
            ctrl.states = 1

            bpy.ops.logic.actuator_add(type='STATE', name='init_actuator')
            act = cdnobj.game.actuators['init_actuator']
            act.operation = 'SET'
            act.states[1] = True

            ctrl.link(sensor=inits, actuator=act)

            bpy.ops.logic.sensor_add(type='ALWAYS', name='loop_sensor')
            loops = cdnobj.game.sensors['loop_sensor']
            loops.use_pulse_true_level = True

            bpy.ops.logic.controller_add(type='PYTHON', name='loop_controller')
            ctrl = cdnobj.game.controllers['loop_controller']
            ctrl.states = 2
            ctrl.mode = 'MODULE'
            ctrl.module = 'blender_codyn.simulator_loop'
            loops.link(ctrl)

            bpy.ops.object.game_property_new(type='STRING', name='cdn_filename')
            cdnobj.game.properties['cdn_filename'].value = path

        systems = network.find_objects('has-template(physics.system)')

        for system in systems:
            objs.extend(self.import_system(cdnobj, context, system))

        bpy.ops.object.select_all(action="DESELECT")

        for o in objs:
            o.select = True

        if len(objs):
            context.scene.objects.active = objs[0]

        context.scene.layers[0] = True
        context.scene.render.engine = 'BLENDER_GAME'

        codyn.data.networks[name].cdn = network
        codyn.data.networks[name].filename = path
        codyn.data.networks[name].mtime = os.path.getmtime(path)
        codyn.data.networks[name].rawc = None

        context.scene.update()

        return {"FINISHED"}

    def invoke(self, context, event):
        context.window_manager.fileselect_add(self)
        return {'RUNNING_MODAL'}

# vi:ts=4:et

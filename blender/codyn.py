import mathutils
from gi.repository import Cdn

class Data(dict):
    def __getattribute__(self, name):
        return self[name]

    def __getitem__(self, key):
        try:
            return dict.__getitem__(self, key)
        except KeyError:
            d = Data()
            self[key] = d
            return d

    def __setattr__(self, name, val):
        self[name] = val

data = Data()

def sync_variable_to_blender(obj, node, vname):
    v = node.get_variable(vname)

    dim = v.get_dimension()
    num = dim.size()

    if num > 1:
        pname = 'cdn_{0}{1}'.format(vname, num)
        setattr(obj, pname, v.get_values().get_flat())
    else:
        pname = 'cdn_{0}'.format(vname)
        setattr(obj, pname, v.get_value())

def to_mat(m):
    v = m.get_flat()
    mat = mathutils.Matrix()

    mat[0] = mathutils.Vector([v[0], v[4], v[8],  v[12]])
    mat[1] = mathutils.Vector([v[1], v[5], v[9],  v[13]])
    mat[2] = mathutils.Vector([v[2], v[6], v[10], v[14]])
    mat[3] = mathutils.Vector([v[3], v[7], v[11], v[15]])

    return mat

def local_blender_transform(node):
    jtmat = to_mat(node.get_variable("localMatrix").get_values())
    return jtmat

def sync_variables_to_blender(obj):
    node = data.blender_to_node[obj]

    if node is None:
        return

    # Set q, dq and ddq
    sync_variable_to_blender(obj, node, 'q')
    sync_variable_to_blender(obj, node, 'dq')
    sync_variable_to_blender(obj, node, 'ddq')

    obj.cdn_mass = node.get_variable('m').get_value()

def variable_sync(name, ndof):
    def variable_sync_impl(obj, context):
        node = data.blender_to_node[obj]

        if node is None:
            return

        v = node.get_variable(name)
        dim = v.get_dimension()

        if dim.size() != ndof:
            return

        if ndof == 1:
            nm = 'cdn_{0}'.format(name)
            prop = [getattr(obj, nm)]
        else:
            nm = 'cdn_{0}{1}'.format(name, ndof)
            prop = getattr(obj, nm)

        if len(prop) != dim.size():
            return

        v.set_values(Cdn.Matrix.new_flat(prop[0:], dim))

        if name == 'q':
            obj.matrix_local = local_blender_transform(node)

    return variable_sync_impl

#def add_object_button(self, context):
#    self.layout.operator(
#        CodynImport.bl_idname,
#        text="Import Codyn Model",
#        icon="PLUGIN")

#def register():
#    bpy.utils.register_class(CodynImport)
#    bpy.types.INFO_MT_add.append(add_object_button)

#    bpy.utils.register_class(CodynPanel)

#    bpy.ops.object.codyn_import()

#def unregister():
#    bpy.utils.unregister_class(CodynImport)
#    bpy.types.INFO_MT_add.remove(add_object_button)

#    bpy.utils.unregister_class(CodynPanel)

#    network = None

#def update_blender_transformations():
#    for obj in gamemap:
#        obj.localTransform = local_blender_transform(gamemap[obj])

#def simulation_start():
#    global network, gamemap

#    bge = sys.modules['bge']

#    cont = bge.logic.getCurrentController()
#    obj = cont.owner

#    gamemap = {}

#    sysnode = network.get_child(obj.name)

#    for child in obj.childrenRecursive:
#        ch = sysnode.get_child(child.name)

#        if ch:
#            gamemap[child] = ch

#    # Reset the network
#    network.reset()
#    update_blender_transformations()

#def simulation_quit():
#    global network, gamemap

#    bge = sys.modules['bge']

#    cont = bge.logic.getCurrentController()
#    obj = cont.owner

#    bge.logic.endGame()

#def simulation_step():
#    global network

#    bge = sys.modules['bge']

#    cont = bge.logic.getCurrentController()
#    obj = cont.owner

#    update_blender_transformations()

#    for i in range(0, 16):
#        network.step(0.001)

#if __name__ == "__main__":
#    register()

# vi:ex:ts=4:et

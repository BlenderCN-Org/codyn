import bpy

class CodynPanel(bpy.types.Panel):
    bl_label = "Codyn"
    bl_idname = "OBJECT_PT_codyn"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"

    def props_for(self, obj, node, row, vname):
        row.label(text='%s:' % (vname, ))

        q = node.get_variable(vname)
        dim = q.get_dimension()
        ndof = dim[0] * dim[1]

        nm = 'cdn_%s%d' % (vname, ndof,)

        for i in range(0, ndof):
            row.prop(obj, nm, text='%d' % (i + 1,), index=i)

    def draw(self, context):
        global objmap

        obj = context.object

        if not obj in objmap:
            return

        node = objmap[obj]
        layout = self.layout

        row = layout.row()
        row.label(text='Generalized Coordinates')

        row = layout.row()

        self.props_for(obj, node, row.column(), 'q')
        self.props_for(obj, node, row.column(), 'dq')
        self.props_for(obj, node, row.column(), 'ddq')

        row = layout.row()
        row.label(text='Inertial Properties')

        row = layout.row()
        row.prop(obj, 'cdn_mass', text='Mass')


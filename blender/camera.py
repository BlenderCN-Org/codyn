import mathutils, math

rotate_sensitivity = 1
translate_sensitivity = 500
zoom_sensitivity = 1
in_view = False
center = mathutils.Vector((0, 0, 0))
translate_center = True

original_rotation = mathutils.Quaternion((1, 0, 0, 0))
original_translation = mathutils.Vector((0, 0, 0))
initial_translation = mathutils.Vector((0, 0, 0))
initial_rotation = mathutils.Quaternion((1, 0, 0, 0))
current_rotation = mathutils.Quaternion((1, 0, 0, 0))

ACTION_ROTATE = 1
ACTION_TRANSLATE = 2

current_action = 0

def mouse_to_sphere(camera, pos):
    import bge

    proj = camera.projection_matrix

    width = bge.render.getWindowWidth()
    height = bge.render.getWindowHeight()

    radius = max(width, height) * rotate_sensitivity / 2

    # calculate x, y on the arcball sphere
    norm = mathutils.Vector((((1 - pos[0]) * width - width / 2) / radius,
                             (pos[1] * height - height / 2) / radius))

    # estimate z
    r = norm[0] * norm[0] + norm[1] * norm[1]

    if r > 1:
        # was outside of radius, move to z == 0
        s = 1 / math.sqrt(r)
        return mathutils.Vector((s * norm[0], s * norm[1], 0))
    else:
        # is inside of radius, move z on radius
        return mathutils.Vector((norm[0], norm[1], math.sqrt(1 - r)))

def final_rotation():
    xyz = initial_rotation.cross(current_rotation)
    w = initial_rotation.dot(current_rotation)

    return mathutils.Quaternion((w, xyz[0], xyz[1], xyz[2]))

def do_mouse_rotate(camera, mouse, keyboard, event):
    import bge
    global original_rotation, original_translation, initial_rotation, current_rotation, initial_translation, center

    if event == bge.logic.KX_INPUT_JUST_ACTIVATED:
        # Begin rotate
        original_rotation = camera.worldOrientation.copy()
        original_translation = original_rotation.transposed() * (camera.worldPosition - center)
        initial_rotation = mouse_to_sphere(camera, mathutils.Vector(mouse.position))

        current_action = ACTION_ROTATE

    elif event == bge.logic.KX_INPUT_JUST_RELEASED:
        # End rotate
        current_action = 0
    else:
        # Update rotation
        current_rotation = mouse_to_sphere(camera, mathutils.Vector(mouse.position))

        rotation = final_rotation()
        rotation.rotate(original_rotation)

        camera.worldOrientation = rotation
        camera.worldPosition = rotation * original_translation + center

def do_mouse_translate(camera, mouse, keyboard, event):
    import bge
    global original_rotation, original_translation, initial_rotation, current_rotation, initial_translation, center

    if event == bge.logic.KX_INPUT_JUST_ACTIVATED:
        # Begin rotate
        original_translation = camera.worldPosition.copy()
        initial_translation = mathutils.Vector(mouse.position)

        current_action = ACTION_TRANSLATE

    elif event == bge.logic.KX_INPUT_JUST_RELEASED:
        # End translate
        current_action = 0

        if translate_center:
            center += camera.worldPosition - original_translation
    else:
        # Moving
        current_translation = mathutils.Vector(mouse.position)
        l = (original_translation - center).length

        proj = camera.projection_matrix

        width = bge.render.getWindowWidth()
        height = bge.render.getWindowHeight()

        ratio = mathutils.Vector((2 * l / (proj[0][0] * width),
                                  2 * l / (proj[1][1] * height)))

        translation = (initial_translation - current_translation) * translate_sensitivity
        translation[0] *= ratio[0]
        translation[1] *= ratio[1]

        tp = camera.worldOrientation.transposed()

        side = tp[0]
        up = tp[1]

        camera.worldPosition = original_translation + side * translation[0] + -up * translation[1]

def do_mouse_cancel(camera, mouse, keyboard, event):
    pass

mouse_rotate = [0, 'none', do_mouse_rotate]
mouse_translate = [1, 'shift', do_mouse_translate]
mouse_cancel = [2, 'none', do_mouse_cancel]

def any_mod_active(keyboard, mods):
    import bge

    for mod in mods:
        if mod in keyboard.events and \
           (keyboard.events[mod] == bge.logic.KX_INPUT_JUST_ACTIVATED or \
            keyboard.events[mod] == bge.logic.KX_INPUT_ACTIVE):
            return True

    return False

def check_and_run(camera, mouse, keyboard, item):
    import bge

    buttons = [bge.events.LEFTMOUSE, bge.events.MIDDLEMOUSE, bge.events.RIGHTMOUSE]

    mods = {
        'shift': [bge.events.RIGHTSHIFTKEY, bge.events.LEFTSHIFTKEY],
        'ctrl': [bge.events.RIGHTCTRLKEY, bge.events.LEFTCTRLKEY],
        'alt': [bge.events.RIGHTALTKEY, bge.events.LEFTALTKEY],
    }

    button = buttons[item[0]]

    if button in mouse.active_events and \
       (mouse.active_events[button] == bge.logic.KX_INPUT_JUST_ACTIVATED or \
        mouse.active_events[button] == bge.logic.KX_INPUT_JUST_RELEASED or \
        mouse.events[button] == bge.logic.KX_INPUT_ACTIVE) and \
       (item[1] == 'none' or any_mod_active(keyboard, mods[item[1]])):
        item[2](camera, mouse, keyboard, mouse.active_events[button])

def update(camera):
    import bge

    mouse = bge.logic.mouse
    keyboard = bge.logic.keyboard

    for item in [mouse_rotate, mouse_translate]:
        check_and_run(camera, mouse, keyboard, item)

# vi:ts=4:et

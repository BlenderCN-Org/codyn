import os, platform

from . import codyn

from gi.repository import Cdn
import cdnrawc

def find_and_load_rawc(data):
    basename = os.path.basename(data.filename)

    if platform.system() == 'Darwin':
        ext = '.dylib'
    elif platform.system() == 'Windows':
        ext = '.dll'
    else:
        ext = '.so'

    name = os.path.splitext(basename)[0]

    rawclib = os.path.join(os.path.dirname(data.filename), 'lib{0}{1}'.format(name, ext))

    # Check if <rawclib> is newer
    try:
        rawctime = os.path.getmtime(rawclib)

        if rawctime > data.mtime:
            # Load rawc version of the network
            data.rawc = cdnrawc.Network(name, rawclib)
            data.mtime = rawctime
        elif rawctime < data.mtime:
            data.rawc = None
    except OSError:
        data.rawc = None

class SimulatorCodyn:
    def __init__(self, data):
        self.data = data

    def reset(self):
        self.data.cdn.reset()

class SimulatorRawc:
    def __init__(self, data):
        self.data = data

    def reset(self):
        self.data.rawc.reset(0)

def update(obj, network):
    pass

def init():
    import bge

    cont = bge.logic.getCurrentController()
    owner = cont.owner

    filename = owner['cdn_filename']
    mtime = os.path.getmtime(filename)

    if not owner.name in codyn.data.networks or \
       mtime > codyn.data.networks[owner.name].mtime:
        # Load network from property
        network = Cdn.Network.new_from_path(filename)
        network.compile(None, None)

        data = codyn.data.networks[owner.name]

        data.cdn = network
        data.filename = filename
        data.mtime = mtime
        data.rawc = None

    # Load rawc version if it's there
    data = codyn.data.networks[owner.name]
    find_and_load_rawc(data)

    if not data.rawc is None:
        data.simulator = SimulatorRawc(data)
    else:
        data.simulator = SimulatorCodyn(data)

    data.simulator.reset()
    update(owner, data)

    cont.activate('init_actuator')

def loop():
    import bge

    cont = bge.logic.getCurrentController()
    owner = cont.owner

    update(owner, codyn.data.networks[owner.name])

# vi:ts=4:et

from cdnrepl import command
import sys

def run():
    c = command.Command()

    if len(sys.argv) > 1:
        if not c.load_network(sys.argv[1]):
            sys.exit(1)

    c.cmdloop()

if __name__ == '__main__':
    run()

# vi:ts=4:et

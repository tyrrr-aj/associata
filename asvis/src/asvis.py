import ctrl
import sys



def main():
    print('Asvis started')
    print(f'Python interpreter: {sys.executable}, version: {sys.version}')

    if len(sys.argv) != 2:
        out_path = '.'  # Default output path
    else:
        out_path = sys.argv[1]

    ctrl.Ctrl(out_path).start()


if __name__ == "__main__":
    main()

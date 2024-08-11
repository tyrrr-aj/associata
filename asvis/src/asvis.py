import ctrl
import sys
import asyncio



def main():
    print('Asvis started')
    print(f'Python interpreter: {sys.executable}, version: {sys.version}')

    if len(sys.argv) < 2:
        out_path = '.'  # Default output path
    else:
        out_path = sys.argv[1]

    client_node_name = sys.argv[2] if len(sys.argv) > 2 else 'associata@Beast'
    vis_node_name = sys.argv[3] if len(sys.argv) > 3 else 'aas_vis@Beast'
    cookie = sys.argv[4] if len(sys.argv) > 4 else 'aas_cookie'

    ctrl_listener = ctrl.Ctrl(out_path, client_node_name, vis_node_name, cookie)

    print('ctrl started')

    asyncio.run(ctrl_listener.wait_until_finished())

    print('Asvis finished')


if __name__ == "__main__":
    main()

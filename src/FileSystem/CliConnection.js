import * as ClientSocket from "../ClientSocket.mjs";
import { S } from "../fp.mjs";
import * as FileSystem from "../FileSystem.mjs";
import { id } from "../help.mjs";

// -- CUSTOM ELEMENT FOR MANAGING THE CONNECTION FROM WITHIN ELM
export const use = app => {
    const onConnect = app.ports.cli_connected.send;
    const onConnectError = app.ports.cli_connect_error_occurred.send;
    const connect = () => {
        const socket = ClientSocket.get();
        if (S.isNothing(socket)) {
            const fsHooks = {
                onSpecWrite: app.ports.fs_specWritten.send,
                onChange: () => null // onChange(app)
            };

            const s = ClientSocket.init(fsHooks);
            s.errorCount = 0;
            // connect the handlers
            s.on("connect", () => {
                onConnect(null);
            });
            s.on("connect_error", e => {
                onConnectError(null);
            });
        } else {
            socket.value.connect();
        }
    };

    const close = () => ClientSocket.close();

    app.ports.cli_connection_requested.subscribe(connect);
    app.ports.cli_connection_termination_requested.subscribe(close);

    // try connecting to the cli and also refresh the file list
    // TODO: find better interaction for clientsocket
    // connect();
    // onChange(app);
};
const onChange = async app => {
    const files = await FileSystem.listFiles();
    const imports = files.map(path => tryImport(path));
    const withModules = await Promise.all(imports);
    app.ports.fs_changed.send(withModules);
};

const tryImport = path =>
    new Promise((resolve, reject) => {
        if (path.startsWith("/custom-elements/")) {
            const randomSuffix = `?op-${id()}`;
            const pathToImport = `/root>${path}${randomSuffix}`;
            import(
                pathToImport
                /* webpackIgnore: true */
            )
                .then(module => resolve([path, module]))
                .catch(problem => resolve([path, null]));
        } else {
            resolve([path, null]);
        }
    });

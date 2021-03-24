import io from "socket.io-client";
import { S, F } from "./fp.mjs";
import * as Operation from "./Operation.mjs";

var socket = null;

export const get = () => (socket ? S.Just(socket) : S.Nothing);

const socketSettings = {
    reconnectionDelay: 2000,
    reconnectionDelayMax: 10000,
    reconnectionAttempts: 4
};
const handleMsg = socket => writeHandlers => async ({ payload, id }) => {
    try {
        if (payload.type == "WriteOperation") {
            await F.promise(
                Operation.executeWrite(process.cwd())(payload.data)
            );
            if (payload.data.path === "root>funk.json") {
                const string = Operation.arrayBufferToString(
                    payload.data.contents
                );
                writeHandlers.onSpecWrite(string);
            }
            writeHandlers.onChange();
            // transactionLog = addToTransactionLog(payload.data)(transactionLog)
            // socket.emit("msgok", { id });
        } else {
            throw `msg type ${payload.type} not supported`;
        }
    } catch (err) {
        console.error(err);
        socket.emit("msgerr", { id, err: err.toString() });
    }
};

// -- START THE SOCKET AND ADD HANDLERS
export const init = writeHandlers => {
    socket = io(":1960", socketSettings);

    socket.on("msg", handleMsg(socket)(writeHandlers));

    socket.on("disconnect", () => {});

    return socket;
};

export const close = () => {
    if (S.isJust(get())) {
        socket.close();
        socket = null;
    }
};

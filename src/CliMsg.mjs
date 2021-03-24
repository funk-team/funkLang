import { def, $, S, Future, F } from "./fp.mjs";
import * as Operation from "./Operation.mjs";
import { id } from "./help.mjs";
export const WriteOperationSymbol = "WriteOperation";
export const WriteOperationSymbolType = $.NullaryType("WriteOperation")(
    "WriteOperation"
)([])(data => data === "WriteOperation");

export const IdType = $.NullaryType("IdType")("IdType")([])(
    data => typeof data === "string" && data.length >= 9
);

export const PayloadType = $.RecordType({
    type: WriteOperationSymbolType,
    data: Operation.WriteOperationType
});

export const MsgType = $.RecordType({
    id: IdType,
    payload: PayloadType
});

export const SocketType = $.NullaryType("SocketType")("SocketType")([])(
    socket => !!socket.emit
);

export const fromOperation = def("fromOperation")({})([
    Operation.WriteOperationType,
    MsgType
])(fileOperation => {
    const id_ = id();
    const msg = {
        id: id_,
        payload: { type: WriteOperationSymbol, data: fileOperation }
    };
    return msg;
});

var pendingOperations = {};

export const emit = def("emit")({})([
    MsgType,
    $.Maybe(SocketType),
    Future($.Error)(MsgType)
])(msg => socket =>
    F.Future((reject, resolve) => {
        if (S.isJust(socket)) {
            const s = S.maybeToNullable(socket);
            s.emit("msg", msg);
            resolve(msg);
            return () => "";
        }
        resolve(msg);
        return () => "";
    })
);

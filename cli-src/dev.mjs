/**
 * The server for funk.
 * It connects to the browser and makes sure local and remote file systems are in sync.
 */

import io from "socket.io";
import fs from "fs";
import p from "path";
import chokidar from "chokidar";
import register from "@babel/register";
import "@babel/polyfill";
import hasha from "hasha";
import ignore from "ignore";
import gitRootDir from "git-root-dir";
import { F, def, $ } from "../src/fp.mjs";
import * as Operations from "../src/Operation.mjs";
import * as CliMsg from "../src/CliMsg.mjs";
// shim
global.fs = fs;

const workingDir = process.cwd();

// keep the history of filesystem transactions
// when the watcher triggers an update because a file was streamed from the client
// we do not want to stream it back
var transactionLog = {};

// CONFIGURE GIT
const defaultIgnore = [".DS_Store", "node_modules", ".git", "elm-stuff"];

const ig = ignore().add(defaultIgnore);

/**
 * initialize the websocket server
 */
const server = io({
    serveClient: false
});

/**
 * The files that are ignored by git do not need to be synced.
 */
const ignoreFilesIgnoredByGit = ignores => path => {
    const relative = p.relative(workingDir, path);
    if (relative) {
        return ig.ignores(relative);
    } else {
        true;
    }
};

/**
 * Configure ignores, the directory to watch etc.
 */
const makeChokidarOptions = async () => {
    const gitDir = await gitRootDir(workingDir);
    var ignores = defaultIgnore;
    if (gitDir) {
        try {
            const file = (
                await fs.promises.readFile(p.join(gitDir, ".gitignore"))
            ).toString();
            const additionalIgnores = file
                .split("\n")
                .map(s => s.trim())
                .filter(s => !(s.startsWith("#") || s.length === 0));
            ignores = [...ignores, ...additionalIgnores];
        } catch {}
    }
    return {
        ignoreInitial: true,
        ignored: [ignoreFilesIgnoredByGit(ignores)]
    };
};

export const addToTransactionLog = def("addToTransactionLog")({})([
    Operations.WriteOperationType,
    $.Any,
    $.Any
])(operation => transactionLog => {
    transactionLog[operation.path] = hasha(operation.contents);
    return transactionLog;
});
export const removeFromTransactionLog = def("removeFromTransactionLog")({})([
    Operations.WriteOperationType,
    $.Any,
    $.Any
])(operation => transactionLog => {
    delete transactionLog[operation.path];
    return transactionLog;
});
export const isInTransactionLog = def("isInTransactionLog")({})([
    Operations.WriteOperationType,
    $.Any,
    $.Any
])(operation => transactionLog => {
    const isThere = transactionLog[operation.path] == hasha(operation.contents);
    return isThere;
});

const handleMsg = socket => async ({ payload, id }) => {
    try {
        console.log("\n[msg]", id);
        if (payload.type == "WriteOperation") {
            await F.promise(
                Operations.executeWrite(process.cwd())(payload.data)
            );
            transactionLog = addToTransactionLog(payload.data)(transactionLog);
            socket.emit("msgok", { id });
        } else {
            throw `msg type ${payload.type} not supported`;
        }
    } catch (err) {
        console.log(err);
        socket.emit("msgerr", { id, err: err.toString() });
    }
};
/**
 * What to do when the UI connects to the server
 */
server.on("connection", async socket => {
    console.log("connected", socket.id);
    // handle incoming messages
    socket.on("msg", handleMsg(socket));
    socket.once("disconnect", async () => {
        await watcher.close();
        console.log("closed and stopped watcher", socket.id);
    });
    // set up watcher
    const chokidarOptions = await makeChokidarOptions();
    const watcher = chokidar.watch(workingDir, chokidarOptions);
    // stream filesystem updates
    watcher.on("add", handleAdd(socket));
    watcher.on("change", handleChange(socket));
});

// WATCH HANDLERS
export const handleAdd = socket => async path => {
    const operation = await F.promise(
        Operations.makeWriteOperationFromLocalFileAddition(workingDir)(path)
    );
    if (isInTransactionLog(operation)(transactionLog)) {
        transactionLog = removeFromTransactionLog(operation)(transactionLog);
        console.log("[written]", operation.path);
    } else {
        socket.emit("msg", CliMsg.fromOperation(operation));
        console.log("[sent]", operation.path);
    }
};

const handleChange = handleAdd;

/**
 * Make sure that the folders that need to be written to are there
 */
const ensurePath = async path => {
    const dir = p.join(workingDir, p.dirname(path));
    console.log(dir, "ensured");
    return fs.promises.mkdir(dir, { recursive: true });
};

export const run = () => {
    const port = 1960;
    console.info(`Starting funk development server on port ${port}`);
    server.listen(port);
};

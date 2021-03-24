/**
import { test } from "zora";
import { def, log, F, S } from "./src/fp.mjs";
import fs from "fs";
import FileApi from "file-api";
import FileReader from "filereader";
import hasha from "hasha";
import slugify from "slugify";
import * as Path from "path";
import Process from "process";
import "@babel/register";

// shims
global.fs = fs;
global.FileReader = FileReader;
global.File = FileApi.File;
global.projectRoot = process.cwd();
global.CustomEvent = class CustomEvent {};

// modules to test
import * as Operation from "./src/Operation.mjs";
import * as DevServer from "./cli-src/dev.mjs";
import * as CliMsg from "./src/CliMsg.mjs";
import * as MediaSelector from "./src/customElements/MediaSelector/Handlers.mjs";
import "./tests/compile.mjs";

// environment
const isNode = typeof processs === "object";

// test data
const exampleBuffer = () => fs.promises.readFile(exampleFileLocation);
const exampleFileName = "batman.6f5902ac237024bdd0c176cb93063dc4.txt";
const exampleFileLocation = Path.resolve(
    process.cwd(),
    "assets",
    exampleFileName
);
const exampleFile = async () =>
    new File({
        name: "batman.txt", // required
        type: "image/jpg", // optional
        buffer: await exampleBuffer()
    });
const sampleOperation = async () => ({
    path: Path.join("root>assets/", exampleFileName),
    contents: await exampleBuffer()
});
class ExampleConnection {
    constructor() {
        this.isConnected = true;
        this.calls = [];
    }
    emit(msg) {
        this.calls = this.calls.concat([msg]);
    }
    connect() {
        this.isConnected = true;
    }
    disconnect() {
        this.isConnected = false;
    }
}
class ExampleHtmlElement {
    constructor() {
        this.isConnected = true;
        this.calls = [];
        this.projectMeta = {projectId : "tests"}
    }
    dispatchEvent(msg) {
        this.calls = this.calls.concat([msg]);
    }
}
// -- TESTS --

test("read file", async t => {
    const virtualBrowserFile = await exampleFile();
    const contents = await exampleBuffer();
    return new Promise((resolve, reject) => {
        F.fork(reject)(buff => {
            resolve(t.equals(buff, contents));
        })(Operation.readFileWithFileReader(virtualBrowserFile));
    });
});

test("generate write operation for file upload", async t => {
    const virtualBrowserFile = await exampleFile();
    const expectedOutput = await sampleOperation();
    return new Promise((resolve, reject) => {
        F.fork(reject)(operation => {
            resolve(t.equals(operation, expectedOutput));
        })(Operation.makeWriteOperationFromUiFileUpload(virtualBrowserFile));
    });
});

test("generate write operation for local file addition", async t => {
    const expectedOutput = await sampleOperation();
    return new Promise((resolve, reject) => {
        F.fork(reject)(operation => {
            resolve(t.equals(operation, expectedOutput));
        })(
            Operation.makeWriteOperationFromLocalFileAddition(process.cwd())(
                exampleFileLocation
            )
        );
    });
});

test("read and write same file", async t => {
    const contents = await exampleBuffer();
    return new Promise((resolve, reject) => {
        const cwd = Process.cwd();
        const filePath = "root>assets/batman_temp.txt";
        const writeOp = Operation.executeWrite(cwd)({
            path: filePath,
            contents
        });
        const readOp = path => Operation.executeRead(cwd)({ path });
        const readWrite = F.chain(readOp)(writeOp);
        const e2e = readWrite;
        F.fork(someError => reject(someError))(readContents => {
            t.equals(readContents, contents);
            resolve();
        })(e2e);
    });
});

test("transactionLog", async t => {
    const operation = await sampleOperation();
    // addition
    var log = DevServer.addToTransactionLog(operation)({});
    // check
    t.equals(true, DevServer.isInTransactionLog(operation)(log));
    t.equals(
        {
            "root>assets/batman.6f5902ac237024bdd0c176cb93063dc4.txt":
                "db3974a97f2407b7cae1ae637c0030687a11913274d578492558e39c16c017de84eacdc8c62fe34ee4e12b4b1428817f09b6a2760c3f8a664ceae94d2434a593"
        },
        log
    );
    // removal
    log = DevServer.removeFromTransactionLog(operation)(log);
    t.equals({}, log);
});

test("CliMsg fromOperation", async t => {
    const operation = await sampleOperation();
    const expected = {
        id: "[stub]",
        payload: { type: "WriteOperation", data: operation }
    };
    const msg = CliMsg.fromOperation(operation);
    t.equals(msg.id.length, 10);
    msg.id = expected.id;
    t.equals(expected, msg);
});

test("do not emit if disconnected", async t => {
    const operation = await sampleOperation();
    const msg = CliMsg.fromOperation(operation);
    const exampleConnection = S.Nothing;
    CliMsg.emit(msg)(exampleConnection);
    t.ok("not crashing");
});
test("emit if connected", async t => {
    const operation = await sampleOperation();
    const exampleConnection = new ExampleConnection();
    const msg = CliMsg.fromOperation(operation);
    t.equal(exampleConnection.calls.length, 0);
    const emission = CliMsg.emit(msg)(S.Just(exampleConnection));
    await F.promise(emission);
    t.equal(exampleConnection.calls.length, 1);
});
test("emit change", async t => {
    const exampleConnection = new ExampleConnection();
    await DevServer.handleAdd(exampleConnection)(exampleFileLocation);
    t.equal(exampleConnection.calls.length, 1);
});
test("image upload", async t => {
    const el = new ExampleHtmlElement();
    const files = [await exampleFile()];
    const event = { target: { files } };
    await MediaSelector.handleChange(el)(event);
    t.equal(el.calls.length, 1);
});

test("localize", t => {
    // example for local file system
    t.equal(
        Operation.localize("/home/linus/funk")("root>/a"),
        "/home/linus/funk/a"
    );
    t.equal(
        Operation.localize("/home/linus/funk")("root>/a/_.jpg"),
        "/home/linus/funk/a/_.jpg"
    );

    // example for in the browser
    t.equal(Operation.localize("/")("root>/a"), "/a");
    t.equal(Operation.localize("/")("root>/a/_.jpg"), "/a/_.jpg");
});

test("ensure dir", async t => {
    const operation = await sampleOperation();
    operation.path = operation.path.replace("assets", "assets/temp");
    const writeOp = await F.promise(
        Operation.executeWrite(projectRoot)(operation)
    );
});

*/

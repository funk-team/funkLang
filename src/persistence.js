import * as ClientSocket from "./ClientSocket.mjs";
export const funkFilePath = "/funk.json";
import * as Operation from "./Operation.mjs";
import { S, $, F } from "./fp.mjs";
import * as CliMsg from "./CliMsg.mjs";
import * as FileSystem from "./FileSystem.mjs";
const funkSpecEntryKey = "funkspec";

const enc = new TextDecoder("utf-8");
export const getProjectRootDirectory = (projectMeta) =>
  `/${projectMeta.projectId}`;

export const decodeBuffer = (buf) => enc.decode(buf);

const normalizedFunkFilePath = (projectMeta) => `root>funk.json`;

export const use = (addons) => (app) => {
  console.log(addons);
  // when saving, save to local FS and try to stream through to the CLI
  const save = ([projectMeta, jsonString]) => {
    const projectRoot = getProjectRootDirectory(projectMeta);

    const operation = {
      contents: jsonString,
      path: normalizedFunkFilePath(projectMeta),
    };
    const localWrite = Operation.executeWrite(projectRoot)(operation);
    const executions = F.chain(() =>
      CliMsg.emit(CliMsg.fromOperation(operation))(ClientSocket.get())
    )(localWrite);
    F.fork((err) => console.error("write failed", err))(async (success) => {
      try {
        await addons.syncToRemote(projectRoot);
      } catch (e) {
        // debounce of earlier things canceled
      }
      app.ports.repoPushed.send(projectRoot);
      // notify other windows
      localStorage.update = JSON.stringify({
        projectMeta,
        timestamp: Date.now(),
      });
    })(executions);
  };
  const simpleCheckout = (app) => async (projectMeta) => {
    const projectRoot = getProjectRootDirectory(projectMeta);
    try {
      await fs.promises.mkdir(projectRoot);
    } catch (e) {}
    app.ports.repoInitialized.send(await readSpec(projectRoot));
  };

  app.ports.save.subscribe(save);
  app.ports.checkout.subscribe(
    addons ? addons.checkout(app) : simpleCheckout(app)
  );

  // TODO: connect to preview path
  window.addEventListener("storage", (ev) => {
    const { projectMeta } = JSON.parse(localStorage.update);
    // if the project meta could not be parsed, do not trigger an update
    // TODO: investigate if updates are actually the latest one
    if (!projectMeta) {
      return;
    }
    if (app.ports.previewSpecUpdated) {
      F.fork((err) =>
        console.error("could not hot reload spec", err)
      )((buffer) =>
        app.ports.previewSpecUpdated.send([projectMeta, decodeBuffer(buffer)])
      )(
        Operation.executeRead(getProjectRootDirectory(projectMeta))({
          path: normalizedFunkFilePath(projectMeta),
        })
      );
    }
  });
};

const propagateToPreview = (spec) => {
  localStorage.spec = spec;
};

export const readSpec = async (repoPath) => {
  const funkFilePath = repoPath + "/funk.json";
  try {
    const fileContents = decodeBuffer(await fs.promises.readFile(funkFilePath));
    return fileContents;
  } catch (e) {
    return null;
  }
};

/**
 * A declarative file system.
 * Works like VDOM

import { def, log, F, S, $, Future } from "../src/fp.mjs";
import * as Operation from "../src/Operation.mjs";
import { test } from "zora";
import * as FileSystem from "../src/FileSystem.mjs";
import hasha from "hasha";

const packageName = "";
const packageURL = "";

export const LazyType = (name, type) =>
    $.NullaryType(`${packageName}/${name}`)(`${packageURL}#${name}`)([])(x =>
        $.test($.env, type(), x)
    );

export const LazyDirectorySpecType = LazyType(
    "DirectorySpecType",
    () => DirectorySpecType
);

export const FileSpecType = $.RecordType({
    name: $.String,
    value: Operation.FileContentsType
});
export const DiffSummaryType = $.RecordType({
    missingInReplica: $.Array($.String),
    missingInPrimary: $.Array($.String),
    differingSharedFiles: $.Array($.String),
    identicalSharedFiles: $.Array($.String)
});

export const DirectoryOrFile = $.Either(LazyDirectorySpecType)(FileSpecType);

export const DirectorySpecType = $.Array1(DirectoryOrFile);

const ComparisonResultType = $.Either(DiffSummaryType)($.Null);

const compare = def("compare")({})([
    $.String,
    $.String,
    Future($.Error)(ComparisonResultType)
])(pathPrimary =>
    F.encaseP(async pathReplica => {
        // read all files in both folders
        const filesPrimary = (
            await FileSystem.listFiles(pathPrimary)
        ).map(path => path.replace(pathPrimary, ""));
        const filesReplica = (
            await FileSystem.listFiles(pathReplica)
        ).map(path => path.replace(pathReplica, ""));

        const checkPath = async pathOfFileInPrimary => {
            if (filesReplica.indexOf(pathOfFileInPrimary) > -1) {
                const hashInPrimary = await hasha.fromFile(
                    pathPrimary + pathOfFileInPrimary
                );
                const hashInSecondary = await hasha.fromFile(
                    pathReplica + pathOfFileInPrimary
                );
                return S.Just(
                    S.Pair(pathOfFileInPrimary)(
                        hashInPrimary == hashInSecondary
                    )
                );
            } else {
                return S.Nothing;
            }
        };
        const checkProcesses = filesPrimary.map(checkPath);
        const shared = S.justs(await Promise.all(checkProcesses));
        const missingInReplica = filesPrimary.filter(
            path => !(filesReplica.indexOf(path) > -1)
        );
        const missingInPrimary = filesReplica.filter(
            path => !(filesPrimary.indexOf(path) > -1)
        );
        // console.log("filesPrimary", filesPrimary);
        // console.log("filesReplica", filesReplica);
        // console.log("shared", shared);
        // console.log("missingInPrimary", missingInPrimary);
        // console.log("missingInReplic", missingInReplica);
        const differingSharedFiles = S.map(S.fst)(S.reject(S.snd)(shared));
        const identicalSharedFiles = S.map(S.fst)(S.filter(S.snd)(shared));
        const isInSync =
            missingInPrimary.length === 0 &&
            missingInReplica.length === 0 &&
            differingSharedFiles.length === 0;
        if (isInSync) {
            return S.Right(null);
        } else {
            const diffSummary = {
                missingInPrimary,
                missingInReplica,
                differingSharedFiles,
                identicalSharedFiles
            };
            return S.Left(diffSummary);
        }
        // normalize file paths and match them
        // aggregate
        // rebase file paths and their hashes
    })
);

test("compare matching directories", async t => {
    const compareOperation = compare("./tests/fixtures/matching-directories/a")(
        "./tests/fixtures/matching-directories/b"
    );
    const result = await F.promise(compareOperation);
    t.ok(S.isRight(result));
});

test("compare matching directories", async t => {
    const expected = {
        differingSharedFiles: ["/samename"],
        identicalSharedFiles: ["/justfine"],
        missingInPrimary: ["/bar", "/baz"],
        missingInReplica: ["/foo"]
    };
    const compareOperation = compare(
        "./tests/fixtures/different-directories/a"
    )("./tests/fixtures/different-directories/b");
    const result = await F.promise(compareOperation);
    console.dir(result);
    if (S.isLeft(result)) {
        // console.log(result.value);
        t.equals(result.value, expected);
    } else {
        t.ok(false);
    }
});
 */

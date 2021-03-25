// Define file system operations to send over the wire
import { def, $, S, F, Future, log } from './fp.mjs'

// helpers for modifying and generating paths
import * as Path from 'path'
import hasha from 'hasha'
import slugify from 'slugify'

// -- TYPES --

// A local path starts with '/'
const LocalPathType = $.NullaryType('LocalPathType')('LocalPathType')([])(
    data => {
        return typeof data === 'string' && data.startsWith('/')
    }
)

// this one starts with root>
const AgnosticFilePathType = $.NullaryType('AgnosticFilePathType')(
    'AgnosticFilePathType'
)([])(data => {
    return typeof data === 'string' && data.startsWith('root>')
})

// buffer | string
export const FileContentsType = $.NullaryType('FileContentsType')(
    'FileContentsType'
)([])(data => {
    return (
        typeof data === 'string' ||
        data instanceof Buffer ||
        data instanceof ArrayBuffer
    )
})

// Is it a file as in the browser API
const FileType = $.NullaryType('FileType')('FileType')([])(data => {
    return data instanceof File
})

// Is it a file as in the browser API
const ArrayBufferType = $.NullaryType('ArrayBufferType')('ArrayBufferType')([])(
    data => {
        return data instanceof ArrayBuffer
    }
)

// the write data to send over the wire
export const WriteOperationType = $.RecordType({
    path: AgnosticFilePathType,
    contents: FileContentsType,
})

// the read location to send over the wire
// QUESTION: does this even have a use case
const ReadOperationType = $.RecordType({
    path: AgnosticFilePathType,
})

// -- IMPLEMENTATION --

// API
const executeWrite_ = cwd =>
    F.encaseP(async ({ path, contents }) => {
        const localPath = localize(cwd)(path)
        await F.promise(ensureDir(localPath))
        return await fs.promises
            .writeFile(localPath, contents)
            .then(() => console.log('written', path))
            .catch(() => console.log('failed', path))
    })

export const executeWrite = def('executeWrite')({})([
    LocalPathType,
    WriteOperationType,
    Future($.Error)(AgnosticFilePathType),
])(executeWrite_)

const executeRead_ = cwd => ({ path }) => readFileWithFs(localize(cwd)(path))

const readFileWithFs = def('readFileWithFs')({})([
    LocalPathType,
    Future($.Any)($.Buffer),
])(F.encaseP(path => fs.promises.readFile(path)))

export const executeRead = def('executeRead')({})([
    LocalPathType,
    ReadOperationType,
    Future($.Error)($.Buffer),
])(executeRead_)

const toString = def('toString')({})([$.Buffer, $.String])(buffer =>
    buffer.toString()
)

// An uploaded file gets hashed to prevent collisions.
const makeWriteOperationFromUiFileUpload_ = file => {
    const combine = ([fileHash, fileBuffer]) => {
        const parts = slugify(file.name, {
            lower: true,
            replacement: '-',
        }).split('.')
        const ext = parts[parts.length - 1]
        const base = 'root>assets'
        const fileName = [...[parts.slice(0, -1)], fileHash, ext].join('.')
        return { path: [base, fileName].join('/'), contents: fileBuffer }
    }

    const addHash = buffer => F.both(hash(buffer))(F.resolve(buffer))
    return F.map(combine)(F.chain(addHash)(readFileWithFileReader(file)))
}

export const makeWriteOperationFromUiFileUpload = def(
    'makeWriteOperationFromUiFileUpload'
)({})([FileType, Future($.Error)(WriteOperationType)])(
    makeWriteOperationFromUiFileUpload_
)

// We need to cut off the local path to make things agnostic
const makeWriteOperationFromLocalFileAddition_ = cwd => path => {
    const readOp = readFileWithFs(path)
    const agnosticPath = 'root>' + path.slice(cwd.length + 1)
    const bake = buf => ({ path: agnosticPath, contents: buf })
    return F.map(bake)(readOp)
}

export const makeWriteOperationFromLocalFileAddition = def(
    'makeWriteOperationFromLocalFileAddition'
)({})([LocalPathType, LocalPathType, Future($.Error)(WriteOperationType)])(
    makeWriteOperationFromLocalFileAddition_
)

// For the browser
export const readFileWithFileReader = def('readFileWithFileReader')({})([
    FileType,
    Future($.Error)($.Buffer),
])(
    F.encaseP(
        file =>
            new Promise((resolve, reject) => {
                var reader = new FileReader()
                reader.addEventListener('loadend', function () {
                    resolve(reader.result)
                })
                reader.readAsArrayBuffer(file)
                return function () {
                    reader.abort()
                }
            })
    )
)

export const arrayBufferToString = def('arrayBufferToString')({})([
    ArrayBufferType,
    $.String,
])(buf => {
    const enc = new TextDecoder()
    return enc.decode(buf)
})

// helpers

const hash = def('hash')({})([FileContentsType, Future($.Void)($.String)])(
    F.encaseP(async data => {
        try {
            return await hasha.async(data, { algorithm: 'md5' })
        } catch {
            return await hasha.async(data.toString(), { algorithm: 'md5' })
        }
    })
)
// window.hash = x => F.promise(hash(x))

export const localize = def('localize')({})([
    LocalPathType,
    AgnosticFilePathType,
    LocalPathType,
])(cwd => path => {
    const withoutPrefix = path.slice(5)
    return Path.join(cwd, withoutPrefix)
})

const ensureDir = def('ensureDir')({})([
    LocalPathType,
    Future($.Error)($.Null),
])(
    F.encaseP(async path => {
        const dir_ = path.split('/').slice(0, -1).join('/')
        const dir = dir_ === '' ? '/' : dir_
        try {
            await fs.promises.mkdir(dir)
            return
        } catch (e) {
            if (e.code == 'EEXIST') {
                return
            } else {
                throw e
            }
        }
    })
)

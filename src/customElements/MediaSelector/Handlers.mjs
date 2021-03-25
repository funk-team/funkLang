import { F, S } from '../../fp.mjs'
import * as CliMsg from '../../CliMsg.mjs'
import * as ClientSocket from '../../ClientSocket.mjs'
import * as Operation from '../../Operation.mjs'
import slugify from 'slugify'
import * as Persistence from '../../persistence.js'

// when the user clicks to select something
export const handleChange = context => async event => {
    const projectRoot = Persistence.getProjectRootDirectory(context.projectMeta)
    // extract file contents
    const detail = (await handleFiles(projectRoot)(event.target.files))[0]
    // notify app
    context.dispatchEvent(new CustomEvent('file-saved', { detail }))
}

// when the user drops a file
export const handleDrop = context => async ev => {
    const detail = await processEvent(context, ev)
    context.dispatchEvent(new CustomEvent('file-saved', { detail }))
    context.classList.remove('dragging-over')
    return
}

const processEvent = async (context, ev) => {
    const projectRoot = Persistence.getProjectRootDirectory(context.projectMeta)
    const textData = ev.dataTransfer.getData('Text')
    const url = ev.dataTransfer.getData('URL')
    if (event.dataTransfer.files.length > 0) {
        return (await handleFiles(projectRoot)(event.dataTransfer.files))[0]
    } else if (url.length > 0) {
        const response = await fetch(url)
        const type = response.headers.get('content-type')
        const blob = await response.blob()
        const file = new File([blob], slugify(url), { type })
        return await processFile(projectRoot)(file)
    } else {
        return null
    }
}

// write a file to the local file system and also mirror it to connected filesystems
const processFile = projectRoot => async file => {
    const mime = file.type
    const writeOperation = await F.promise(
        Operation.makeWriteOperationFromUiFileUpload(file)
    )
    await F.promise(Operation.executeWrite(projectRoot)(writeOperation))
    await F.promise(
        CliMsg.emit(CliMsg.fromOperation(writeOperation))(ClientSocket.get())
    )
    const detail = { name: file.name, path: writeOperation.path, mime }
    return detail
}

// small helper for data plumbing
const handleFiles = projectRoot => async filesList => {
    const filePostProcessing = Array.from(filesList).map(
        processFile(projectRoot)
    )
    return Promise.all(filePostProcessing)
}

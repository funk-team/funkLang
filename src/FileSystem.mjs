import { S, $, F } from './fp.mjs'

export const listFiles = async path_ => {
    const path = path_ || '/'
    const recurse = async entity => {
        // try recursing into the directory
        try {
            const entities = await fs.promises.readdir(entity)
            const results = await Promise.all(
                entities.map(entity_ => recurse(entity + '/' + entity_))
            )
            return S.reduce(S.concat)([])(results)
        } catch (e) {
            return [entity]
        }
    }
    const entities = await fs.promises.readdir(path)
    const all = await Promise.all(
        entities.map(entity =>
            recurse((path.endsWith('/') ? path : path + '/') + entity)
        )
    )
    const flat = S.reduce(S.concat)([])(all)
    return flat
}
export const wipe = async () => (await listFiles()).map(fs.promises.unlink)

if (typeof window === 'object') {
    window.listFiles = listFiles
    window.wipe = wipe
}

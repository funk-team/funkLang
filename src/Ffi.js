const AsyncFunction = eval(
    'Object.getPrototypeOf(async function(){}).constructor'
)

/**
 * http://disq.us/p/24waw5t
 */
const esm = raw =>
    URL.createObjectURL(new Blob([raw], { type: 'text/javascript' }))

/**
 * Execute a code snipped inside an async function
 */
const runCode = cb => async ({ sourceId, code, values }) => {
    const timestamp = new Date().getTime()

    const makeReturn = return_ => ({ timestamp, sourceId, return_ })
    try {
        // make the blob
        const blob = esm(code)

        // import the custom module
        const customModule = await eval(`import(blob)`)

        // get the return value
        const return_value = await customModule.default(...values)

        // check the return value
        // return the return value back to funk
        if (return_value && return_value.hasOwnProperty) {
            const ok = return_value
            cb(makeReturn({ ok }))
        } else {
            const ok = null
            cb(makeReturn({ ok }))
        }
    } catch (e) {
        console.error(e)
        const err = e.toString()
        const returnValue = makeReturn({ err })
        console.log(returnValue)
        cb(returnValue)
    }
}

export const use = ({ ports }) => {
    ports.runCode.subscribe(runCode(ports.gotCodeResult.send))
}

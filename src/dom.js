/**
 * a wrapper around the dom API for sync access in elm
 */
const getElementById = new Proxy(
    {},
    {
        has(_, id) {
            return true
        },
        get(_, id) {
            return document.getElementById(id)
        },
    }
)

const dom = { getElementById }

export default dom

window.dom = dom

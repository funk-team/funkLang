const CLIPBOARD_KEY = 'funk_clipboard'
export const use = app => {
    window.addEventListener('storage', event => {
        if (event.key === CLIPBOARD_KEY) {
            app.ports.clipboardChanged.send(JSON.parse(event.newValue))
        }
    })

    app.ports.storeClipboard.subscribe(data => {
        localStorage[CLIPBOARD_KEY] = JSON.stringify(data)
    })
}

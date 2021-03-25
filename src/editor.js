import { $, def } from './fp.mjs'

import Main from './Main.elm'
import './main.css'
import './ui.css'
import * as LightningFS from '@isomorphic-git/lightning-fs/dist/lightning-fs.min.js'
import * as CliConnection from './FileSystem/CliConnection'
import * as Ffi from './Ffi.js'
import * as Clipboard from './Clipboard.js'

// currently, the SW is disabled and we bust the cache
import { registerPseudoServer } from './registerServiceWorker'

// registerServiceWorker();
import './cachebuster.js'

import * as Persistence from './persistence.js'
import * as DomMonkeyPatches from './domMonkeyPatches.js'
import './customElements'
import dom from './dom'
import CodeEditor from './CodeEditor.js'

// INIT

// generate a random seed for the elm app
const seed = crypto.getRandomValues(new Uint32Array(1))[0]

/**
 * Funk requires the pseudo server to be installed in order to serve files from the in-browser filesystem
 */
export const installAndInit = async enterpriseAdditions => {
    // we wait for the service worker registration to complete
    // https://developer.mozilla.org/en-US/docs/Web/API/ServiceWorkerContainer/controller
    registerPseudoServer(async () => {
        // because the service worker only is available after a refresh sometimes we need to force it.
        if (navigator.serviceWorker.controller) {
            window.fs = new LightningFS('funk-lightning-fs', { wipe: false })
            initMain(enterpriseAdditions)
        } else {
            location.reload()
        }
    })
}

async function initMain(enterpriseAdditions) {
    const flags = {
        storage: localStorage,
        seed,
        dom,
        now: Date.now(),
        mode: enterpriseAdditions ? 'enterprise' : 'core',
    }

    const app = Main.Elm.Main.init({
        node: document.getElementById('root'),
        flags,
    })

    DomMonkeyPatches.apply(app)
    // init CodeMirror
    CodeEditor.initialize()
    console.log(enterpriseAdditions)

    // Log the session if this is not on localhost
    Persistence.use(
        enterpriseAdditions && enterpriseAdditions.persistenceAddons
    )(app)
    CliConnection.use(app)
    Ffi.use(app)
    Clipboard.use(app)
    // enterprise hook 1 - pre-mount
    enterpriseAdditions && (await enterpriseAdditions.use(app))
}

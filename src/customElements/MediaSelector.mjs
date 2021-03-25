import * as ClientSocket from '../ClientSocket.mjs'
import * as Operation from '../Operation.mjs'
import { F, S } from '../fp.mjs'
import * as CliMsg from '../CliMsg.mjs'
import './MediaSelector.css'
import * as Handlers from './MediaSelector/Handlers.mjs'

/**
 * MediaPicker module
 * @module customElements/MediaPicker
 * @description
 * Wraps a label. When clicked opens a popup to select a single file.
 * This file is then be saved to the the filesystem abstraction.
 * When the file is saved, the `imagewrite` event is emitted with name and path to the file.
 * @example <funk-media-picker></funk-media-picker>
 */
const MediaPicker = () =>
    class MediaPicker extends HTMLElement {
        connectedCallback() {
            const node = document.createElement('input')
            node.type = 'file'
            node.addEventListener('change', Handlers.handleChange(this))
            this.addEventListener('click', () =>
                node.dispatchEvent(new MouseEvent('click'))
            )
            this.addEventListener('dragenter', ev => {
                this.classList.add('dragging-over')
            })
            this.addEventListener('drop', Handlers.handleDrop(this))
            this.addEventListener('dragleave', ev => {
                this.classList.remove('dragging-over')
            })
        }
    }

if (typeof customElements !== 'undefined') {
    customElements.define('funk-media-picker', MediaPicker())
}

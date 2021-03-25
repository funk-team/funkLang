/**
 * ContentEditable module
 * @module customElements/ContentEditable
 * @description
 * Takes a text attribute and renders the string as content.
 * Prevents resetting of cursor when new data is fed into the contenteditable.
 * Changes can be listened to on the 'input' event.
 * @example
 * <funk-contenteditable text="Lorem ipsum"><span contenteditable="true"></span><span style="opacity: 0.5;">Lorem ipsum</span></funk-contenteditable>
 */

import './ContentEditable.css'

class ContentEditable extends HTMLElement {
    static get observedAttributes() {
        return ['text']
    }

    connectedCallback() {
        const text = this.getAttribute('text')
        if (text) {
            this.childrenAsArray[0].innerText = text
        }
        this.childrenAsArray[1].addEventListener('click', () => {
            this.childrenAsArray[0].focus()
        })

        // prevent styling
        this.childrenAsArray[0].addEventListener('keydown', ev => {
            if (ev.metaKey) {
                const stylingHotkeys = ['i', 'b', 'u']
                if (stylingHotkeys.indexOf(ev.key) > -1) {
                    ev.preventDefault()
                }
            }
        })
    }

    attributeChangedCallback(name, oldValue, newValue) {
        // do not update if the contenteditable is currently focused and used to author text
        if (document.activeElement == this.childrenAsArray[0]) {
            return
        } else if (this.childrenAsArray[0]) {
            this.childrenAsArray[0].innerText = newValue
        }
    }
}
customElements.define('funk-contenteditable', ContentEditable)

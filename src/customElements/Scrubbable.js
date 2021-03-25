/**
 * Scrubbable module
 * @module customElements/Scrubbable
 * @description
 * Wraps a label.
 * On mouse down, capture the mouse input and enter a drag-like interaction that can go beyond screen borders.
 * Helpful for adjusting numeric values.
 * @example
 * <funk-scrubbable>Border radius: 10px</funk-scrubbable>
 */

class Scrubbable extends HTMLElement {
    static get observedAttributes() {
        return ['value']
    }

    // set up canvas and indicator as well as events
    connectedCallback() {
        this.canvas = document.createElement('canvas')
        this.canvas.height = 0
        this.canvas.width = 0
        this.addEventListener('mousedown', this.startScrub)
        this.addEventListener('mouseup', this.endScrub)
        this.appendChild(this.canvas)
        this.createIndicator()
    }
    // indicator is a little arrow
    createIndicator() {
        this.indicator = document.createElement('img')
        this.indicator.src = '/move-horizontal.png'
        this.indicator.style.position = 'fixed'
        const size = 25
        this.indicator.style.top = `-${size / 2}px`
        this.indicator.style.left = `-${size / 2}px`
        this.indicator.style.width = `${size}px`
        this.indicator.style.height = `${size}px`
    }
    startScrub(ev) {
        this.canvas.requestPointerLock()
        this.canvas.addEventListener('mousemove', this.trackMouseMove)
        document.body.appendChild(this.indicator)
        const x = ev.clientX
        const y = ev.clientY
        this.transform = { x, y }
        this.indicator.style.transform = makeTransform(this.transform)
    }
    trackMouseMove(ev) {
        const detail = ev.movementX
        this.parentElement.dispatchEvent(new CustomEvent('input', { detail }))
        this.parentElement.updateIndicatorPosition(ev)
    }
    // detach events and indicator
    endScrub() {
        this.canvas.removeEventListener('mousemove', this.trackMouseMove)
        document.exitPointerLock()
        document.body.removeChild(this.indicator)
    }
    // update the indicator position
    updateIndicatorPosition({ movementX, movementY }) {
        const x = mod(this.transform.x + movementX, window.innerWidth)
        const y = mod(this.transform.y + movementY, window.innerHeight)
        this.transform = { x, y }
        this.indicator.style.transform = makeTransform(this.transform)
    }
}
// calculate the transform style property
const makeTransform = ({ x, y }) => `translateX(${x}px) translateY(${y}px)`

customElements.define('funk-scrubbable', Scrubbable)

// https://stackoverflow.com/a/13163436/3934396
var mod = function (n, m) {
    var remain = n % m
    return Math.floor(remain >= 0 ? remain : remain + m)
}

import { children, html, dispatch } from 'hybrids'
import TabItem from './tab-item'

// Function factory takes tab name and returns callback
// which can be added as event listener
function activate(name) {
    return host => {
        // Set next active element by it's name
        host.activeItem = name

        // After change custom event is dispatched
        // for the user of tab-group element
        dispatch(host, 'change')
    }
}

export default {
    // Children defined in 'tab-item.js'
    items: children(TabItem),

    // Sets and returns active item by name, which can be
    // used by the user of tab-group element
    activeItem: {
        set: ({ items }, name) =>
            items
                .filter(item => (item.active = item.name === name))
                .map(({ name }) => name)[0],
    },
    render: ({ items }) => html`
        <style>
            button.active {
                color: red;
            }
        </style>

        <nav>
            ${items.map(({ name, active }) =>
                html`
                    <button class="${{ active }}" onclick="${activate(name)}">
                        ${name}
                    </button>
                `.key(name)
            )}
        </nav>

        <slot></slot>
    `,
}

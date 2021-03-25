import { html } from 'hybrids'

// Explicit import of elements, that are used
// and defined in the template
import TabGroup from './tab-group'
import TabItem from './tab-item'

// Callback, which adds new dynamic element to items array
function addItem(host) {
    const number = host.items.length + 1

    host.items = [
        ...host.items,
        {
            name: `Dynamic ${number}`,
            content: `Dynamic content ${number}`,
        },
    ]
}

// Callback triggered by the `change` event of <tab-group>
// It is used to update changelog property (computed value)
function log(host, { target }) {
    // We can use `activeItem` property of <tab-group> to get active item
    host.changelog = target.activeItem
}

export default {
    items: [],
    changelog: {
        // Setter takes previous next state, connect with name
        // and produce new changelog
        set: (host, name, { next } = {}) => {
            return { previous: next, next: name }
        },
    },
    render: ({ items, changelog }) =>
        html`
            <tab-group onchange="${log}">
                <tab-item name="Static one" active>
                    <p>Static content in "One" section</p>
                </tab-item>

                ${items.map(({ name, content }) =>
                    html`
                        <tab-item name="${name}">
                            <p>${content}</p>
                        </tab-item>
                    `.key(name)
                )}
            </tab-group>

            <hr />

            <p>
                Dynamic items count: ${items.length}
                <button onclick="${addItem}">Add item</button>
            </p>

            ${changelog &&
            html`
                <p>
                    Change: from
                    <strong>${changelog.previous || '...'}</strong> to
                    <strong>${changelog.next}</strong>
                </p>
            `}
        `.define({ TabGroup, TabItem }),
}

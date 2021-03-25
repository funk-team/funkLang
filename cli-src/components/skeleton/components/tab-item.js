import { html } from 'hybrids'

export default {
    name: '',
    active: false,
    // Renders children (<slot/>) if active is set to true
    render: ({ active }) => html` ${active && html` <slot></slot> `} `,
}

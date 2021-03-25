import './Dropdown.css'
/**
 * Dropdown module
 * @module customElements/Dropdown
 * @description
 * Given button and content will produce a dropdown.
 * @example
 * <funk-dropdown>
 * <div class="funk-dropdown-button">Click me</div>
 * <div class="funk-dropdown-contents">Some options...</div>
 * </funk-dropdown>
 */

class Dropdown extends HTMLElement {
    // set up events
    connectedCallback() {
        this.classList.add('closed')
        this.handleButtonClick = ev => {
            if (this.classList.contains('open')) {
                this.classList.remove('open')
                this.classList.add('closed')
            } else {
                this.classList.add('open')
                this.classList.remove('closed')
            }
        }
        this.handleClickOutside = ev => {
            this.classList.remove('open')
            this.classList.add('closed')
        }
        // when an item in the content was selected, close the dropdown
        // exception: if the clicked item is 'insensitive', do not close the dropdown
        this.handleContentClick = ev => {
            // recursive function to detect if it's an insensitive row
            const isInsensitiveRow = el => {
                const isInsensitive = el.classList.contains(
                    'funk-insensitive-row'
                )
                const isContainer = el.classList.contains(
                    'funk-dropdown-contents'
                )

                if (isInsensitive) {
                    return true
                } else if (isContainer) {
                    // we have checked everywhere but could not find a class that tells that we are in an insensitive row
                    return false
                } else {
                    // when we are not at the container yet but have not found the class we go up by one
                    return isInsensitiveRow(el.parentElement)
                }
            }

            // only close if the row that was clicked is not sensitive to closing on click
            if (isInsensitiveRow(ev.target)) {
                return
            } else {
                this.classList.remove('open')
                this.classList.add('closed')
            }
        }
        this.button = this.querySelector('.funk-dropdown-button')
        this.contents = this.querySelector('.funk-dropdown-contents')
        this.button.addEventListener('click', this.handleButtonClick)
        this.addEventListener('clickoutside', this.handleClickOutside)
        this.contents.addEventListener('click', this.handleContentClick)
    }
    disconnectedCallback() {
        this.button.removeEventListener('click', this.handleButtonClick)
        this.removeEventListener('clickoutside', this.handleClickOutside)
        this.contents.removeEventListener('click', this.handleContentClick)
    }
}

customElements.define('funk-dropdown', Dropdown)

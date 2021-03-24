/**
 * This bad boi allows for detecting events that happen outside an element!
 * For this we attach an event listener to the document which catches events from anywhere.
 * Then we check if the element is somewhere in the hierarchy from the document to the event target.
 * If so, ignore it. If it's not in the hierarchy, the event happened outside and we want to inform the listener.
 */
export const apply = () => {
    // this map is used to associate our custom handlers with those in memory of the code that is adding `outside` event listeners
    // in order to find the right custom handlers when the consuming code removes their handlers again.
    document.clickoutsideHandlers = new Map();
    // we keep the original method here so that we can call it later
    // we only use this one from now on because we do not want to produce an infinite recursion loop
    const _addEventListener = HTMLElement.prototype.addEventListener;
    // patch addEventListener
    HTMLElement.prototype.addEventListener = function(
        type,
        handler,
        useCapture
    ) {
        // add new custom listener for everything that ends with `outside`
        if (type.endsWith("outside")) {
            // shave off the `outside` suffix
            const handlerName = type.substr(0, type.length - 7);
            const targetEl = this;
            const customHandler = ev => {
                // only fire if the target node is still in the DOM.
                // Elm will not disconnect event listeners and thus the handler on the document is still there.
                if (!this.isConnected) {
                    return;
                }
                const clickedElement = ev.target;
                var elementInHierarchy = clickedElement;
                while (elementInHierarchy.parentElement) {
                    if (elementInHierarchy == targetEl) {
                        return; // clicked inside
                    } else {
                        elementInHierarchy = elementInHierarchy.parentElement;
                    }
                }
                handler(ev);
            };
            // if the user clicks anywhere, check if the element we attached the listener to is the target or in the target's hierarchy
            _addEventListener.apply(document, [handlerName, customHandler]);
            document.clickoutsideHandlers.set(handler, customHandler);
        } else {
            _addEventListener.apply(this, [type, handler, useCapture]);
        }
    };
    const _removeEventListener = HTMLElement.prototype.removeEventListener;
    HTMLElement.prototype.removeEventListener = function(
        type,
        handler,
        useCapture
    ) {
        // remove new custom listener for clickoutside
        if (type.endsWith("outside")) {
            // shave off the `outside` suffix
            const handlerName = type.substr(0, type.length - 7);
            const targetEl = this;

            // find the good ol' handler and remove it from the element
            const customHandler = document.clickoutsideHandlers.get(handler);
            _removeEventListener.apply(document, [handlerName, customHandler]);
            document.clickoutsideHandlers.delete(handler);
        } else {
            _removeEventListener.apply(this, [type, handler, useCapture]);
        }
    };
};

import * as EventsOutside from "./domMonkeyPatches/eventsOutside";
import parseCurl from "./parse-curl";

// Elm is quite limited with regard to navigating and retrieving information from the DOM.
// We patch some functions on the native elements to tailor the platform to our needs
export const apply = app => {
    // prevent files from opening when dropping them in the browser
    window.addEventListener("dragover", ev => ev.preventDefault());
    window.addEventListener("drop", ev => ev.preventDefault());

    // preven safari pinch-to-zoom default behaviors
    window.addEventListener("gesturestart", ev => ev.preventDefault());
    window.addEventListener("gesturechange", e => e.preventDefault());
    window.addEventListener("gestureend", e => e.preventDefault());

    // Prevent keys from getting stuck when the user leaves the browser window.
    // https://package.elm-lang.org/packages/ohanhi/keyboard/2.0.0/Keyboard#update
    const notifyBlur = app => {
        window.addEventListener("blur", () =>
            app.ports.windowBlurred.send(null)
        );
        window.addEventListener("mousemove", ev => {
            !ev.getModifierState("Shift") && app.ports.shiftUp.send(null);
            !(ev.getModifierState("Meta") || ev.getModifierState("Control")) &&
                app.ports.metaUp.send(null);
        });
    };

    // prevent backspace from navigating in firefox
    window.addEventListener("keydown", ev => {
        const isTextInput =
            ["TEXTAREA", "INPUT"].indexOf(ev.target.tagName) > -1;
        const isContentEditable = ev.target.contentEditable === "true";
        if (isTextInput || isContentEditable) {
            // allow the user to delete the last character or selection
            // everythign else would be disappointing
            return;
        } else if (ev.key === "Backspace") {
            ev.preventDefault();
        }
    });

    // monkey-patch HTML so that elm can read properties from HTML elements
    HTMLElement.prototype.__defineGetter__("boundingClientRect", function() {
        return this.getBoundingClientRect();
    });

    HTMLElement.prototype.__defineGetter__("funk_forElement", function() {
        return document.getElementById(this.getAttribute("for"));
    });

    HTMLElement.prototype.__defineGetter__("funk_window", function() {
        return window;
    });

    HTMLElement.prototype.__defineGetter__(
        "funk_curl_parsed_input",
        function() {
            try {
                return parseCurl(this.value);
            } catch {
                return null;
            }
        }
    );

    const flatten = arrays => Array.prototype.concat(...arrays);

    HTMLElement.prototype.__defineGetter__("childrenAsArray", function() {
        const directChildren = Array.from(this.children);
        const absoluteChildren = directChildren
            .filter(el => el.classList.contains("fr"))
            .map(el => Array.from(el.children));
        return flatten([directChildren, flatten(absoluteChildren)]);
    });

    // this is useful for finding the element that another element is inside and we are targeting the handle
    HTMLElement.prototype.__defineGetter__("funk_closestParent", function() {
        return this.closest('[id^="el-"]') // get the element if we are on a handle
            .parentElement // get the parent and search further
            .closest('[id^="el-"]'); // because `closest` can also select the current element
    });

    // this is useful for finding the element that another element is inside and we are targeting the handle
    HTMLElement.prototype.__defineGetter__("funk_closestElement", function() {
        return this.closest('[id^="el-"]'); // get the element if we are on a handle
    });

    // this is useful for finding the element that another element is inside and we are targeting the handle
    HTMLElement.prototype.__defineGetter__("funk_isInSidebar", function() {
        return !!this.closest(".sidebar"); // get the element if we are on a handle
    });

    HTMLElement.prototype.__defineGetter__("funkCanvasElements", function() {
        return Array.from(document.querySelectorAll(".funk-canvas-element"));
    });

    HTMLElement.prototype.__defineGetter__("allFunkElements", function() {
        return Array.from(document.querySelectorAll("[id^='el-']"));
    });

    EventsOutside.apply();
};

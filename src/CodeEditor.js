const load = () => {
    return Promise.all([
        import(
            /* webpackChunkName: "codemirror-base" */ "codemirror/lib/codemirror"
        ),
        import(
            /* webpackChunkName: "codemirror-base", webpackMode: "eager" */ "codemirror/lib/codemirror.css"
        ),
        import(
            /* webpackChunkName: "codemirror-base" */ "codemirror/mode/elm/elm"
        ),
        import(
            /* webpackChunkName: "codemirror-base" */ "codemirror/mode/htmlmixed/htmlmixed"
        ),
        import(
            /* webpackChunkName: "codemirror-base" */ "codemirror/addon/lint/lint"
        ),
        import(
            /* webpackChunkName: "codemirror-base" */ "codemirror/addon/selection/active-line"
        ),
        import(
            /* webpackChunkName: "codemirror-base", webpackMode: "eager" */ "codemirror/addon/lint/lint.css"
        ),
        import(
            /* webpackChunkName: "codemirror-base", webpackMode: "eager" */ "codemirror/theme/material.css"
        )
    ]).then(([CodeMirror]) => CodeMirror);
};

const loadVimMode = () => {
    return Promise.all([
        import(
            /* webpackChunkName: "codemirror-vim" */ "codemirror/keymap/vim"
        ),
        import(
            /* webpackChunkName: "codemirror-vim" */ "codemirror/addon/dialog/dialog"
        ),
        import(
            /* webpackChunkName: "codemirror-vim", webpackMode: "eager" */ "codemirror/addon/dialog/dialog.css"
        )
    ]).then(() => {});
};

const debounce = (func, wait) => {
    let timeout;
    return function() {
        var later = function() {
            timeout = null;
            func.apply(null, arguments);
        };
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
    };
};

const initialize = () => {
    return load().then(CodeMirror => {
        CodeMirror.registerHelper("lint", "elm", (text, options, instance) => {
            return instance._errors || [];
        });

        customElements.define(
            "code-editor",
            class CodeEditor extends HTMLElement {
                constructor() {
                    super();
                    this._linterFormatDiv = document.createElement("div");
                    this._ready = false;
                    this._value = "";
                    this._tabSize = 4;
                    this._readOnly = false;
                    this._mode = "htmlmixed";
                    this._instance = null;
                    this._errors = [];
                    this._vimMode = false;
                    this._vimModeLoading = false;
                }

                get vimMode() {
                    return this._vimMode;
                }
                set vimMode(value) {
                    if (value === null) value = false;
                    this._vimMode = value;
                    if (!this._vimModeLoading && this._vimMode) {
                        this._vimModeLoading = true;
                        loadVimMode().then(() => {
                            if (!this._instance) return;
                            this._instance.setOption(
                                "keyMap",
                                this._vimMode ? "vim" : "default"
                            );
                        });
                    } else if (this._instance) {
                        this._instance.setOption(
                            "keyMap",
                            this._vimMode ? "vim" : "default"
                        );
                    }
                }

                get value() {
                    return this._value;
                }
                set value(value) {
                    if (value !== null && value !== this._value) {
                        if (!this._instance) {
                            return;
                        }
                        const prevScrollPosition = this._instance.getScrollInfo();
                        this._instance.setValue(value);
                        this._instance.scrollTo(
                            prevScrollPosition.left,
                            prevScrollPosition.top
                        );
                        this._value = value;
                    }
                }

                get tabSize() {
                    return this._tabSize;
                }
                set tabSize(value) {
                    if (value === null) value = 4;
                    this._tabSize = value;
                    if (!this._instance) return;
                    this._instance.setOption("indentWidth", this._tabSize);
                    this._instance.setOption("tabSize", this._tabSize);
                    this._instance.setOption("indentUnit", this._tabSize);
                }

                get readOnly() {
                    return this._readOnly;
                }
                set readOnly(value) {
                    if (value === null) value = false;
                    this._readOnly = value;
                    this._instance.setOption("readOnly", value);
                }

                get mode() {
                    return this._mode;
                }
                set mode(value) {
                    if (value === null) value = "htmlmixed";
                    this._mode = value;
                    if (!this._instance) return;
                    this._instance.setOption("mode", this._mode);
                }

                get linterMessages() {
                    return this._errors;
                }
                set linterMessages(value) {
                    if (value === null) value = [];
                    this._errors = value;
                    if (!this._instance) return;
                    this._instance._errors = this.formatLinterMessages(value);
                    this._instance.performLint();
                }

                connectedCallback() {
                    this.addEventListener("keydown", ev =>
                        ev.stopPropagation()
                    );
                    if (this._instance) return;
                    this._instance = CodeMirror.default(this, {
                        lineNumbers: true,
                        styleActiveLine: { nonEmpty: true },
                        smartIndent: true,
                        indentWithTabs: false,
                        keyMap: this._vimMode ? "vim" : "default",
                        lint: { lintOnChange: false },
                        theme: "material",
                        indentWidth: this._tabSize,
                        tabSize: this._tabSize,
                        indentUnit: this._tabSize,
                        readOnly: this._readOnly,
                        mode: this._mode,
                        value: this._value,
                        extraKeys: {
                            Tab(cm) {
                                let x = "";
                                for (
                                    let i = cm.getOption("indentUnit");
                                    i > 0;
                                    i--
                                )
                                    x += " ";
                                cm.replaceSelection(x);
                            }
                        }
                    });

                    const runDispatch = debounce(() => {
                        this._value = this._instance.getValue();
                        const event = new Event("change_");
                        this.dispatchEvent(event);
                    }, 200);

                    this._instance.on("change", runDispatch);

                    requestAnimationFrame(() => {
                        this._instance.refresh();
                    });

                    if (this._vimMode && !this._vimModeLoading) {
                        this._vimModeLoading = true;
                        loadVimMode();
                    }
                }

                formatLinterMessages(messages) {
                    return messages.map(message => {
                        this._linterFormatDiv.innerHTML = message.message;
                        return {
                            from: message.from,
                            to: message.to,
                            message: linterFormatDiv.innerText,
                            severity: message.severity
                        };
                    });
                }
            }
        );
    });
};

export default { initialize };

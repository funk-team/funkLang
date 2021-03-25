#!/usr/bin/env node

import Elm from '../elm-stuff/cli.js'
import readline from 'readline'
import Process from 'process'
import * as Server from './dev.mjs'
// import * as Components from "./components.mjs";
run()

function run(opts) {
    const program = Elm.Elm.Cli.init({
        flags: { argv: process.argv, versionMessage: '1.2.3' },
    })

    // program.ports.print.subscribe(message => {
    //     console.log(message);
    // });
    program.ports.printAndExitFailure.subscribe(message => {
        console.log(message)
        process.exit(1)
    })
    program.ports.printAndExitSuccess.subscribe(message => {
        console.log(message)
        process.exit(0)
    })
    program.ports.dev.subscribe(() => {
        Server.run()
    })
    // program.ports.generate.subscribe(componentName => {
    //     console.log("generating", componentName);
    //     generate.componentCreate(componentName);
    // });

    if (opts && opts.stdin) {
        const rl = readline.createInterface({
            input: process.stdin,
            output: process.stdout,
            terminal: false,
        })

        rl.on('line', function (line) {
            program.ports.onStdinLine.send(line)
        })

        rl.on('close', function (line) {
            program.ports.onStdinClosed.send(null)
        })
    }
}

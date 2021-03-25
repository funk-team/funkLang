#!/usr/bin/env node

import fs from 'fs'
import path from 'path'
import cproc from 'child_process'
import ifetch from 'isomorphic-fetch'
import git from 'isomorphic-git'
import * as ini from './components/init.mjs'
import * as comp from './components/components.mjs'
import * as funk_git from './components/funk_git.mjs'
git.plugins.set('fs', fs)

// Helpers
const defaultComponentsDir = path.resolve('./components/')

const init = to => {
    if (to === undefined) {
        to = path.resolve('.')
    } // get the absolute path

    from = path.join(__dirname + '/lib/skeleton')

    maybeIsPathClear = ini.isPathClear(from)(to)
    if (maybeIsPathClear[0] === false) {
        return maybeIsPathClear
    }

    files = fs.readdirSync(from)
    files.forEach(file => {
        whereFrom = path.join(from, file)
        if (fs.lstatSync(whereFrom).isDirectory()) {
            ini.copyDirRecursively(whereFrom)(to)
        } else {
            ini.copyFileSync(whereFrom)(to)
        }
    })

    return [true, 'all good so far. Initializing basic skeleton...']
}

const installDeps = () => {
    const install = cproc.exec('npm install', (err, stdout, stderr) => {
        console.log(stdout)
        console.log(stderr)
    })
    install.stdout.on('data', data => {
        console.log(data)
    })
    install.stderr.on('data', data => {
        console.warn(data)
    })
    install.on('exit', code => {
        console.log('child process exited with code: ' + code)
    })
    install.on('error', err => {
        console.log(err)
    })
    return [true, 'getting dependencies...']
}

const serve = () => {
    const parcel = cproc.exec('npx parcel ./index.html')
    parcel.stdout.on('data', function (data) {
        console.log(data)
    })
    parcel.stderr.on('data', data => {
        console.warn(data)
    })
    parcel.on('exit', code => {
        console.log('child process exited with code: ' + code)
    })
    parcel.on('error', err => {
        console.log(err)
    })
    return [true, 'serving...']
}

const makeSureDirExists = place => {
    // make sure the components directory exists
    if (fs.existsSync(place)) {
        if (fs.lstatSync(place).isDirectory()) {
            return
        }
    }
    console.log(
        "components dir doesn't exist... Have you run `funk init` already?"
    )
    fs.mkdirSync(place)
}

// NOTE: on github, if we try to get data about a private or non-existent repo,
// we get a 404 (Not Found) code in the header
// but for an existent repo, or a private one for which we've authenticated,
// we get a 200 (Ok) code in the response header

// TODO: Write tests for this
export const componentCreate = name => {
    makeSureDirExists(defaultComponentsDir)
    // TODO: will have to check the name doesn't contain illegal chars
    const thisComponentDir = path.join(defaultComponentsDir, name)
    const componentFilePath = path.join(thisComponentDir, name) + '.js'
    if (fs.existsSync(componentFilePath)) {
        if (!fs.lstatSync(componentFilePath).isDirectory()) {
            return [
                false,
                'The component ' + name + ' exists already. will not override',
            ]
        }
    }

    makeSureDirExists(thisComponentDir)
    maybeComp = comp.makeBasicComponent(name)
    if (maybeComp[0] === false) {
        return maybeComp
    }
    fs.writeFileSync(componentFilePath, maybeComp[1])
    return [true, 'made component']
}

const fetchRepo = author => repo => {
    let ret = false
    promise = ifetch(`https://api.github.com/repos/${author}/${repo}`)
        .then(function (response) {
            return response.status
        })
        .then(function (code) {
            if (code === 404) {
                return false
            } else if (code === 200) {
                return true
            } else {
                console.log('This response header code was not accounted for:')
                console.error(code)
                process.exit(1)
            }
        })
    return promise
}

const funkFetch = author => repo => {
    // make sure we have a place to put them in
    makeSureDirExists(defaultComponentsDir)
    const thisComponentDir = path.join(defaultComponentsDir, repo)
    if (fs.existsSync(thisComponentDir)) {
        return [
            false,
            'The component "' +
                repo +
                '" already exists. I won\'t override it.',
        ]
    }

    try {
        git.clone({
            dir: thisComponentDir,
            corsProxy: 'https://cors.isomorphic-git.org',
            url: `https://github.com/${author}/${repo}`,
            singleBranch: true,
            depth: 1,
        })
        console.log('checked out')
    } catch (e) {
        return [false, "couldn't fetch component: ", e]
        // console.log(e)
    }
    return [true, 'Cloning...']
}

// TODO: Write a sane parser instead of this function
// TODO: Write tests for this
const parseCmd = args => {
    if (args.length === 0) {
        return [
            false,
            "you haven't provided any commands... so what do you want me to do?",
        ]
    }

    if (args[0] === 'component') {
        if (args[1] === 'create') {
            if (args[2] !== undefined) {
                return componentCreate(args[2])
            } else {
                return [
                    false,
                    'the command should look something like: `funk component create <supply-a-name-here>`',
                ]
            }
        } else if (args[1] === 'fetch') {
            if (args[2] !== undefined) {
                maybeAuthorAndRepo = funk_git.parseUrl(args[2])
                if (maybeAuthorAndRepo[0] === false) {
                    return maybeAuthorAndRepo
                }
                const author = maybeAuthorAndRepo[1]
                const repo = maybeAuthorAndRepo[2]
                fetchRepo(author)(repo).then(doesRepoExist => {
                    if (doesRepoExist === false) {
                        console.log(
                            `Couldn't fetch funk component from https://github.com/${author}/${repo}\n\rThe repo is either private or non-existent.`
                        )
                    } else {
                        console.log(funkFetch(author)(repo))
                    }
                })
            }
        } else {
            return [
                false,
                'maybe you meant `funk component create <component-name>`?\n\r',
            ]
        }
    } else if (args[0] === 'serve') {
        return serve()
    } else if (args[0] === 'init') {
        ret = init()
        if (ret[0] === false) {
            return ret
        }
        console.log(installDeps())
        return ret
    } else {
        return [false, "I don't understand this argument: " + args[0]]
    }
}

// first two items in the `process.argv` list are the
// path to node and the path to the script. we don't need either
const cliArgs = process.argv.slice(2)
function main() {
    // this will ultimately (hopefully) either be:
    // a [ true, *some successful value*] OR
    // [ false, *some failure value (probably string)*]
    // it's basically a very caveman way of having the `Either` type from FP languages
    const result = parseCmd(cliArgs)

    return result
}

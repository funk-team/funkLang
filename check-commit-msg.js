/**
 * This husky hook checks if your commit message is well-formatted
 * The goals are to improve readability, consistency and integration with project managment processes
 */

// CONFIGURATION
const keywords = [
    'Add',
    'Allow',
    'Change',
    'Clean',
    'Connect',
    'Disable',
    'Disallow',
    'Document',
    'Enable',
    'Factor out',
    'Fix',
    'Hook in',
    'Implement',
    'Improve',
    'Integrate',
    'Make',
    'Prepare',
    'Re-add',
    'Refactor',
    'Refine',
    'Remove',
    'Rename',
    'Run',
    'Simplify',
    'Try',
    'Update',
    'Upgrade',
    'Use',
    'Write',
]

// READ ENVIRONMENT

const process = require('process')
const fs = require('fs')

// get the husky params which is pointing to the file with the commit msg
const huskyParams = process.argv[2]

// read the commit message given by the user
const msg = fs.readFileSync(huskyParams, 'utf8')

const GITHUB_TAG = 'GH-[1-9][0-9]+'

// MAKE REGEX AND CHECK BRANCH NAME

// enforce giving github issue context if available and an uppercase descriptive verb at the start
const regex = new RegExp(
    `^(${GITHUB_TAG}|no-issue)(;${GITHUB_TAG})*: (${keywords.join(
        '|'
    )}) [ 0-9a-zA-Z]+`
)

// if we have matches it's valid
const isValid = msg.match(regex) !== null

// GIVE USER FEEDBACK

// if it's valid tell the user
if (isValid) {
    console.info('Commit message is 👍 👍 👌')
} else {
    // if it's invalid annoy the user
    throw `Your commit message should match ${regex.toString()}`
}

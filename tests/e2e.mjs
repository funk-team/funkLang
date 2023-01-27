import puppeteer from 'puppeteer'
import fs from 'fs'
import Yadda from 'yadda'

import PrettyError from 'pretty-error'
import library_ from './e2e/library.mjs'
import esMain from 'es-main'

export const library = library_

/*

// script for the browser to delete all projects
const deleteAllProjects = () => {
        document.querySelector('[role="button"] svg').parentElement.click();
        
        setTimeout(() =>
            {
                  document.querySelector('[class*="fc-231"][role="button"]').click();
                  setTimeout(deleteAllProjects, 500)
            } , 100)
    }

// helper for debugging xpaths

function getElementByXpath(path, origin) {
  return document.evaluate(path, origin || document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;
}

*/

// instantiate PrettyError, which can then be used to render error objects
var pe = new PrettyError()

// White text on white ground is not so readable
pe.appendStyle({
    'pretty-error > header > message': { color: 'black' },
})
pe.start()

export const run = async library => {
    const featureParser = new Yadda.parsers.FeatureParser()
    const parse = file =>
        fs.promises
            .readFile(file, 'utf8')
            .then(text => featureParser.parse(text))
    const features = await Promise.all(
        new Yadda.FeatureFileSearch('./tests/e2e/features/').list().map(parse)
    )
    const activeFeatures = filterFocus(features)
    for (const feature of activeFeatures) {
        await runFeature(feature)
    }
}

const filterFocus = items => {
    const oneFocused = items.some(item => item.annotations.focus)
    return items.filter(item => {
        const isEffectivelyFocused = oneFocused ? item.annotations.focus : true
        const isSkipped = item.annotations.skip
        const isKept = !isSkipped && isEffectivelyFocused
        if (!isKept) {
            console.log('skipping', item.title)
        }
        return isKept
    })
}

const runFeature = async feature => {
    const yadda = Yadda.createInstance([library])

    // run one scenario
    const runOne = scenario =>
        new Promise((resolve, reject) => {
            // define ctx here because the callback needs access to it
            const ctx = {}
            // yadda usese callbacks, we translate this into a promise
            const callback = async (err, res) => {
                if (err) {
                    try {
                        await ctx.page.screenshot({
                            path: './tests/e2e/failure.png',
                            format: 'A4',
                        })
                    } catch (e) {
                        console.log('could not take screenshot')
                    }
                    reject(err)
                } else {
                    console.log('SCENARIO PASS')
                    await ctx.browser.close()
                    resolve()
                }
            }
            const { title, steps } = scenario
            console.log()
            console.log('RUNNING ', title)
            yadda.run(steps, { ctx }, callback)
        })

    // filter active scenarios
    const activeScenarios = filterFocus(feature.scenarios)
    const runAllScenarios = async activeScenarios => {
        for (const scenario of activeScenarios) {
            try {
                await runOne(scenario)
                console.log('SUCCESS ', scenario.title)
                console.log()
            } catch (err) {
                console.log(err)
                return false
            }
        }
        return true
    }
    const success = await runAllScenarios(activeScenarios)

    if (success) {
        console.log(`${feature.title}: ALL SCENARIOS PASS`)
    } else {
        throw new Error(`${feature.title}: FAIL`)
    }
}

if (esMain(import.meta)) {
    run(library)
}

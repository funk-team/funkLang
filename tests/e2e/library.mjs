/**
 * Step library for connecting feature files to e2e tests
 * TODOS:
 * factor out low-level and funk-specific
 * automatically log steps performed
 */
import Yadda from 'yadda'
import puppeteer from 'puppeteer'
import S from 'sanctuary'

// GLOBALS
// INFO: You will have to create the user first
const DEFAULT_TIMEOUT = 15000
const elementInset = 20

export default Yadda.localisation.English.library()

    .given('that I open the app', async function () {
        // init the context / the browser here because the step parsing does not happen before execution
        // otherwise we would wait for the browser to launch every time we want to see if the library matches the feature file
        this.ctx = Object.assign(this.ctx, await initContext())
        console.log('that I open the app')
    })
    .then('I see the editor', async function () {
        console.log('I see the editor')
        await seeEditor(this.ctx.page)
    })
    .when('I use the shortcut Shift-E', async function () {
        console.log('I use the shortcut Shift-E')
        await selectDrawTool(this.ctx.page)
    })
    .then('I use the shortcut Shift-E', async function () {
        console.log('I use the shortcut Shift-E')
        await selectDrawTool(this.ctx.page)
    })
    .when('I draw a screen with an element', async function () {
        console.log('I draw a screen with an element')
        await drawScreenWithElement(this.ctx.page)(0)
        await validateScreenAndElementStyles(this.ctx.page)
    })

    .when('I go to $modeName mode', async function (modeName) {
        await goToMode(this.ctx.page)(modeName)
    })
    .then('I go to $modeName mode', async function (modeName) {
        await goToMode(this.ctx.page)(modeName)
    })
    // NAVIGATION

    .then(
        'add the text $text to the currently selected element',
        async function (text) {
            await addTextContent(this.ctx.page)(text)
        }
    )
    .then('I draw another screen with an element', async function () {
        await drawScreenWithElement(this.ctx.page)(1)
    })
    .then('I select the element that says $text', async function (text) {
        await selectSelectAndTransformTool(this.ctx.page)
        await clickCenterOfElementByText(this.ctx.page)(text)
    })
    .then('I select the select and transform tool', async function () {
        await selectSelectAndTransformTool(this.ctx.page)
    })
    .then(
        'tell the element to navigate to the second screen',
        async function () {
            await setNavigationToSecondScreen(this.ctx.page)
        }
    )
    .when('I click the first preview button', async function () {
        const previewPage = keepNewTab(this.ctx.browser)
        await clickLinkByContent(this.ctx.page)('Preview')
        this.ctx.deployedPage = await previewPage
    })
    .then('I expect to see $text', async function (text) {
        await expectToSee(this.ctx.deployedPage)(text)
    })
    .when('I click the link that says $text', async function (text) {
        await clickLinkByContent(this.ctx.deployedPage)(text)
    })
    .when('go to icons', async function (text) {
        await clickUiElementByText(this.ctx.page)('ICONS')
        // make sure the tab is loaded
    })
    .when(
        'select an icon that covers the center of its viewbox',
        async function () {
            await selectCircleFillIcon(this.ctx.page)
        }
    )
    .then(
        'I wait for the icon to show up in my custom icon set',
        async function (text) {
            await this.ctx.page.waitForSelector('svg.jam-circle-f')
        }
    )
    .then(
        'I add the icon to the currently selected element',
        async function (text) {
            await goToMode(this.ctx.page)('Canvas')
            await clickAttributesPanelTab(this.ctx.page)('Content')
            await clickUiElementByText(this.ctx.page)('Icons')
            await this.ctx.page.waitForSelector('svg.jam-circle-f')
            const icon = await this.ctx.page.$('svg.jam-circle-f')
            await icon.click()
        }
    )
    .when('I click the element with the icon', async function (text) {
        await this.ctx.page.waitForSelector('svg.jam-circle-f')
        // select the element by content
        const elementIcon = await this.ctx.page.$('[id^="el-"] svg.jam')
        // https://stackoverflow.com/a/40333166/3934396
        const containingFunkElementXpath =
            'ancestor::*[starts-with(@id,"el-")][position()=1]'
        const [element] = await elementIcon.$x(containingFunkElementXpath)
        await selectSelectAndTransformTool(this.ctx.page)
        await clickCenterOfElement(this.ctx.page)(element)
        this.ctx.clickedElement = element
    })
    .then('I expect the element to be selected', async function (text) {
        await verifyElementIsSelected(this.ctx.page)(this.ctx.clickedElement)
    })
    // MOVE ELEMENT WITH HANDLES

    .when('I click the screen', async function () {
        const screen = await this.ctx.page.$('.funk-canvas-screen')
        const { left, top } = await findElementPosition(this.ctx.page)(screen)
        this.ctx.screen = screen
        await this.ctx.page.mouse.move(
            left + elementInset / 2,
            top + elementInset / 2
        )
        await this.ctx.page.mouse.down()
        await this.ctx.page.mouse.up()
        await this.ctx.page.waitForTimeout(100)
        await verifyElementIsSelected(this.ctx.page)(this.ctx.screen)
    })
    .then("I click and drag the screens's center handle", async function () {
        this.ctx.oldScreenPosition = await findElementPosition(this.ctx.page)(
            this.ctx.screen
        )
        await moveCurrentlySelectedElementUsingCenterHandle(this.ctx.page)(
            this.ctx.screen
        )
    })
    .then('I expect the screen to still be selected', async function () {
        await verifyElementIsSelected(this.ctx.page)(this.ctx.screen)
    })
    .then(
        'I expect the screen to be in a different location',
        async function () {
            // find new position of element
            const currentPosition = await findElementPosition(this.ctx.page)(
                this.ctx.screen
            )
            if (S.equals(this.ctx.oldScreenPosition)(currentPosition)) {
                throw new Error('screen has not moved')
            }
        }
    )
    .then('I click the $tabName tab', async function (tabName) {
        await clickAttributesPanelTab(this.ctx.page)(tabName)
    })
    .when('I add a custom typography style', async function () {
        await this.ctx.page.waitForSelector(
            "[aria-label='Customize typography']"
        )
        const addTypoButton = await this.ctx.page.$(
            "[aria-label='Customize typography']"
        )
        await addTypoButton.click()
    })
    .then('I set the font family to $family', async function (family) {
        await useDropdown(this.ctx.page)('Roboto')(family)
    })
    .when('I expect the font family to be honored', async function () {
        // get selected element
        const selectedElement = await this.ctx.page.$(".outline[id^='el-']")
        await ensureFontFaceIsLoadedForElement(this.ctx.page)(selectedElement)
    })

export const keepNewTab = async browser =>
    new Promise(resolve =>
        browser.once('targetcreated', async target => {
            const page = await target.page()
            page.setDefaultTimeout(DEFAULT_TIMEOUT)
            resolve(page)
        })
    )

export const seeEditor = async page => {
    await page.waitForSelector('.sidebar')
}
const selectDrawTool = async page => {
    // enter sticky keyboard mode
    await page.keyboard.down('Shift', { delay: 250 })
    await page.keyboard.press('E', { delay: 250 })
    await page.keyboard.up('Shift', { delay: 250 })
}
const selectCircleFillIcon = async page => {
    // sometimes the search field is not typed into, try to fix with timeout
    // waiting for "New Icon set" does not work.
    await page.waitForSelector('input[aria-label="Search icon"]')
    await page.type('input[aria-label="Search icon"]', 'circle-f')
    // select the fill circle from jam icons
    const blackCircle = 'img[src*="/circle-f.svg"]'
    await page.waitForSelector(blackCircle)
    const icon = await page.$(blackCircle)
    // make sure the CDN responds correctly
    await waitForImageToLoad(icon)
    await icon.click()
}

/**
 * Make sure screens have a white background and elements have a grey shade
 * When fixing #623 the screen backgrounds broke, that's why we need this check
 */
const validateScreenAndElementStyles = async page => {
    await page.waitForSelector(".funk-canvas-screen div[id^='el-']")
    const screen = await page.$('.funk-canvas-screen')
    const element = await page.$(".funk-canvas-screen div[id^='el-']")
    const screenStyles = await getStyles(page)(screen)
    const elementStyles = await getStyles(page)(element)
    if (screenStyles['background-color'] !== 'rgb(255, 255, 255)') {
        throw new Error(
            `Screen background is not white, it is ${screenStyles['background-color']}`
        )
    }
    if (elementStyles['background-color'] !== 'rgba(0, 0, 0, 0.05)') {
        throw new Error(
            `Element background is not the transparent grey, it is ${elementStyles['background-color']}`
        )
    }
    if (
        elementStyles['box-shadow']
            .replace(/\s+/g, ' ')
            .indexOf('rgba(100, 100, 100, 0.15)') < 0
    ) {
        throw new Error(
            `Element outline is not present, the shadow is ${elementStyles['box-shadow']}`
        )
    }
}

const getStyles = page => async element => {
    const styles = await page.evaluate(element => {
        const computed = window.getComputedStyle(element)
        // the CSSStyleDeclaration is not serializing well
        var serializeable = {}
        Object.entries(computed).forEach(
            ([_, key]) => (serializeable[key] = computed[key])
        )
        return serializeable
    }, element)
    return styles
}

const verifyElementIsSelected = page => async element => {
    await page.waitForTimeout(50)
    const classNameHandle = await element.getProperty('className')
    const className = await classNameHandle.jsonValue()
    if (className.indexOf('outline') > -1) {
        return
    } else {
        throw new Error(
            `element has no outline and hence does not seem to be selected, the className is ${className}`
        )
    }
}

const ensureFontFaceIsLoadedForElement = page => async element => {
    const styles = await getStyles(page)(element)
    const wantedFontFace = {
        family: styles['font-family'],
        weight: styles['font-weight'],
        style: styles['font-style'],
    }
    await page.waitForTimeout(2000)
    await checkFontIsLoaded(page)(wantedFontFace)(0)
}

const checkFontIsLoaded = page => wantedFontFace => async retries => {
    const maxRetries = 5
    const interval = 300
    const loadedFontFaces = await page.evaluate(async () => {
        var loadedFontFaces = []
        document.fonts.forEach(
            ({ status, family, weight, style }) =>
                status === 'loaded' &&
                loadedFontFaces.push({ family, weight, style })
        )
        return loadedFontFaces
    })

    const fontLoaded = S.any(S.equals(wantedFontFace))(loadedFontFaces)

    if (!fontLoaded) {
        if (retries > maxRetries) {
            throw new Error(
                `Font face '${wantedFontFace.family} ${wantedFontFace.style} ${wantedFontFace.weight}' not loaded.`
            )
        } else {
            // recurse to retry
            await checkFontIsLoaded(page)(wantedFontFace)(retries + 1)
        }
    }
}

const waitForImageToLoad = page => img =>
    page.evaluate(async () => {
        await Promise.all(
            selectors.map(img => {
                if (img.complete) return
                return new Promise((resolve, reject) => {
                    img.addEventListener('load', resolve)
                    img.addEventListener('error', reject)
                })
            })
        )
    }, img)

// START THE HEADLESS BROWSER

const initContext = async () => {
    const browser = await puppeteer.launch({ headless: false })
    const page = await browser.newPage()
    page.setDefaultTimeout(DEFAULT_TIMEOUT)
    await page.goto('http://localhost:3000', {
        waitUntil: 'networkidle2',
    })
    const uniqueProjectName = null
    const deployedPage = null

    await page.setViewport({ width: 1280, height: 900 })
    return { page, browser, uniqueProjectName, deployedPage }
}
const selectSelectAndTransformTool = async page => {
    // enter sticky keyboard mode
    const button = await page.$("[title*='Select & Transform']")
    await button.click()

    await page.waitForTimeout(100)
}

const drawScreenWithElement = page => async offset => {
    const screenWidth = 250
    const screenHeight = 350

    // determine position of screen based on size
    const screenPositionX = 400 + offset * (screenWidth + 50) // offset additional screens
    const screenPositionY = 100

    // element is square
    const elementWidth = screenWidth - 2 * elementInset
    const elementHeight = screenWidth - 2 * elementInset

    // draw screen
    await page.mouse.move(screenPositionX, screenPositionY)
    await page.mouse.down()
    await page.mouse.move(
        screenPositionX + screenWidth,
        screenPositionY + screenHeight
    )
    await page.mouse.up()

    // draw smaller element
    await page.mouse.move(
        screenPositionX + elementInset,
        screenPositionY + elementInset
    )
    await page.mouse.down()
    await page.mouse.move(
        screenPositionX + elementInset + elementWidth,
        screenPositionY + elementInset + elementHeight
    )
    await page.mouse.up()
    return
}

const clickAttributesPanelTab = page => async tabName => {
    const TAB_XPATH = `//div[contains(., '${tabName}') and @role='button']`
    await page.waitForXPath(TAB_XPATH)
    const [tab] = await page.$x(TAB_XPATH)
    if (tab) {
        await tab.click()
        return
    }
    console.error(`${tabName} tab not found`)
    throw `${tabName} tab now found`
}

export const addTextContent = page => async content => {
    await clickAttributesPanelTab(page)('Content')
    await page.waitForSelector('textarea')
    await page.type('textarea', content)
}

const clickUiElementByText = page => async text => {
    const xpath = `//*[contains(text(), "${text}")]`
    await page.waitForXPath(xpath)
    const [element] = await page.$x(xpath)
    await element.click()
}

// TOOLS FOR SELECTING FUNK ELEMENTS
const clickCenterOfElementByText = page => async text => {
    const xpath = `//*[funk-contenteditable[contains(., "${text}")]]`
    const [element] = await page.$x(xpath)
    await clickCenterOfElement(page)(element)
}

const findElementPosition = page => async element => {
    const rect = await page.evaluate(element => {
        // destructuring and rebuilding because this will get converted to JSON
        // and JSON can not handle circulars
        const { top, left, bottom, right } = element.getBoundingClientRect()
        return { top, left, bottom, right }
    }, element)
    return rect
}

const moveCurrentlySelectedElementUsingCenterHandle = page => async element => {
    const rect = await findElementPosition(page)(element)
    const [centerX, centerY] = findCenter(rect)
    await page.mouse.move(centerX, centerY)
    await page.mouse.down()
    await page.waitForTimeout(50)
    await page.mouse.move(centerX + 100, centerY + 100)
    await page.mouse.up()
}

const findCenter = rect => {
    const centerX = (rect.left + rect.right) / 2
    const centerY = (rect.top + rect.bottom) / 2
    return [centerX, centerY]
}

const clickCenterOfElement = page => async element => {
    const rect = await findElementPosition(page)(element)
    const [centerX, centerY] = findCenter(rect)
    await page.mouse.move(centerX, centerY)
    await page.mouse.down()
    await page.mouse.up()
}

// DEPLOY SPECIFIC

const goToMode = page => async modeName => {
    const MODE_XPATH = `//a[.//*[contains(., '${modeName}')]]`
    const [tab] = await page.$x(MODE_XPATH)
    if (tab) {
        await tab.click()
    } else {
        throw `${modeName} mode tab now found`
    }
}
export const clickLinkByContent = page => async content => {
    const [link] = await page.$x(`//a[contains(., '${content}')]`)
    try {
        await link.click()
    } catch {
        throw new Error(`Could not find link with text ${content}.`)
    }
}

export const expectToSee = page => async content => {
    await page.waitForXPath(`//*[contains(., '${content}')]`)
    return page
}

const setNavigationToSecondScreen = async page => {
    await clickAttributesPanelTab(page)('Actions')
    // add action
    await page.waitForXPath(`//div[contains(text(), "+")]`)
    const [plusButton] = await page.$x(`//div[contains(text(), "+")]`)
    await plusButton.click()

    // set action to navigate
    await page.waitForXPath(`//div[contains(text(), "Pick action")]`)
    const [dropdownButton] = await page.$x(
        `//div[contains(text(), "Pick action")]`
    )
    await dropdownButton.click()

    await page.waitForXPath(`//div[contains(text(), "Navigate")]`)
    const [dropdownOption] = await page.$x(
        `//div[contains(text(), "Navigate")]`
    )
    await dropdownOption.click()

    // set action to navigate
    await page.waitForXPath(`//div[contains(text(), "None selected")]`)
    const [dropdown2Button] = await page.$x(`//div[contains(text(), "None")]`)
    await dropdown2Button.click()
    console.log('clicked button - is the dropdown showing?')

    const targetScreenXpath = `//*[contains(@class, 'dropdown')]//div[contains(text(), "Screen 2")]`
    await page.waitForXPath(targetScreenXpath)
    const [dropdown2Option] = await page.$x(targetScreenXpath)

    await dropdown2Option.click()
}

const useDropdown = page => currentLabel => async optionToClick => {
    const buttonXPath = `//div[contains(@class, 'dropdown-button') and ./*[contains(text(), "${currentLabel}")]]`
    await page.waitForXPath(buttonXPath)
    const [button] = await page.$x(buttonXPath)
    await button.click()

    const optionXPath = `//div[contains(@class, 'dropdown-contents')]//*[contains(text(), "${optionToClick}")]`
    await page.waitForXPath(optionXPath)
    const [option] = await page.$x(optionXPath)
    await option.click()

    // expect the label to show
    const buttonAfterXPath = `//div[contains(@class, 'dropdown-button') and ./*[contains(text(), "${optionToClick}")]]`
    await page.waitForXPath(buttonAfterXPath)
}

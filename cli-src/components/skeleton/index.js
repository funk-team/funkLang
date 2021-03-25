// Add shims and polyfills
import '@webcomponents/webcomponentsjs/webcomponents-bundle.js'

// Import global stylesheets
import './style.css'

import { define } from 'hybrids'

import TabGroup from './components/tab-group'
import TabItem from './components/tab-item'
import TabDynamic from './components/tab-dynamic'

// Explicit definition for static example
define('tab-group', TabGroup)
define('tab-item', TabItem)

// Implicit definition inside of the tab-dynamic
define('tab-dynamic', TabDynamic)

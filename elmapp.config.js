const { createProxyMiddleware } = require('http-proxy-middleware')

const configureWebpack = (config, env) => {
    eval(process.env.CONFIGURE_WEBPACK)
    // adjustments required to use mjs files for code sharing between node and browser
    config.module.rules =
        // remove url-loader rule
        config.module.rules
            .map(
                rule =>
                    rule.loader && rule.loader.indexOf('url-loader') > -1
                        ? {}
                        : rule
                // add mjs rule
            )
            .concat([
                {
                    test: /\.mjs$/,
                    type: 'javascript/auto',
                },
            ])
    // remove uglify optimization because it's breaking uglifyJS
    config =
        config.mode === 'production'
            ? removeOptimizationMinimizer(config)
            : config
    return config
}
const removeOptimizationMinimizer = config => {
    config.optimization.minimizer = []
    return config
}

const setupProxy = app => {
    eval(process.env.SETUP_PROXY)
}

module.exports = {
    configureWebpack,
    setupProxy,
}

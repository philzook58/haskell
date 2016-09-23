module.exports = {
    entry: "./entry.js",
    output: {
        path: __dirname,
        filename: "bundle.js"
    },
    module: {
        loaders: [
            {
              test: /\.purs$/,
              loader: 'purs-loader',
              exclude: /node_modules/,
              query: {
                psc: 'psa',
                src: ['bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs']
              }
            }
        ]
    }
};
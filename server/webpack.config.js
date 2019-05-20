const path = require('path');
const VueLoaderPlugin = require('vue-loader/lib/plugin')

module.exports = {
  mode: 'development',
  entry: './public/js/tablet.js',
  output: {
    path: path.resolve(__dirname, 'public', 'js'),
    filename: 'bundle.js',
  },
  module: {
    rules: [
      { test: /\.css$/, use: ['style-loader', 'css-loader'] },
      { test: /\.scss$/,
        use: [
          'vue-style-loader',
          'css-loader',
          'sass-loader'
        ]
      },
      { test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/, use: 'file-loader' },
      {
        // vue-loader config to load `.vue` files or single file components.
        test: /\.vue$/,
        loader: 'vue-loader',
        options: {
            loaders: {
                // https://vue-loader.vuejs.org/guide/scoped-css.html#mixing-local-and-global-styles
                css: ['vue-style-loader', {
                  use: [{ loader: 'style-loader' }, { loader: 'css-loader' }],
                }],
                js: [
                    'babel-loader',
                ],
            },
            cacheBusting: true,
        },
      },
      {
        test: /\.m?js$/,
        exclude: /(node_modules|bower_components)/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-env']
          }
        }
      },
    ]
  },
  plugins: [
    new VueLoaderPlugin()
  ],
};
const path = require("path");
const webpack = require("webpack");
const isProd = process.env.npm_lifecycle_event === 'build';

module.exports = function makeConfig() {
  const config = {};

  config.entry =  {
    app: './src/index.js',
    vendor: './src/vendor.js'

  };

  config.output = {
    path: path.resolve(__dirname + '/dist'),
    filename: '[name].js',
  };

  config.resolve = {
    modules: [root('src'), root('node_modules')],
    //extensions: ['.js', '.elm']
  };

  config.module = {
    loaders: [
      {
        test: /\.js$/,
        include: [path.resolve(__dirname, 'src')],
        exclude: /(node_modules|bower_components)/,
        loader: 'babel-loader',
        query: {
          presets: ['es2015']
        }
      },
      {
        test: /\.(css|scss)$/,
        loaders: [
          'style-loader',
          'css-loader',
        ]
      },
      {
        test:    /\.html$/,
        exclude: /(node_modules|bower_components)/,
        loader:  'file?name=[name].[ext]',
      },
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/, /bower_components/],
        loader:  'babel-loader?presets[]=es2015!elm-webpack',
      },
      {
        test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'url-loader?limit=10000&mimetype=application/font-woff',
      },
      {
        test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        loader: 'file-loader',
      },
    ],

    noParse: /\.elm$/,
  };
  config.plugins = [];
  if (isProd) {
    config.plugins.push(
      new webpack.optimize.OccurenceOrderPlugin(),
      new webpack.optimize.DedupePlugin(),
      new webpack.optimize.UglifyJsPlugin({
        minimize: true,
        compressor: {warnings: false}
      })
    );
  }
  config.devServer = {
    inline: true,
    stats: { colors: true },
  };



  return config;
}();


// Helper functions
function root(args) {
  args = Array.prototype.slice.call(arguments, 0);
  return path.join.apply(path, [__dirname].concat(args));
}

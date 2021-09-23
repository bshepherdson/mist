const path = require('path');

module.exports = {
  mode: 'development',
  watch: true,
  entry: './js/main.mjs',
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist'),
  },
  module: {
    rules: [
      {
        test: /\.mjs$/,
        include: [path.resolve(__dirname, "js")],
        loader: 'babel-loader',
        options: {
          presets: ['@babel/preset-env'],
        },
      },
    ],
  },
};

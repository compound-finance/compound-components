'use strict';

const path = require('path');
const fs = require('fs');
const url = require('url');

// Make sure any symlinks in the project folder are resolved:
// https://github.com/facebookincubator/create-react-app/issues/637
const appDirectory = fs.realpathSync(process.cwd());
const resolveApp = (relativePath) => path.resolve(appDirectory, relativePath);

const envPublicUrl = process.env.PUBLIC_URL;

function ensureSlash(path, needsSlash) {
  const hasSlash = path.endsWith('/');
  if (hasSlash && !needsSlash) {
    return path.substr(path, path.length - 1);
  } else if (!hasSlash && needsSlash) {
    return `${path}/`;
  }
  return path;
}

const getPublicUrl = (appPackageJson) => envPublicUrl || require(appPackageJson).homepage;

// We use `PUBLIC_URL` environment variable or "homepage" field to infer
// "public path" at which the app is served.
// Webpack needs to know it to put the right <script> hrefs into HTML even in
// single-page apps that may serve index.html for nested URLs like /todos/42.
// We can't use a relative path in HTML because we don't want to load something
// like /todos/42/static/js/bundle.7289d.js. We have to know the root.
function getServedPath(appPackageJson) {
  const publicUrl = getPublicUrl(appPackageJson);
  const servedUrl = envPublicUrl || (publicUrl ? url.parse(publicUrl).pathname : '/');
  return ensureSlash(servedUrl, true);
}

module.exports = {
  appPath: resolveApp('./sample-app'),
  appPublic: resolveApp('./sample-app/public'),
  appHtml: resolveApp('./sample-app/public/app.html'),
  appIndexJs: resolveApp('./sample-app/src/app.js'),
  appSrc: resolveApp('./sample-app/src'),
  dotenv: resolveApp('./sample-app/.env'),
  appBuild: resolveApp('./sample-app/build'),
  elmJson: resolveApp('./sample-app/elm.json'),
  elm: require.resolve('elm/bin/elm'),
  publicUrl: getPublicUrl(resolveApp('./sample-app/elm.json')),
  servedPath: getServedPath(resolveApp('./sample-app/elm.json')),
};

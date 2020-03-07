let Main;

if (process.env.NODE_ENV === 'production') {
    Main = require('./app.js');
} else {
    Main = require('../output/Main');
}

function main () {
  Main.main(process.env.API_URL)();
}

// HMR setup. For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    document.body.innerHTML = "";
    main();
  });
}

main();

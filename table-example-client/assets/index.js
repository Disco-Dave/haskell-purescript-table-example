const Main = require('../output/Main');

function main () {
  Main.main('http://localhost:8081')();
}

// HMR setup. For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    document.body.innerHTML = "";
    main();
  });
}

main();

'use strict';

require('./styles.scss');

const {Elm} = require('./Main');
const storageKey = "store";
const flags = sessionStorage.getItem(storageKey);
const app = Elm.Main.init({flags: 6});

app.ports.storeCache.subscribe(function(val) {
  if (val === null) {
    sessionStorage.removeItem(storageKey);
  } else {
    sessionStorage.setItem(storageKey, JSON.stringify(val));
  }

  // Report that the new session was stored succesfully.
  setTimeout(function() { app.ports.onStoreChange.send(val); }, 0);
});

app.ports.toJs.subscribe(data => {
  console.log(data);
});

window.addEventListener("storage", function(event) {
  if (event.storageArea === sessionStorage && event.key === storageKey) {
    app.ports.onStoreChange.send(event.newValue);
  }
}, false);

const testFn = (inp) => {
  let a = inp + 1;
  return a;
}

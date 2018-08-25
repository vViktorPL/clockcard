'use strict';

const Elm = require('./elm.js');

let container = document.getElementById('container');
let app = Elm.Main.embed(container);

app.ports.save.subscribe(
  serializedState => {
    localStorage.setItem('state', serializedState);
  }
);
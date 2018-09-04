'use strict';

const Elm = require('./elm.js');
const initialState = localStorage.getItem('state');

let container = document.getElementById('container');
let app = Elm.Main.embed(container, initialState ? JSON.parse(initialState) : null);

app.ports.save.subscribe(
  stateModel => {
    // console.log(stateModel);
    localStorage.setItem('state', JSON.stringify(stateModel));
  }
);
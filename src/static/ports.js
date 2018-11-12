'use strict';

const { ipcRenderer } = require('electron');

const { Elm: { Main: ElmApp } } = require('./elm.js');
const initialState = localStorage.getItem('state');

let container = document.getElementById('container');
let app = ElmApp.init({
  node: container,
  flags: initialState ? JSON.parse(initialState) : null,
});

app.ports.save.subscribe(
  stateModel => {
    // console.log(stateModel);
    localStorage.setItem('state', JSON.stringify(stateModel));
  }
);

ipcRenderer.on('menu-jira-clicked', () => {
  app.ports.showJiraManager.send(null);
});

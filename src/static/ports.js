'use strict';

require('time-elements');
require('./datetime-picker');

const { ipcRenderer, remote } = require('electron');

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

app.ports.showElectronErrorBox.subscribe(
  ({ title = 'Error', content }) => {
    remote.dialog.showErrorBox(title, content);
  }
);

// Action links router
addChildEventListener(document, 'click', 'a[href^="clockcard:"]', function (event) {
  event.preventDefault();
  const action = this.href.split(':')[1];
  switch (action) {
    case 'jira-config-manager':
      app.ports.showJiraManager.send(null);
      break;
  }

});

function addChildEventListener(base, eventName, selector, handler) {
  base.addEventListener(eventName, function(event) {
    const closest = event.target.closest(selector);
    if (closest && base.contains(closest)) {
      handler.call(closest, event);
    }
  });
}
'use strict';

require('time-elements');
require('./datetime-picker');

const { ipcRenderer, remote, shell } = require('electron');

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
addChildEventListener(document, 'click', 'a', function (event) {
  event.preventDefault();

  const [protocol, ...afterProtocol] = this.href.split(':');

  if (protocol === 'clockcard') {
    const action = afterProtocol[0];
    switch (action) {
      case 'jira-config-manager':
        app.ports.showJiraManager.send(null);
        break;
    }
    return;
  }

  shell.openExternal(this.href);
});

function addChildEventListener(base, eventName, selector, handler) {
  base.addEventListener(eventName, function(event) {
    const closest = event.target.closest(selector);
    if (closest && base.contains(closest)) {
      handler.call(closest, event);
    }
  });
}
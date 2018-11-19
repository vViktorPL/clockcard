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

customElements.define('datetime-picker', class extends HTMLElement {
  connectedCallback() {
    this.createShadowRoot();
  }

  attachedCallback() {
    const input = document.createElement('input');
    input.setAttribute('type', 'datetime-local');
    input.addEventListener('change', () => {
      const value = input.value !== '' ? JSON.parse(JSON.stringify(new Date(input.value))) : '';

      this.dispatchEvent(new CustomEvent('localizedChange', { detail: { value } }));
    });

    this.shadowRoot.appendChild(input);
  }
});
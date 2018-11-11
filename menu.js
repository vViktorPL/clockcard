const { Menu, app, shell, ipcMain } = require('electron');
const defaultMenu = require('electron-default-menu')(app, shell);

module.exports = (window) => {

  const menuTemplate = [
    {
      label: 'Integration',
      submenu: [
        {
          label: 'JIRA',
          click() {
            window.webContents.send('menu-jira-clicked');
          }
        },
        {
          label: 'Google Spreadsheets',
          click() {
            window.webContents.send('menu-gspreadsheets-clicked');
          }
        },
      ],
    },
  ];


  return Menu.buildFromTemplate(
    defaultMenu.slice(0, 4)
      .concat(menuTemplate)
      .concat(defaultMenu[4])
  );
};
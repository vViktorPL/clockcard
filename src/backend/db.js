const sqlite3 = require('sqlite3').verbose();
const db = new sqlite3.Database('clockcard.db');


db.serialize(() => {
  db.run('CREATE TABLE IF NOT EXISTS issues (name TEXT NOT NULL)');

  // Timesheets
  db.run(`CREATE TABLE IF NOT EXISTS timesheets (
    issue_id INTEGER NOT NULL,
    period_in_progress INTEGER,
    jira_next_ref_index INTEGER NOT NULL,
    FOREIGN KEY(issue_id) REFERENCES issues(rowid)
  )`);
  db.run(`CREATE TABLE IF NOT EXISTS timesheet_periods (
    timesheet_id INTEGER NOT NULL,
    start INTEGER,
    \`end\` INTEGER,
    FOREIGN KEY(timesheet_id) REFERENCES timesheets(rowid)
  )`);
  db.run(`CREATE TABLE IF NOT EXISTS timesheet_period_logrefs (
    period_id INTEGER NOT NULL,
    \`type\` TEXT NOT NULL,
    \`data\` TEXT NOT NULL,
    FOREIGN KEY(period_id) REFERENCES timesheet_periods(rowid)
  )`);

  // JIRA integration
  db.run(`CREATE TABLE IF NOT EXISTS jira_destinations (
    \`name\` TEXT NOT NULL,
    host TEXT NOT NULL,
    auth_username TEXT NOT NULL,
    auth_password TEXT NOT NULL,
    valid BOOLEAN NOT NULL
  )`);
  db.run(`CREATE TABLE IF NOT EXISTS jira_destination_projects (
    destination_id INTEGER NOT NULL,
    id TEXT NOT NULL,
    \`name\` TEXT NOT NULL,
    \`key\` TEXT NOT NULL,
    avatar_urls TEXT NOT NULL,
    FOREIGN KEY(destination_id) REFERENCES jira_destinations(rowid)
  )`);
  db.run(`CREATE TABLE IF NOT EXISTS jira_logs (
    timesheet_id INTEGER NOT NULL,
    ref INTEGER NOT NULL,
    issue_key TEXT NOT NULL,
    issue_url TEXT NOT NULL,
    logged_time INTEGER NOT NULL,
    commit_time INTEGER NOT NULL,
    FOREIGN KEY(timesheet_id) REFERENCES timesheets(rowid)
  )`);
});

const fetchAll = (query, params = []) => new Promise(
  (resolve, reject) => {
    db.all(query, params, (err, rows) => {
      if (err) {
        reject(err);
        return;
      }

      resolve(rows);
    });
  }
);
const fetchFirst = (query, params = []) => new Promise(
  (resolve, reject) => {
    db.get(query, params, (err, row) => {
      if (err) {
        reject(err);
        return;
      }

      resolve(row);
    });
  }
);
const executeStatement = (query, params = []) => new Promise(
  (resolve, reject) => {
    const stmt = db.prepare(query);
    stmt.run.apply(stmt, [
      ...params,
      function (err) {
        if (err) {
          reject(err);
          return;
        }

        resolve(this);
      },
    ]);
  }
);


module.exports = { db, fetchAll, fetchFirst, executeStatement };
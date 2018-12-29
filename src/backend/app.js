const { flow, groupBy, map } = require('lodash/fp');
const elexpress = require('./elexpress');
const { fetchFirst, fetchAll, executeStatement } = require('./db');
const app = elexpress();

app.get('/db/issues', (req, res) =>
  fetchAll(`
    SELECT i.rowid as id, i.name as name, t.period_in_progress as timesheet_period_in_progress
    FROM issues i
    JOIN timesheets t on i.rowid = t.issue_id
  `).then(
    rows => rows.map(
      ({ id, name, timesheet_period_in_progress }) => ({
        id,
        name,
        timesheet: {
          periodInProgress: timesheet_period_in_progress
        },
      })
    )
  ).then(res.send)
);

app.get('/db/issues/:id', (req, res) =>
  fetchFirst(`
    SELECT i.rowid as id, i.name as name, 
    t.period_in_progress as timesheet_period_in_progress,
    t.jira_next_ref_index as timesheet_jira_next_ref_index
    FROM issues i
    JOIN timesheets t on i.rowid = t.issue_id
    WHERE i.rowid = ? LIMIT 1
  `, [req.params.id])
    .then(issueRow => Promise.all([
      Promise.resolve(issueRow),
      fetchAll(`
        SELECT * FROM timesheet_periods p 
        LEFT JOIN timesheet_period_logrefs l ON l.period_id = p.rowid 
        WHERE p.timesheet_id = ?
      `, [issueRow.id]),
      fetchAll(`
        SELECT * FROM jira_logs
        WHERE timesheet_id = ?
      `, [issueRow.id]),
    ]))
    .then(
      ([issueRow, periodRows, jiraLogRows]) => ({
        id: issueRow.id,
        name: issueRow.name,
        timesheet: {
          periodInProgress: issueRow.timesheet_period_in_progress,
          finishedPeriods: flow([
            groupBy('start'),
            map(logrefs => ({
              start: logrefs[0].start,
              end: logrefs[0].end,
              logrefs: logrefs.map(({ type, data }) => ({ type, data })),
            }))
          ])(periodRows),
          integrations: {
            jira: {
              nextRefIndex: issueRow.timesheet_jira_next_ref_index,
              logs: jiraLogRows.map(
                ({ ref, issue_key, issue_url, logged_time, commit_time }) => ({
                  ref,
                  issueKey: issue_key,
                  issueUrl: issue_url,
                  loggedTime: logged_time,
                  commitTime: commit_time,
                })),
            }
          }
        }
      }))
    .then(res.send)
);

app.post('/db/issues', (req, res) =>
  executeStatement(
    'INSERT INTO issues VALUES (?)',
    [req.body.name]
  ).then(
    ({ lastID }) => executeStatement(
      'INSERT INTO timesheets VALUES (?, ?, ?)',
      [lastID, null, 1]
    ).then(lastID)
  ).then(issueId => {
      res.send({
        id: issueId,
        name: req.body.name,
        timesheet: {
          periodInProgress: null,
          finishedPeriods: [],
          integrations: {
            jira: {
              nextRefIndex: 1,
              logs: []
            }
          }
        }
      });
  })
);

module.exports = app;
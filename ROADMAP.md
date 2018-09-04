# ClockCard - Roadmap

## App state load/save

As every app, ClockCard requires some state to be saved and restored when app launches to begin with state that we left app with.

Currently, very simple solution using LocalStorage has been implemented. But in future some better replacement should be implemented
for instance [TeDB](https://github.com/tedb-org/teDB#readme). This will probably require also some changes in `src/elm/` code to change state saving behavior which saves whole
app state currently on every major change like stopwatch start/pause or new issue creation. This behavior should be changed to 
something more optimal as with change of an engine we will have possibility to update for instance specific stopwatch that
has been started or paused instead of encoding whole issue list to JSON each time. For now TeDB looks promising 
but some other solution might be chosen in future.

* ✓ LocalStorage based state of an app
* Switch to TeDB for better performance

## List improvements

* Enable empty selectable list (because current model uses SelectableList which does not support empty lists)
* Issue removing
    * Confirmation before removal
* Issue renaming on double click

## Commits

The main inspiration for this app was the problematic process with everyday time tracking. 
When we cooperate with our clients, we are often required to log time for instance in JIRA. 
Commits feature will integrate ClockCard with JIRA or Google Spreadsheets, by enabling user to log time in these systems.

Each issue stopwatch consists of periods when it was started and paused. Such list of periods should appear under the currently displayed stopwatch
and every list entry will have action icon/button which enables user to log time in specific system. If user already logged time
in specific system, icon of the specified system will indicate that period has been already logged and clicking on it will open webbrowser
to place where time was logged (JIRA issue or Google Spreadsheet cell).

* remove reset button from stopwatch
* every paused period will appear on commits list
* "push log" 
    * icon next to period which enables to log time
    * bulk "push log" (checkboxes on list and bulk action button)
    * round up precision options before pushing (options: minutes, 15m, 30m, 1h, custom)
    * JIRA integration
    * Google Spreadsheets integration
* JIRA projects management
* Spreadsheets management

## Improve Issue Model

Currently, the Issue Model is very simple. Some additional fields will improve it's usefulness.

* Tagging issue with categories (for instance "👨‍💻 coding",  "💬 meeting", "🔧 refactor", "📽 demo" etc)
* Personal notes textarea - user may want to add some comment for each issue

## Other

* Tray icon
* Hotkeys
* Export / Import data (backup to JSON file)
* Settings (in menu)
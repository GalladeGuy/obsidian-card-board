# Obsidian CardBoard Plugin

![License](https://img.shields.io/github/license/roovo/obsidian-card-board)
![GitHub release (latest by date)](https://img.shields.io/github/v/release/roovo/obsidian-card-board?style=flat-square)
![Obsidian Downloads](https://img.shields.io/badge/dynamic/json?logo=obsidian&color=%23483699&label=downloads&query=%24%5B%22card-board%22%5D.downloads&url=https%3A%2F%2Fraw.githubusercontent.com%2Fobsidianmd%2Fobsidian-releases%2Fmaster%2Fcommunity-plugin-stats.json)


An [Obsidian](https://obsidian.md/) plugin to make working with tasks a pleasure
(hopefully anyway).

- Uses regular tasks/subtasks wherever they are in your vault.
- Shows them on kanban style boards.
- Two board types supported:
  - Date based (with daily/periodic notes support).
  - Tag based (uses `#tags` to define your boards).

## New
- Ability to use css snippets to
  [set card hightlight color](#customising-card-highlight-color) based on tags
- Board reordering
  - drag and drop tab headers or board names in the settings dialog
- Improved styling of tab headers (consistent with obsidian)

![date based board screenshot](/images/dateBoard.png?raw=true)

## Installation
Please install via the regular Community Plugins setting tab within Obsidian.

## Use
When installed, you can launch the plugin:

- using the icon in the app ribbon (see below), or
- using the Command Pallete command (which allows you to open a specific board).

![app ribbon icon](/images/ribbonIcon.png?raw=true)

If you have no boards defined, you should get a dialog asking you to add a new
board.  There are 2 types of board:

- **Date based**: looks like the main screenshot above.
- **Tag based**: uses tags to define the columns (you need to include tags on
  your tasks or in front matter for this to work).

Name and configure your boards and you are good to go.

## Cards
Any task in your vault can appear as a card in a column on a board.  In order to
do this, it must:

- Be in a markdown file.
- Not be indented.
- Use one of the commonmark supported unordered list formats:
  - `- [ ] Task title`
  - `* [ ] Task title`
  - `+ [ ] Task title`

What appears on the card depends on what your task looks like:

- Anything that is indented under a task will appear in the body of the task.
  - Indented tasks will appear as subtasks (all subtasks are grouped together).
  - Indented text will appear as notes.
- `#tags` in front matter or on the line of the task (or any subtasks) will
  appear at the top of the card.
- Due date (if given) will appear at the bottom of the card.

So, if you had the following in one of your markdown files:

```
- [ ] run erands @due(2022-12-02)
  - [x] do shopping #town
  - [ ] wash car #home/outside
  - [ ] cook dinner #home/kitchen

  perhaps I should look up some [[example_tasks/recipes|recipes]] first

  - [ ] do something with a long title that will truncate when displayed
  - [ ] go to bed
```

It will look something like this on a card on your board:

![example card](/images/card.png?raw=true)

### Marking a task as complete
If you mark an task as complete on the board it will be marked as completed in the markdown
(and vice-versa).  If you mark as complete on the board, a completion timestamp is appended
to the task:

```
- [x] Task title @completed(2021-10-30T13:57:48)
```

See the [compatibility section](#other-plugin-compatibility) for details on how you can choose to use
a format compatible with other plugins.

If you have subtasks and the parent task is tagged as an _autocomplete_ task then the main
task will be marked as complete when you tick off the final subtask:

```
- [ ] Task title @autocomplete(true)
  - [ ] Do this first
  - [ ] Do this next
  - [ ] Finally do this and you are done
```

### Deleting a task
You can delete a task using the trash icon on the card.  This will not actually delete
the task from your vault, it simply surrounds it with markdown `<del>` tags:

```
<del>- [x] Task title</del>
```

### Editing tasks (and hover preview)
Click on the edit icon to open the file containing the task.  Cmd (or Ctrl on windows)
hover over the icon for the normal Obsidian hover preview.


### Card ordering in columns
The current behaviour for the different columns is:

- **Completed**: has the most recently completed at the top (assuming they were
  marked as complete using the checkbox on the board).
- **Future**, **Today**, **Tags**: these are sorted by due date and then
  alphabetically within this.
- other columns are sorted alphabetically.

I am not convinced that this is the best strategy so this may well change in a future release.

### Customising Tags
I have to recommend using the wonderful
[Colorful Tag](https://obsidian.md/plugins?id=obsidian-colorful-tag) plugin as this
will allow you to style tags on your markdown pages as well as in CardBoard.

If you would like to "roll your own", you can using a CSS snippet containing something
along the lines of:

```css
.card-board-view a.tag[href="#foo/bar"] {
  background-color: HotPink;
  color: DimGrey;
}
```

This will style the tag `#foo/bar` wherever it appears in the CardBoard view with your favorite
color for foo/bars, which just has to be HotPink :)

### Customising Card Highlight Color
By default cards with due dates will have a colored bar on their left hand side
to give a visual indication of when they are due.  These colours are dependent on
the theme you are using.  There are two way to over-ride this, both of which require
the use of [css snippets](https://help.obsidian.md/Extending+Obsidian/CSS+snippets).

If you wish to stick with the due date based behaviour, but would like to choose the
colors, you can use a snippet along these lines:

```css
/* overdue */
.card-board-view .card-board-card-highlight-area.critical {
  background-color: red;
}

/* due today */
.card-board-view .card-board-card-highlight-area.important {
  background-color: orange;
}

/* due after today */
.card-board-view .card-board-card-highlight-area.good {
  background-color: green;
}
```

If you wish to set the color based on tags, then you can use a snippet such as:

```css
.card-board-card[data-tags~=status-doing] .card-board-card-highlight-area {
  background-color: yellow;
}
```

Which will set the color to yellow for all cards with a tag `#status/doing`. This will
override any date based highlighting.  If you use nested tags, you will need to replace
the `/` character with a `-` (as in the example above).  If the card has multipler tags
which you have set different colors for, the last one read from your snippets will take
priority.  Tag based colors will be applied for tags even if they are [hidden](#hiding-tags).

## Boards
Two board types supported:
  - Date based (with daily/periodic notes support).
  - Tag based (uses `#tags` to define your boards).

### Reordering boards
Boards can be reordered by dragging their tabs on the main view.

### Date boards
You will get the best out of these if you are using the (core) Daily Notes
or the (community) Periodic Notes plugins, as any tasks you place on a daily
note will be assigned to the day of the note.  If you do not want this behaviour
you can turn it off in the Global Settings pane, then tasks on daily notes
pages will not have a due date unless specifically specified.

![filters](/images/ignoreFileNameDatesSetting.png?raw=true)

You can assign a date to any task using the format:

```
- [ ] My task @due(2021-10-31)
```

Cardboard also understands the format used by Dataview and Tasks:

```
- [ ] My task [due:: 2021-10-31]
- [ ] My task 📅 2021-10-31
```

A due date specified on a task line will overide any date derived from a task
being on a daily note page.

You can turn off the due date for a specific task on a daily note page:

```
- [ ] My task @due(none)
```

#### Overdue tasks
These will appear in the `Today` column above any  any tasks that are actually
due today.

The idea being that it will get steadily more annoying to see what you were planning
to do today if you have a lot of incomplete tasks from previous days, (hopefully)
encouraging you to do something about them; like do them or move them to a future
date if you want to schedule them later.


#### Completed tasks
If you choose to show completed tasks on your date board, you will only see
completed tasks which have a due date unless the Undated column is enabled.


### Tag boards
If you give your tasks tags, you can use these to set up a tag-board.  So if you
have the tags `#status/backlog`, `#status/triaged`, `status/blocked`, `#status/doing`,
you can define a board that shows tasks tagged with these in separate columns:

![tag board settingx](/images/tagBoardSettings.png?raw=true)


#### Sub-tasks
Tasks which have sub-tasks with matching tags will also appear on the board.

#### Subtags
If you specify a tag with a trailing `/` then the column will contain all subtags of the tag.

#### Front Matter Tags
If you want to give all the tasks on a page the same tag, you can put it in the
page front matter:

```
---
tags: [ project1 ]
---

# Project 1

- [ ] this task will automatically have a project1 tag
```

#### Hiding Tags
If you don't want to see the tags used to configure the board's columns on the cards,
you can show/hide them in the settings.  If you choose not to show the column tags,
this will hide all the tags used in the settings wherever cards are on the board.
Only tags that exactly match those used in the settings will be hidden.

#### Completed tasks
When a task is shown in a column due to tags on sub-tasks it will only show in that
column if those subtasks are incomplete.  So the following
task is shown only in the Barney column as the Wilma sub-task is complete:

![wilma_barney](/images/wilma_barney.png?raw=true)

This does mean that a card can appear in the completed column of a tag board even if
the top level task is not complete, e.g for the above example if the barney task is
marked as complete the card will move to the Completed column:

![wilma_barney_completed](/images/wilma_barney_completed.png?raw=true)

The board config defines which tasks will appear on a Tag board, and this is true for
the completed column too; only tasks which would appear on the board if all tasks and
subtasks were marked as incomplete can ever be in the Completed column.


### Board Filters
You can filter which tasks appear on each board in the board settings.  There are 3
types of filter you can use: file, path, and #tags (which includes front matter tags).  You can
use any combination of these on a per-board basis.

You can also:

- Choose whether to use the filters as an allow or a deny list.
- Choose if you want any tags specified as a filter to be shown or
  hidden on cards on the board.

Filters are applied before tasks are placed onto a board:

![filters](/images/filters.png?raw=true)


## Settings
Plugin settings are accessible from the plugin view itself, via the settings icon
above the board to the left of the tabs.  You can:

- Create new boards (using the + icon next to _BOARDS_).
- Configure your boards.
- Customize the names of the built-in columns.
- Delete any boards you no longer need.
- Choose whether to use Cardboard, Dataview or Tasks format for marking task completion.
- Choose to not use the date of daily notes files as the due date for tasks.

The settings for your boards are saved in the

```
.obsidian/plugins/card-board/data.json
```

file inside your vault.  You may see some older versions (e.g. data.0.5.0.json) in there
as well.  These are saved when the internal version of the settings file is updated, just
in case something goes wrong!  If you want to ensure you never loose your CardBoard settings
then do ensure that your .obsidian directory is backed up.

## Other Plugin Compatibility
Cardboard is compatible with the *Due* and *Completion* date formats used in
both [Tasks](https://obsidian-tasks-group.github.io/obsidian-tasks/)
and [Dataview](https://blacksmithgu.github.io/obsidian-dataview/).
Dates from both of these are understood with no configuration.

When marking a task as complete, you can choose which format to use via
CardBoard's Global Settings:

![task completion format setting panel](/images/taskCompletionFormatSetting.png?raw=true)

You can also set this value to *None* if you don't want any completion date or timestamp adding
to a task on completion.  **This is not recommended** as if you do this, the task may well not
appear at (or near) the top of any completed column you have on your board when you mark
it as complete.  CardBoard uses this time or date so it knows which are the most
recently completed tasks so they can be shown at the top of the column.

### Dataview Plugin

```
- [ ] todo in Dataview format [due:: 2021-10-30] [completion:: 2021-10-29]
```

Will be read as a task with a due date of 30th Oct 2021 which was completed a day early.

Cardboard will honour the task related settings in Dataview, so if you set Dataview
to use emoji completion or a different "completion" string (such as "done") then
CardBoard will mirror this.

### Tasks Plugin

```
- [ ] todo in Tasks format 📅 2021-10-30 ✅ 2021-10-29
```

Will be read as a task with a due date of 30th Oct 2021 which was completed a day early.

#### Recurring Tasks
**CardBoard does not understand recurring tasks**, even if you have set it up to use Tasks
format for marking tasks as complete.   Checking off a recurring task from
the CardBoard board view will add the completion date in Tasks format but **will
not generate a new instance of the recurring task**.

To get the correct behavior for recurring tasks, click the edit icon on the card
to go to the file where the task is written, and then use the
*"Tasks: Toggle Done"* command or click the checkbox from there.

## Scaling Board Text and Column Sizes
Use the following css snippet to customize these:

```css
.card-board-view {
  font-size: 1em;
}

.card-board-view .card-board-column {
  width: 20em;
}
```

You can alter the general size of the contents of the cards by changng the font-size,
and/or set the width of the columns by changing the width setting.


## Limitations
- Might not work that great on large vaults (as it parses all markdown files at startup).
- Might not work that great on large files (as it parses all markdown files, and doesn't use
  any form of cache).
- Might not be great on mobile (see previous, plus I haven't put any particular effort into
  making the interface mobile friendly - yet).

## Alternatives
If the way that this works isn't for for you, there are plenty of other fabulous
plugins you can use for task management in Obsidian, e.g.
[Checklist](https://github.com/delashum/obsidian-checklist-plugin),
[Kanban](https://github.com/mgmeyers/obsidian-kanban),
[Reminder](https://github.com/uphy/obsidian-reminder),
[Tasks](https://github.com/schemar/obsidian-tasks),
[Projects](https://github.com/marcusolsson/obsidian-projects), as well as the
[Tasks Calendar snippet](https://github.com/702573N/Obsidian-Tasks-Calendar).
There are others too, see the wonderful
[Obsidian Plugin Stats site](https://obsidian-plugin-stats.vercel.app).


## Contributing
I am working on this myself for now; it's my do-some-coding-when-I-have-some-time
project so I am not accepting pull requests.  However, if you want to mess around
with the code then see [contributing doc](CONTRIBUTING.md) for more info on getting
a dev environment set up and running.

If you have any thoughts, ideas, bugs n stuff:

- **Bugs/suggestions/feature requests** - [github issues](https://github.com/roovo/obsidian-card-board/issues).
- **What's being worked on and up next?** - [CardBoard Dev](https://github.com/users/roovo/projects/4)
- **Questions/discussions** - [github discussions](https://github.com/roovo/obsidian-card-board/discussions)


[![Build Status](https://travis-ci.org/deltadak/plep.svg?branch=master)](https://travis-ci.org/deltadak/plep)

# plep
Plepping is the new planning.

Find the latest stable version (v1.2.1) [here](https://github.com/deltadak/plep/blob/master/releases/plep_v1.2.1.jar?raw=true).
You can also download the [experimental windows installer](https://github.com/deltadak/plep/blob/master/releases/setup/setup_plep_v1.2.1.exe?raw=true)

Find the latest debug build [here](https://github.com/deltadak/plep/blob/master/out/artifacts/plep_jar/plep.jar?raw=true).

## Features/User Manual
To install plep, download the version you want. The first time plep is runned, it will create a database file in the folder plep.jar is stored. Below we have listed a few things that might come in handy, as well as known bugs we have to live with, for now.
+ To **move** a task from one day to another, click, hold, and drag the task to the day you wish to move it to. 
  * Sometimes when you drag an item into a cell/task that is selected (dark grey) it disappears.
+ To **delete** a task, select it and press the delete button on your keyboard.
+ To **edit** or **create** a task, double-click on it to make it editable. When you you are done editing, press enter to submit.
  * Not pressing enter results in the label being removed.
  * When you click a selected cell/task, it will see this as double-click and start editing.
+ To **edit** a label, click the label you wish to edit. A drop-down menu will show from which you can select the label you want.
+ To view other days, use the forward (>) and backward (<) button, go back to today in view with the 'today' button. 
+ To **change settings** click the settings button and then you can edit the labels used, number of days shown and number of days skipped by the 'forward' and 'backward' buttons. 

## Screenshot of v1.1
![screenshot](v1.1.PNG)

## Release Notes
Bugs solved and features, listed by the version in which they were added.

#### [v1.2.2](https://github.com/deltadak/plep/blob/master/releases/plep_v1.2.2.jar?raw=true)
+ Option to choose how much columns to display, or let it be calculated automatically.
+ `Ctrl+Z` undoes deletion of an item.
+ Added a features page with some keyboard shortcuts.
+ Fixed a bug which allowed half-selected cells to be everywhere.
+ Fixed a bug in which the selector overrided the colors.

#### [v1.2.1](https://github.com/deltadak/plep/blob/master/releases/plep_v1.2.1.jar?raw=true)
+ Plep now installs with a setup.exe
+ Settings page 
  + Changing the labels
  + Changing the number of shown days
  + Changing the number of days to move at once
+ Fixed a bug which left grey selected cells behind.

#### [v1.2&#946;](https://github.com/deltadak/plep/blob/master/releases/plep_v1.2B.jar?raw=true) 
+ Improved speed when executing a database query

#### [v1.1.1](https://github.com/deltadak/plep/blob/master/releases/plep_v1.1.1.jar?raw=true)
+ Fixes an important bug which moved tasks when editing on a new day
+ Fixes a bug of duplicating tasks when swapping them in the same day

#### [v1.1](https://github.com/deltadak/plep/blob/master/releases/plep_v1.1.jar?raw=true)
+ Moving forwards and backwards a week
+ Automatically refreshes on a new day

#### [v1.0](https://github.com/PHPirates/plep/blob/master/releases/plep_v1.0.jar?raw=true)
+ Tasks can have a colour
+ Tasks can be repeated each week
+ Fixed some bugs

#### [v0.2](https://github.com/PHPirates/plep/blob/master/releases/plep_v0.2.jar?raw=true)
+ A small fix which solves the issue of not creating a database in some cases

#### [v0.1](https://github.com/PHPirates/plep/blob/master/releases/plep_v0.1.jar?raw=true)
+ Dragging tasks from one day to another
+ Adding a label to each task
+ Automatically saves your tasks to a local database
+ View multiple days at once

## Instructions for building from source in IntelliJ
This application works with a database, so you need to add the sqlite library by going to project structure -> libaries -> add new one and then selecting the file located at something like C:\Users\s156757\.IntelliJIdea2016.2\config\jdbc-drivers\sqlite-jdbc-3.8.11.2.jar
To add the database to IntelliJ, go to View -> Tool Windows -> Database and add an SQLite data source. To find out the path to your database file (plep.db), look for a method in `Database.java` called `setDefaultDatabasePath()` and find out the value of `jarDir`. The database file will be in the same folder as the plep folder.

## [Javadoc](http://htmlpreview.github.io/?https://github.com/deltadak/plep/blob/master/Javadoc/index.html)

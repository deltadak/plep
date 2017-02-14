# plep
Plepping is the new planning.

Find the latest jar [here](https://github.com/PHPirates/plep/blob/master/out/artifacts/plep_jar/plep.jar?raw=true).

## User Manual
To install plep, download the latest [jar](https://github.com/PHPirates/plep/blob/master/out/artifacts/plep_jar/plep.jar?raw=true). The first time plep is runned, it will create a database file in the folder plep.jar is stored. Below we have listed a few things that might come in handy, as well as known bugs we have to live with, for now.
+ To **move** a task from one day to another, click, hold, and drag the task to the day you wish to move it to. 
  * Sometimes when you drag an item into a cell/task that is selected (dark grey) it disappears.
+ To **delete** a task, select it and press the delete button on your keyboard.
+ To **edit** or **create** a task, double-click on it to make it editable. When you you are done editing, press enter to submit.
  * Not pressing enter results in the label being removed.
  * When you click a selected cell/task, it will see this as double-click and start editing.
+ To **edit** a label, click the label you wish to edit. A drop-down menu will show from which you can select the label you want.

## Release Notes
Features of plep, listed by the version in which they were added.
#### [v0.2](plep_v0.2.jar)
+ A small fix which solves the issue of not creating a database in some cases
#### [v0.1](plep_v0.1.jar)
+ Dragging tasks from one day to another
+ Adding a label to each task
+ Automatically saves your tasks to a local database
+ View multiple days at once

## Instructions for building from source in IntelliJ
This application works with a database, so you need to add the sqlite library by going to project structure -> libaries -> add new one and then selecting the file located at something like C:\Users\s156757\.IntelliJIdea2016.2\config\jdbc-drivers\sqlite-jdbc-3.8.11.2.jar
Then you go to view -> tool windows -> database and follow the instructions to add an SQLite data source.


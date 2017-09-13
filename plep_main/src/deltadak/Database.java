package deltadak;

import javafx.scene.control.TreeView;

import java.io.File;
import java.security.CodeSource;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.LocalDate;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Class to communicate with database, it is an enum by the singleton design pattern.
 * Call methods by using Database.INSTANCE.method()
 */
// incorrect warning about LocalDate may be weakened to ChronoLocalDate (not
// true)
@SuppressWarnings("TypeMayBeWeakened")
public enum Database {
    /**
     * Implicit empty constructor.
     */
    INSTANCE;
    
    
//    private Connection connection;
//    private Statement statement;
    private String databasePath;
    private int countID = 1;
    
    /*
     * Methods that are available to the controller
     */
    
    // homework tasks --------------------------------------------------
    
    /**
     * Gets all the parent tasks on a given day.
     *
     * @param day the date for which to get all the parent tasks
     *
     * @return List<HomeworkTask>
     */
    public List<HomeworkTask> getParentTasksDay(final LocalDate day) {
        
        // convert the day to a string so we can compare it to the value in
        // the database
        String dayString = day.toString();
        String sql = "SELECT id, done, task, label, color " + "FROM tasks " +
                "WHERE day = '"
                + dayString + "' ORDER BY orderInDay";
        List<HomeworkTask> homeworkTasks = new ArrayList<>();

        Connection connection = setConnection();
        try {
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            while (resultSet.next()) {
                // create a HomeworkTask with the values from the database,
                // and add this to the List
                HomeworkTask homeworkTask = new HomeworkTask(
                                resultSet.getBoolean("done"),
                                resultSet.getString("task"),
                                resultSet.getString("label"),
                                resultSet.getString("color"),
                                resultSet.getInt("id"));
                homeworkTasks.add(homeworkTask);

            }
            statement.close();
            connection.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return homeworkTasks;
    }
    
    /**
     * Gets all the task of a given day. Parent- and subtasks.
     *
     * @param day The date for which to get all the tasks.
     * @return List<List<HomeworkTask>>
     */
    public List<List<HomeworkTask>> getTasksDay(final LocalDate day) {
        
        // create the list to eventually return
        List<List<HomeworkTask>> homeworkTasks = new ArrayList<>();
        
        // get the parent tasks of this day
        List<HomeworkTask> parentTasks = getParentTasksDay(day);
    
        // for each parent task:
        for (HomeworkTask parentTask : parentTasks) {
            // get their subtasks from the database
            String sql = "SELECT done, task FROM subtasks WHERE parentID = "
                    + parentTask.getDatabaseID();
        
            // create a list to contain the parent task and its children
            List<HomeworkTask> oneFamily = new ArrayList<>();
            // add the parent task as first item of the family
            oneFamily.add(parentTask);
        
            Connection connection = setConnection();
            try {
                Statement statement = connection.createStatement();
                ResultSet resultSet = statement.executeQuery(sql);
                while (resultSet.next()) {
                    // create a subtask with the values from the
                    // database, and add the subtask to the family
                    HomeworkTask childTask = new HomeworkTask(
                            resultSet.getBoolean("done"),
                            resultSet.getString("task"), "", "", -1);
                    oneFamily.add(childTask);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        
            homeworkTasks.add(oneFamily);
        
        }
        return homeworkTasks;
    }
    
    /**
     * updates a day in the database
     *
     * @param day date for which to update
     * @param homeworkTasks the new homeworkTasks
     */
    public void updateTasksDay(final LocalDate day, final List<List<HomeworkTask>> homeworkTasks) {
        
        // first remove all the items for this day that are currently in the
        // database before we add the new ones,
        // so we don't get double homeworkTasks
        deleteTasksDay(day);
        
        // then add the new homeworkTasks
        for (int i = 0; i < homeworkTasks.size(); i++) {
            // add the parent task to the database
            HomeworkTask parent = homeworkTasks.get(i).get(0);
            insertTask(day, parent, i);

            // add the subtasks of the parent tasks to the database
            int parentID = parent.getDatabaseID();
            for (int j = 1; j < homeworkTasks.get(i).size(); j++) {
                insertSubtask(homeworkTasks.get(i).get(j), parentID);
            }
        }
        
        deleteEmptyRows("tasks", "task");
    }
    
    /**
     * Updates the parent tasks in the database (tasks table).
     *
     * @param day The day for which to update the tasks.
     * @param parentTasks The List<HomeworkTask> with 'new' parents.
     */
    public void updateParentsDay(final LocalDate day, final List<HomeworkTask> parentTasks) {
        
        // first remove all the items for this day that are currently in the
        // database before we add the new ones,
        // so we don't get double homeworkTasks
        deleteParentTasksDay(day);
        
        // then add the new homeworkTasks
        for (int i = 0; i < parentTasks.size(); i++) {
            // add the parent task to the database
            insertTask(day, parentTasks.get(i), i);
            
        }
        deleteEmptyRows("tasks", "task");
    }
    
    /**
     * Updates the parent tasks in the database. It also copies the subtasks,
     * using
     * {@link this#insertTaskForRepeat(LocalDate, HomeworkTask, int)}
     * instead of {@link this#insertTask(LocalDate, HomeworkTask, int)}.
     *
     * @param day The day for which to update the tasks.
     * @param parentTasks The new tasks to be updated.
     */
    public void updateParentsForRepeat(LocalDate day, List<HomeworkTask> parentTasks) {
        
        // first remove all the items for this day that are currently in the
        // database before we add the new ones,
        // so we don't get double homeworkTasks
        deleteParentTasksDay(day);
    
        // then add the new homeworkTasks
        for (int i = 0; i < parentTasks.size(); i++) {
            // add the parent task to the database
            insertTaskForRepeat(day, parentTasks.get(i), i);
        }
    
        deleteEmptyRows("tasks", "task");
    }
    
    /**
     * Inserts a row in the expanded table.
     *
     * @param parentID The id of the homework task this boolean belongs to.
     * @param expanded The boolean that states if the homework task is expanded or not.
     */
    public void insertExpandedItem(int parentID, boolean expanded) {
        int expandedInt = expanded ? 1 : 0;
        String sql = "INSERT OR IGNORE INTO expanded(parentID, expanded) "
                + "VALUES(" + parentID + ", " + expandedInt + ")";
        query(sql);
    }
    
    /**
     * Update a row in the expanded table with the new boolean.
     *
     * @param parentID The id from the row to update.
     * @param expanded The new value of the boolean.
     */
    public void updateExpanded(int parentID, boolean expanded) {
        int expandedInt = expanded ? 1 : 0;
        String sql = "UPDATE expanded SET expanded = " + expandedInt
                + " WHERE parentID = " + parentID;
        query(sql);
    }
    
    /**
     * Delete a row from the expanded table.
     *
     * @param id The id of the row to delete.
     */
    public void deleteExpanded(int id) {
        String sql = "DELETE FROM expanded WHERE parentID = " + id;
        query(sql);
    }
    
    // settings ------------------------------------------------------
    
    /**
     * Gets the value of a setting from the database.
     * @param name Name of the setting to get the value from.
     * @return String with the value.
     */
    public String getSetting(String name) {
        String value = "";
        String sql = "SELECT value FROM settings where name = '" + name + "'";
        Connection connection = setConnection();
        try{
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            while(resultSet.next()) {
                value = resultSet.getString("value");
            }
            statement.close();
            connection.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return value;
    }
    
    /**
     * Updates a setting in the database.
     * @param name The name of the setting to update.
     * @param newValue The new value to update the setting with.
     */
    public void updateSetting(String name, String newValue) {
        String sql = "UPDATE settings SET value = '" + newValue +
                "' WHERE name = '" + name + "'";
        query(sql);
    }
    
    // labels ---------------------------------------------------------
    
    /**
     * Retrieves all the labels that are stored in the database, and returns
     * them as Strings in an ArrayList.
     * @return labels
     */
    public ArrayList<String> getLabels() {
        String sql = "SELECT * FROM labels ORDER BY id";
        ArrayList<String> labels = new ArrayList<>();
    
        Connection connection = setConnection();
        try {
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            while(resultSet.next()) {
//                System.out.println(resultSet.getString("label"));
                labels.add(resultSet.getString("label"));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return labels;
    }
    
    /**
     * Updates the label in the database for the given id.
     * @param id Integer with the primary key of label that has to be changed.
     * @param label String with the new label.
     */
    public void updateLabel(int id, String label) {
        removeLabel(id);
        insertLabel(id, label);
        deleteEmptyRows("labels", "label");
    }
    
    // misc ---------------------------------------------------------------
    
    /**
     * Creates all the tables in the database.
     */
    public void createTables() {
        createHomeworkTable();
        createExpandedItemstable();
        createSubtaskTable();
        createSettingsTable();
        createLabelsTable();
    }
    
    /**
     * sets the default path of the database to the directory the jar file is in
     */
    public void setDefaultDatabasePath() {
        try {
            // get the directory of the jar
            CodeSource codeSource = this.getClass().getProtectionDomain().getCodeSource();
            File jarFile = new File(codeSource.getLocation().toURI().getPath());
            String jarDir = jarFile.getParentFile().getPath();
            
            // set up the path of the database to make connection with the database
            databasePath = "jdbc:sqlite:" + jarDir + "\\plep.db";
            System.out.println(databasePath);
        } catch (Exception e) {
            e.printStackTrace();
        }
        
    }
    
    
    
    /*
     * Private methods that are not available to the Controller
     * These are methods needed by the available methods
     */
    
    // homework tasks ---------------------------------------------------
    
    /**
     * Creates table with all the tasks, if it doesn't exist yet.
     */
    private void createHomeworkTable() {
        String sql = "CREATE TABLE IF NOT EXISTS tasks(" + "id INT PRIMARY KEY, done BOOLEAN, "
                + "day DATE," + "task CHAR(255)," + "label CHAR(10),"
                + "color CHAR(50)," + "orderInDay INT)";
        query(sql);
        
    }
    
    /**
     * Creates the expanded table. This table holds ids of homework tasks
     * (the primary key in the tasks table) and their corresponding boolean
     * of their expanded state.
     */
    private void createExpandedItemstable() {
        String sql = "CREATE TABLE IF NOT EXISTS expanded(parentID INT PRIMARY KEY,"
                + " expanded BOOLEAN)";
        query(sql);
    }
    
    /**
     * inserts a homeworkTask into the database, given
     *
     * @param day
     *         - the date as a LocalDate
     * @param homeworkTask
     *         - the homeworkTask to be inserted
     * @param order
     *         - this is the i-th homeworkTask on this day, as an int
     */
    private void insertTask(final LocalDate day,
                            final HomeworkTask homeworkTask, final int order) {
        // don't add empty tasks
        if(!homeworkTask.getText().equals("")) {
            setHighestID(); // sets countID
    
            // update the parentID for all its subtasks
            updateSubtasksID(homeworkTask);
            // update the id in the expanded table
            updateExpandedID(homeworkTask);
            
            homeworkTask.setDatabaseID(countID);
    
            String dayString = day.toString();
    
            int doneInt = homeworkTask.getDone() ? 1 : 0;
    
            String sql =
                    "INSERT INTO tasks(id, done, day, task, label, color, orderInDay) "
                            
                            + "VALUES (" + countID + ", '" + doneInt + "', '" + dayString + "', '"
                            + homeworkTask.getText() + "','" + homeworkTask.getLabel() + "','"
                            + homeworkTask.getColor() + "'," + order + ")";
            countID++;
            query(sql);
        }
    }
    
    /**
     * Updates the id of a row in the expanded table with the new id from
     * its task.
     *
     * @param parentTask The task of which to update the id.
     */
    private void updateExpandedID(HomeworkTask parentTask) {
        String sql = "UPDATE expanded SET parentID = " + countID +
                " WHERE parentID = " + parentTask.getDatabaseID();
        query(sql);
    }
    
    /**
     * Gets tuples of the form (int id, boolean expanded) from the expanded
     * table.
     *
     * @return List<Map.Entry<Integer, Boolean>>
     */
    public List<Map.Entry<Integer, Boolean>> getExpanded() {
        
        List<Map.Entry<Integer, Boolean>> expandedPairs = new ArrayList<>();
        // select all the values from the expanded table
        String sql = "SELECT * FROM expanded";
        Connection connection = setConnection();
        try {
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            while (resultSet.next()) {
                int id = resultSet.getInt("parentID");
                boolean expanded = resultSet.getBoolean("expanded");
                // add a new pair with the values to the List
                expandedPairs.add(new AbstractMap.SimpleEntry<>(
                        id, expanded));
            }
            return expandedPairs;
        } catch (SQLException e) {
            e.printStackTrace();
            return expandedPairs;
        }
    }
    
    /**
     * Inserts a parent task, keeps the subtasks with the old IDs (of the to
     * be repeated task) and creates new subtasks.
     *
     * @param day The day on which the to be repeated task is repeated.
     * @param homeworkTask The task to be repeated.
     * @param order The i-th parent task on the day.
     */
    private void insertTaskForRepeat(final LocalDate day,
                            final HomeworkTask homeworkTask, final int order) {
        // don't add empty tasks
        if(!homeworkTask.getText().equals("")) {
            setHighestID(); // sets countID
            
            // get all the subtasks for the task to be repeated
            List<HomeworkTask> subtasks = getSubtasksByID(homeworkTask.getDatabaseID());
            
            // update the parent IDs of the subtasks
            updateSubtasksID(homeworkTask);
            // update the id in the expanded table
            updateExpandedID(homeworkTask);
            
            // add the subtasks with the old parentID again. We can do this
            // because the id of the task that is repeated does not change.
            for (HomeworkTask subtask : subtasks) {
                insertSubtask(subtask, homeworkTask.getDatabaseID());
            }
    
            homeworkTask.setDatabaseID(countID);
            
            String dayString = day.toString();
            
            int doneInt = homeworkTask.getDone() ? 1 : 0;
            
            String sql =
                    "INSERT INTO tasks(id, done, day, task, label, color, orderInDay) "
                            
                            + "VALUES (" + countID + ", '" + doneInt + "', '" + dayString + "', '"
                            + homeworkTask.getText() + "','" + homeworkTask.getLabel() + "','"
                            + homeworkTask.getColor() + "'," + order + ")";
            countID++;
            query(sql);
        }
    }
    
    /**
     * Sets countID to the highest ID that's currently in the database.
     * To prevent double IDs
     */
    private void setHighestID() {
        String sql = "SELECT * FROM tasks ORDER BY id DESC";
    
        Connection connection = setConnection();
        try {
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            
            if (resultSet.isBeforeFirst()) {
                // if the database is not empty, we set the id to be the
                // highest + 1
                countID = resultSet.getInt("id") + 1;
            } else {
                // if the database is empty we set the id to 1
                countID = 1;
            }
            statement.close();
            connection.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    /**
     * Delete a task from the database given its id
     *
     * @param id
     *         - id of a task (primary key in the database)
     */
    private void deleteTask(final int id) {
        String sql = "DELETE FROM tasks WHERE id = " + id;
        query(sql);
    }
    
    /**
     * Deletes all tasks from the database for a given day.
     *
     * @param day
     *         - the day of which all tasks have to be deleted. Calendar
     *         object.
     */
    private void deleteTasksDay(final LocalDate day) {
        
        String getIDs = "SELECT id FROM tasks WHERE day = '" + day + "'";
        ArrayList<Integer> parentIDs = new ArrayList<>();

        Connection connection = setConnection();
        try {
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(getIDs);
            while(resultSet.next()) {
                parentIDs.add(resultSet.getInt("id"));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        // delete child tasks so we don't get double ones
        for (Integer parentID : parentIDs) {
            String querySubTasks = "DELETE FROM subtasks WHERE parentID = " +
                    parentID;
            query(querySubTasks);
        }
        
        deleteParentTasksDay(day); // delete the parent tasks
        
        
    }
    
    /**
     * Deletes the parent tasks of the given day.
     *
     * @param day The day of which to delete the tasks.
     */
    private void deleteParentTasksDay(final LocalDate day) {
        
        // delete the parent tasks
        String sql = "DELETE FROM tasks WHERE day = '" + day + "'";
        query(sql);
        
    }
    
    // subtasks -------------------------------------------------------------
    
    /**
     * Creates the subtasks table in the database.
     */
    private void createSubtaskTable() {
        String sql = "CREATE TABLE IF NOT EXISTS subtasks(" + "parentID INT, done BOOLEAN, "
                + "task CHAR(255))";
        query(sql);
    }
    
    /**
     * Inserts a subtask into the database, given the id of its parent.
     *
     * @param subtask HomeworkTask to insert.
     * @param parentID id of the parent task.
     */
    public void insertSubtask(final HomeworkTask subtask, final int parentID) {
        // check if the task is not empty, because then it shouldn't
        // be in the database
        if(!subtask.getText().equals("") && (parentID != -1)) {
            int doneInt = subtask.getDone() ? 1 : 0;
            String sql = "INSERT INTO subtasks(parentID, done, task) VALUES (" +
                    parentID + ", " + doneInt + ", '" + subtask.getText() + "')";
            query(sql);
        }
    }
    
    /**
     * Update all the ids of the subtasks when the id of their parent task
     * has changed.
     * NOTE: We have to call this method ourselves.
     *
     * @param parentTask The parent task of which the id has changed.
     */
    private void updateSubtasksID(HomeworkTask parentTask) {
        String sql = "UPDATE subtasks SET parentID = " + countID +
                " WHERE parentID = " + parentTask.getDatabaseID();
        query(sql);
    }
    
    /**
     * Gets the subtasks belonging to the HomeworkTask with the given id.
     *
     * @param parentID The id of the HomeworkTask.
     * @return List<HomeworkTask> with subtasks.
     */
    private List<HomeworkTask> getSubtasksByID(int parentID) {
        
        String sql = "SELECT done, task FROM subtasks WHERE parentID = " +
                parentID;
        
        List<HomeworkTask> subtasks = new ArrayList<>();
        
        Connection connection = setConnection();
        try {
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            while(resultSet.next()) {
                HomeworkTask subtask = new HomeworkTask(
                        resultSet.getBoolean("done"),
                        resultSet.getString("task"),
                        "", "", -1);
                subtasks.add(subtask);
            }
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return subtasks;
    }
    
    // settings -------------------------------------------------------------
    
    /**
     * Creates a table with settings and populates it with default settings.
     * The settings table contains
     *      - the name of the setting as primary key,
     *      - the value of the setting as a string.
     */
    private void createSettingsTable() {
        String sql = "CREATE TABLE IF NOT EXISTS settings("
                + "name CHAR(50) PRIMARY KEY, "
                + "value CHAR(50))";
        query(sql);
        
        // insert the default settings
        insertSetting("number_of_days", "9");
        insertSetting("number_of_moving_days", "7");
        insertSetting("max_columns", "3");
        insertSetting("max_columns_auto", "true");

    }
    
    /**
     * Inserts a setting with given name and value into the settings table.
     * Only insert it when there is no row with the same name in the table.
     * @param name Name of the setting.
     * @param value Value of the setting, as a string.
     */
    private void insertSetting(String name, String value) {
        String sql = "INSERT OR IGNORE INTO settings(name, value) VALUES ('"
                + name + "', '" + value + "')";
        query(sql);
    }
    
    // labels ---------------------------------------------------------------
    
    /**
     * Creates the table in the database to hold the labels, if it doesn't
     * exist yet.
     */
    private void createLabelsTable() {
        String sql = "CREATE TABLE IF NOT EXISTS labels(id INT PRIMARY KEY, "
                + "label CHAR(10))";
        query(sql);
    }
    /**
     * Inserts a new label into the database, used by
     * {@link Database#updateLabel(int, String)}
     * @param id Integer with the primary key of label that has to be removed.
     * @param label String with the new label.
     */
    private void insertLabel(int id, String label) {
        String sql = "INSERT INTO labels(id, label)"
                + "VALUES (" + id + ", '" + label + "')";
        query(sql);
    }
    
    /**
     * Remove a label from the database, used by
     * {@link Database#updateLabel(int, String)}
     * @param id Integer with primary key of label that has to be deleted.
     */
    private void removeLabel(int id) {
        String sql = "DELETE FROM labels WHERE id = " + id;
        query(sql);
    }
    
    // misc -------------------------------------------------------------
    
    /**
     * create a connection with the database
     *
     * @return Connection
     */
    private Connection setConnection() {
        try {
            Class.forName("org.sqlite.JDBC");
            Connection connection = DriverManager.getConnection(databasePath);
            return connection;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
    
    /**
     * deletes all the tasks from the database where field task is empty
     * used when updating a day of which an item has been removed (by dragging)
     * @param tableName String with the name of the table from which to
     *                  remove the empty rows.
     * @param conditionColumn String (name) of the column on which to check for
     *                        empty rows.
     */
    private void deleteEmptyRows(String tableName, String conditionColumn) {
        String sql = "DELETE FROM " + tableName
                + " WHERE "+ conditionColumn + " = '' ";
        query(sql);
    }
    
    /**
     * Delete a given table from the database.
     *
     * @param tableName
     *         - name of the table that has to be deleted
     */
    private void deleteTable(final String tableName) {
        String sql = "DROP TABLE IF EXISTS " + tableName;
        query(sql);
    }
    
    /**
     * Sends query to the database. Don't use this when selecting data from the
     * database. (No method for that because we need to do something with the
     * data in the try-block).
     *
     * @param sql
     *         - string with the sql query
     */
    private void query(final String sql) {
        Connection connection = setConnection();
        try {
            Statement statement = connection.createStatement();
            statement.executeUpdate(sql);
            statement.close();
            connection.close();
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    //    /**
    //     * used to change the directory of the database
    //     * not used yet because we only set default database
    //     */
    //    private void changeDirectory() {
    //        Dialog chooseDialog = new Dialog();
    //        chooseDialog.setHeight(100);
    //        chooseDialog.setWidth(300);
    //        //            chooseDialog.setResizable(true);
    //        chooseDialog.setTitle("Decisions!");
    //
    //        GridPane grid = new GridPane();
    //        grid.setPrefHeight(chooseDialog.getHeight());
    //        grid.setPrefWidth(chooseDialog.getWidth());
    //
    //        Button browseButton = new Button("Browse");
    //        Text text = new Text("Choose database directory...");
    //
    //        ButtonType browseButtonType = new ButtonType("OK",
    //                                                     ButtonBar.ButtonData.OK_DONE);
    //        chooseDialog.getDialogPane().getButtonTypes().add(browseButtonType);
    //        chooseDialog.getDialogPane().lookupButton(browseButtonType).setDisable(true);
    //
    //        browseButton.setOnMouseClicked(event -> {
    //
    //            System.out.println("button clicked");
    //            DirectoryChooser directoryChooser = new DirectoryChooser();
    //            directoryChooser.setTitle("Choose Directory");
    //            File directory = directoryChooser.showDialog(new Stage());
    //            String databaseDirectory = directory.getAbsolutePath();
    //            text.setText(databaseDirectory);
    //            databasePath = "jdbc:sqlite:";
    //            databasePath += databaseDirectory + "\\plep.db";
    //            System.out.println(databasePath);
    //            chooseDialog.getDialogPane().lookupButton(browseButtonType).setDisable(false);
    //
    //        });
    //
    //
    //        grid.add(browseButton,0,1);
    //        grid.add(text,0,0);
    //        chooseDialog.getDialogPane().setContent(grid);
    //
    //        chooseDialog.showAndWait();
    //    }
}
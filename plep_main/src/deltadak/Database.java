package deltadak;

import com.sun.org.apache.xpath.internal.operations.Bool;

import java.io.File;
import java.security.CodeSource;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

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
     * Gets all the tasks on a given day.
     *
     * @param day the date for which to get all the tasks
     *
     * @return List<HomeworkTask>
     */
    public List<HomeworkTask> getTasksDay(final LocalDate day) {
        
        String dayString = day.toString();
        String sql = "SELECT done, task, label, color " + "FROM tasks " + "WHERE day = '"
                + dayString + "' ORDER BY orderInDay";
        List<HomeworkTask> homeworkTasks = new ArrayList<>();
    
        Connection connection = setConnection();
        try {
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            List<HomeworkTask> tasks = new ArrayList<>();
            while (resultSet.next()) {
                
                HomeworkTask homeworkTask = new HomeworkTask(
                                resultSet.getBoolean("done"),
                                resultSet.getString("task"),
                                resultSet.getString("label"),
                                resultSet.getString("color"));
//                tasks.add(homeworkTask);
                homeworkTasks.add(homeworkTask);
//                homeworkTasks.add(
//                        new HomeworkTask(
//                                done,
//                                resultSet.getString("task"),
//                                resultSet.getString("label"),
//                                resultSet.getString("color")));
            
            }
            statement.close();
            // don't close the connection, otherwise we get
            // SQLException: Database has been closed
            // when trying getLabels()... Even though that has a
            // setConnection()...
            connection.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return homeworkTasks;
    }
    
    /**
     * updates a day in the database
     *
     * @param day date for which to update
     * @param homeworkTasks List<HomeworkTask> with the new homeworkTasks
     */
    public void updateTasksDay(final LocalDate day, final List<HomeworkTask> homeworkTasks) {
        
        // first remove all the items for this day that are currently in the
        // database before we add the new ones,
        // so we don't get double homeworkTasks
        deleteTasksDay(day);
        
        // then add the new homeworkTasks
        for (int i = 0; i < homeworkTasks.size(); i++) {
            insertTask(day, homeworkTasks.get(i), i);
        }
        
        deleteEmptyRows("tasks", "task");
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
            // don't close the connection, otherwise we get
            // SQLException: Database has been closed
            // when trying getLabels()... Even though that has a
            // setConnection()...
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
        setHighestID(); // sets countID
        
        String dayString = day.toString();
        
        int doneInt = homeworkTask.getDone() ? 1 : 0;
        
        String sql = "INSERT INTO tasks(id, done, day, task, label, color, orderInDay) "
                + "VALUES (" + countID + ", '" + doneInt + "', '" + dayString +
                "', '"
                + homeworkTask.getText() + "','" + homeworkTask.getLabel() + "','"
                + homeworkTask.getColor() + "'," + order + ")";
        countID++;
        query(sql);
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
            // don't close the connection, otherwise we get
            // SQLException: Database has been closed
            // when trying getLabels()... Even though that has a
            // setConnection()...
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
        
        String sql = "DELETE FROM tasks WHERE day = '" + day + "'";
        query(sql);
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
            // don't close the connection, otherwise we get
            // SQLException: Database has been closed
            // when trying getLabels()... Even though that has a
            // setConnection()...
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
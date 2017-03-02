package deltadak;

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
    
    
    private Connection connection;
    private Statement statement;
    private String databasePath;
    private int countID = 1;
    
    /*
     * Methods that are available to the controller
     */
    
    /**
     * Creates table with all the tasks, if it doesn't exist yet.
     */
    public void createTable() {
        String sql = "CREATE TABLE IF NOT EXISTS tasks(" + "id INT PRIMARY KEY,"
                + "day DATE," + "task CHAR(255)," + "label CHAR(10),"
                + "color CHAR(50)," + "orderInDay INT)";
        query(sql);
        
    }
    
    /**
     * Gets all the tasks on a given day.
     *
     * @param day
     *         - the date for which to get all the tasks
     *
     * @return List<Task>
     */
    public List<Task> getTasksDay(final LocalDate day) {
        
        String dayString = day.toString();
        String sql = "SELECT task, label, color " + "FROM tasks " + "WHERE day = '"
                + dayString + "' ORDER BY orderInDay";
        List<Task> tasks = new ArrayList<>();
        
        setConnection();
        try {
            statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            while (resultSet.next()) {
                tasks.add(new Task(resultSet.getString("task"),
                                   resultSet.getString("label"),
                                   resultSet.getString("color")));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return tasks;
    }
    
    /**
     * updates a day in the database
     *
     * @param day
     *         - date for which to update
     * @param tasks
     *         - List<Task> with the new tasks
     */
    public void updateTasksDay(final LocalDate day, final List<Task> tasks) {
        
        System.out.println("updateTasksDay " + day);
        
        // first remove all the items for this day that are currently in the
        // database before we add the new ones,
        // so we don't get double tasks
        deleteTasksDay(day);
        
        // then add the new tasks
        for (int i = 0; i < tasks.size(); i++) {
            insertTask(day, tasks.get(i), i);
        }
        
        deleteEmptyRows("tasks", "task");
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
    
    public void createLabelsTable() {
        String sql = "CREATE TABLE IF NOT EXISTS labels(id INT PRIMARY KEY, "
                + "label CHAR(10))";
        query(sql);
    }
    
    public ArrayList<String> getLabels() {
        String sql = "SELECT * FROM labels ORDER BY id";
        ArrayList<String> labels = new ArrayList<>();
        
        setConnection();
        try {
            statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            System.out.println("resultsss");
//            System.out.println(resultSet.next());
            while(resultSet.next()) {
                System.out.println("resultset");
                labels.add(resultSet.getString("label"));
                System.out.println(resultSet.getString("label"));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return labels;
    }
    
    public void updateLabel(int id, String label) {
        removeLabel(id);
        insertLabel(id, label);
        deleteEmptyRows("labels", "label");
    }
    
    private void insertLabel(int id, String label) {
        String sql = "INSERT INTO labels(id, label)"
                + "VALUES (" + id + ", '" + label + "')";
        query(sql);
    }
    
    private void removeLabel(int id) {
        String sql = "DELETE FROM labels WHERE id = " + id;
        query(sql);
    }
    
    /*
     * Private methods that are not available to the Controller
     * These are methods needed by the available methods
     */
    
    /**
     * create a connection with the database
     */
    private void setConnection() {
        try {
            Class.forName("org.sqlite.JDBC");
            connection = DriverManager.getConnection(databasePath);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    /**
     * inserts a task into the database, given
     *
     * @param day
     *         - the date as a LocalDate
     * @param task
     *         - the task to be inserted
     * @param order
     *         - this is the i-th task on this day, as an int
     */
    private void insertTask(final LocalDate day,
                            final Task task, final int order) {
        setHighestID(); // sets countID
        
        String dayString = day.toString();
        
        String sql = "INSERT INTO tasks(id, day, task, label, color, orderInDay) "
                + "VALUES (" + countID + ", '" + dayString + "', '" + task.getText()
                + "','" + task.getLabel() + "','" + task.getColor() + "'," + order + ")";
        countID++;
        query(sql);
    }
    
    /**
     * Sets countID to the highest ID that's currently in the database.
     * To prevent double IDs
     */
    private void setHighestID() {
        String sql = "SELECT * FROM tasks ORDER BY id DESC";
        
        setConnection();
        try {
            statement = connection.createStatement();
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
     * deletes all the tasks from the database where field task is empty
     * used when updating a day of which an item has been removed (by dragging)
     */
    private void deleteEmptyRows(String tableName, String conditionColumn) {
        String sql = "DELETE FROM " + tableName
                + " WHERE "+ conditionColumn + " = '' ";
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
        setConnection();
        try {
            statement = connection.createStatement();
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

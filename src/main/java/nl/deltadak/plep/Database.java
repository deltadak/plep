package nl.deltadak.plep;

import nl.deltadak.plep.database.tables.SubTasks;

import java.io.File;
import java.security.CodeSource;
import java.sql.*;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * Class to communicate with database, it is an enum by the singleton design
 * pattern. Call methods by using Database.INSTANCE.method()
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
     * @param day
     *         the date for which to get all the parent tasks
     *
     * @return List<HomeworkTask>
     */
    public List<HomeworkTask> getParentTasksDay(final LocalDate day) {
        
        // convert the day to a string so we can compare it to the value in
        // the database
        String dayString = day.toString();
        String sql = "SELECT id, done, task, label, expanded, color " + "FROM "
                + "tasks" + " " + "WHERE day = '" + dayString
                + "' ORDER BY orderInDay";
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
                        resultSet.getString("label"), resultSet.getInt("color"),
                        resultSet.getBoolean("expanded"),
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
     * @param day
     *         The date for which to get all the tasks.
     *
     * @return List<List<HomeworkTask>>
     */
    public synchronized List<List<HomeworkTask>> getTasksDay(final LocalDate day) {
        
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
                            resultSet.getString("task"));
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
     * Deletes a task and its subtasks from the database for a given its id.
     *
     * @param id
     *         the id of the task to be deleted.
     */
    public void deleteByID(int id) {
        deleteTask(id);
        SubTasks.INSTANCE.delete(id);
    }
    
    /**
     * Updates a day in the database. Is synchronized because multiple threads can want to call this.
     *
     * @param day
     *         date for which to updateOrInsert
     * @param homeworkTasks
     *         the new homeworkTasks
     */
    public synchronized void updateTasksDay(final LocalDate day,
                               final List<List<HomeworkTask>> homeworkTasks) {
        
        // updateOrInsert or insert the homework tasks
        for (int i = 0; i < homeworkTasks.size(); i++) {
            // get the parent task from the list/matrix of tasks
            HomeworkTask parent = homeworkTasks.get(i).get(0);
            // updateOrInsert or insert the parent task to the database
            insertOrUpdateTask(day, parent, i);
            
            int parentID = parent.getDatabaseID();
            // first remove all the old subtasks of this task
            SubTasks.INSTANCE.delete(parentID);
            // add the updated subtasks
            for (int j = 1; j < homeworkTasks.get(i).size(); j++) {
                SubTasks.INSTANCE.insert(homeworkTasks.get(i).get(j), parentID);
            }
        }
        
        deleteEmptyRows("tasks", "task");
    }
    
    /**
     * Updates the parent tasks in the database (tasks table).
     *
     * @param day
     *         The day for which to updateOrInsert the tasks.
     * @param parentTasks
     *         The List<HomeworkTask> with 'new' parents.
     */
    public synchronized void updateParentsDay(final LocalDate day,
                                 final List<HomeworkTask> parentTasks) {
        
        // insert or updateOrInsert the parent tasks in the database
        for (int i = 0; i < parentTasks.size(); i++) {
            // add the parent task to the database
            insertOrUpdateTask(day, parentTasks.get(i), i);
        }
        deleteEmptyRows("tasks", "task");
    }
    
    /**
     * Copies a homework task including subtasks to a new day.
     *
     * @param day
     *         The new day to be copied to
     * @param taskToBeCopied
     *         The homeworktask to be copied.
     */
    public void copyAndInsertTask(LocalDate day, HomeworkTask taskToBeCopied) {
        // get the subtasks of the task to be copied
        List<HomeworkTask> subtasks = SubTasks.INSTANCE.get(taskToBeCopied.getDatabaseID());
        // copy the task to a new task
        HomeworkTask newTask = taskToBeCopied;
        // give the new task its own id
        newTask.setDatabaseID(getHighestID());
        
        // insert the new task in the database
        insertOrUpdateTask(day, newTask, getHighestOrder(day));
        // insert the subtasks in the database, as subtasks of the new task
        for (HomeworkTask subtask : subtasks) {
            SubTasks.INSTANCE.insert(subtask, newTask.getDatabaseID());
        }
    }
    
    /**
     * Collects all the orders in a day and returns an int that is bigger than
     * the maximum.
     *
     * @param day
     *         The day of which to find the highest order.
     *
     * @return (The highest order that's currently in a day.) + 1
     */
    private int getHighestOrder(LocalDate day) {
        String dayString = day.toString();
        
        String sql = "SELECT orderInDay FROM tasks WHERE day == '" + dayString
                + "' ORDER BY orderInDay DESC";
        
        int order = 0;
        Connection connection = setConnection();
        try {
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            
            if (resultSet.isBeforeFirst()) {
                // if the day is not empty, we set the order to be the
                // highest + 1
                order = resultSet.getInt("orderInDay") + 1;
            }
            // otherwise we return 0 as the default order in a day
            statement.close();
            connection.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        return order;
    }

    // misc ---------------------------------------------------------------
    
    /**
     * Creates all the tables in the database.
     */
    public void createTables() {
        createHomeworkTable();
    }
    
    /**
     * sets the default path of the database to the directory the jar file is
     * in
     */
    public void setDefaultDatabasePath() {
        try {
            // get the directory of the jar
            CodeSource codeSource = this.getClass().getProtectionDomain()
                    .getCodeSource();
            File jarFile = new File(codeSource.getLocation().toURI().getPath());
            String jarDir = jarFile.getParentFile().getPath();
            
            // set up the path of the database to make connection with the
            // database
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
        String sql = "CREATE TABLE IF NOT EXISTS tasks("
                + "id INT PRIMARY KEY, done BOOLEAN, " + "day DATE,"
                + "task CHAR(255)," + "label CHAR(10),"
                + "color INT, expanded BOOLEAN, " + "orderInDay INT)";
        query(sql);
        
    }
    
    /**
     * inserts or updates a homeworkTask into the database, given
     *
     * @param day
     *         - the date as a LocalDate
     * @param homeworkTask
     *         - the homeworkTask to be inserted
     * @param order
     *         - this is the i-th homeworkTask on this day, as an int
     */
    public void insertOrUpdateTask(final LocalDate day,
                                   final HomeworkTask homeworkTask,
                                   final int order) {
        
        // convert the day to a string
        String dayString = day.toString();
        // convert the done boolean to an int
        int doneInt = homeworkTask.getDone() ? 1 : 0;
        int expandedInt = homeworkTask.getExpanded() ? 1 : 0;
        
        if (!homeworkTask.getText().equals("")) {
            // only put the task in the database if it isn't empy
            // !! this means that deleting/clearing the text of an item does
            //    nothing
            
            if (homeworkTask.getDatabaseID() == -1) {
                // if the database id of homeworkTask is currently -1, that
                // means that it first was an empty task and that it's not in
                // the database yet, so it has to get an id
                
                // set the new id of homeworkTask to the number above the
                // currently highest id in the database
                homeworkTask.setDatabaseID(getHighestID());
            }
            
            // updateOrInsert the item in the database
            // REPLACE INTO updates an item if there already is an item with
            // that id, otherwise it inserts it
            String sql = "REPLACE INTO tasks(id, done, day, task, label, "
                    + "color, expanded, orderInDay) "
                    
                    + "VALUES (" + homeworkTask.getDatabaseID() + ", '"
                    + doneInt + "', '" + dayString + "', '" + homeworkTask
                    .getText() + "','" + homeworkTask.getLabel() + "', "
                    + homeworkTask.getColorID() + ", " + expandedInt + ", "
                    + order + ")";
            query(sql);
            
        }
    }
    
    /**
     * Gets all the ids from the database, and returns (the highest id) + 1
     *
     * @return int - (highest id currently in the database) + 1
     */
    private int getHighestID() {
        String sql = "SELECT * FROM tasks ORDER BY id DESC";
        
        int id = 1;
        Connection connection = setConnection();
        try {
            Statement statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            
            if (resultSet.isBeforeFirst()) {
                // if the database is not empty, we set the id to be the
                // highest + 1
                id = resultSet.getInt("id") + 1;
            }
            statement.close();
            connection.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        return id;
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
            while (resultSet.next()) {
                parentIDs.add(resultSet.getInt("id"));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        
        // delete child tasks so we don't get double ones
        for (Integer parentID : parentIDs) {
            String querySubTasks = "DELETE FROM subtasks WHERE parentID = "
                    + parentID;
            query(querySubTasks);
        }
        
        deleteParentTasksDay(day); // delete the parent tasks
        
    }
    
    /**
     * Deletes the parent tasks of the given day.
     *
     * @param day
     *         The day of which to delete the tasks.
     */
    private void deleteParentTasksDay(final LocalDate day) {
        
        // delete the parent tasks
        String sql = "DELETE FROM tasks WHERE day = '" + day + "'";
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
            if (databasePath != null) {
//                Class.forName("org.h2.Driver");
                Class.forName("org.sqlite.JDBC");
                return DriverManager.getConnection(databasePath);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }
    
    /**
     * deletes all the tasks from the database where field task is empty used
     * when updating a day of which an item has been removed (by dragging)
     *
     * @param tableName
     *         String with the name of the table from which to remove the empty
     *         rows.
     * @param conditionColumn
     *         String (name) of the column on which to check for empty rows.
     */
    private void deleteEmptyRows(String tableName, String conditionColumn) {
        String sql = "DELETE FROM " + tableName + " WHERE " + conditionColumn
                + " = '' ";
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
            if (connection != null) {
                Statement statement = connection.createStatement();
                statement.executeUpdate(sql);
                statement.close();
                connection.close();
            }
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
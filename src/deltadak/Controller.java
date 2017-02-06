package deltadak;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.control.cell.TextFieldListCell;
import javafx.scene.input.*;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.util.Callback;
import javafx.util.StringConverter;
import javafx.util.converter.DefaultStringConverter;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.net.URL;
import java.util.ResourceBundle;

import jdk.nashorn.internal.runtime.ECMAException;

import java.net.URL;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.ResourceBundle;

import static java.lang.Integer.min;

public class Controller implements Initializable {
    @FXML ListView<Task> day1;
    @FXML ListView<Task> day2;
    @FXML GridPane gridpane;
    private DataFormat dataFormat = new DataFormat("com.deltadak.Task");

    Connection connection;
    Statement statement;
    int countID = 1;
    ArrayList<String[]> dayOne = new ArrayList<>(); // containing "task", "label"
    ArrayList<String[]> dayTwo = new ArrayList<>();
    Calendar dayOneCal;
    Calendar dayTwoCal;


    /**
     * Initialization method for the controller.
     */
    @FXML public void initialize(URL location, ResourceBundle resourceBundle){

//        deleteTable("tasks");
//        createTable();

        Calendar calendar = Calendar.getInstance();
        insertTask(calendar, "exam1", "2WA60",1);
        insertTask(calendar, "exam2", "2WA60",2);
        insertTask(calendar, "exam3", "2WA30",3);
        insertTask(calendar, "exam4", "2WA30",4);
//        deleteTasksDay(calendar);

        Calendar dayTwoCal = Calendar.getInstance();
        dayTwoCal.add(Calendar.DAY_OF_MONTH,1);
        insertTask(dayTwoCal, "one", "2WA60",1);
        insertTask(dayTwoCal, "two", "2WA60",2);
        insertTask(dayTwoCal, "three", "2WA30",3);
        insertTask(dayTwoCal, "boom", "2WA30",4);

        /**
         * method demonstration! Yay!
         */

        // get todays task
        ArrayList<String[]> todayTasks = getTasksDay(calendar);

        for (int i = 0; i < todayTasks.size(); i++) {
            System.out.println(todayTasks.get(i)[1] + " --- " + todayTasks.get(i)[0]);
        }

        System.out.println();
        // change one entry
        todayTasks.set(3, new String[]{"do nothing, yet", "2WF50"});

        for (int i = 0; i < todayTasks.size(); i++) {
            System.out.println(todayTasks.get(i)[1] + " --- " + todayTasks.get(i)[0]);
        }

        // store new values in database
        putTasksDay(calendar, todayTasks);

        setupGridPane();

    }

    /**
     * database communication below -----------------------------------------------------
     */

    /**
     * inserts a task into the database, given
     * @param dayCal - the date as a Calendar
     * @param task - the task as a string
     * @param label - the label/course (code) as a string
     * @param order - this is the i-th task on this day, as an int
     */
    public void insertTask(Calendar dayCal, String task, String label, int order) {
        getHighestID();

        String dayString = calendarToString(dayCal);

        String sql = "INSERT INTO tasks(id, day, task, label, orderInDay) " +
                    "VALUES (" + countID + ", '" + dayString + "', '" + task + "','" + label + "'," + order + ")";
        countID++;
        query(sql);
    }

    /**
     * Gets all the tasks on a given day.
     *
     * @param dayCal - the date for which to get all the tasks
     * @return ArrayList<String[]> , where String[] is of size 2. A task and a label.
     */
    public ArrayList<String[]> getTasksDay(Calendar dayCal) {

        String dayString = calendarToString(dayCal);
        String sql = "SELECT task, label FROM tasks WHERE day = '" + dayString + "' ORDER BY orderInDay";
        ArrayList<String[]> tasksDay = new ArrayList<>();

        setConnection();
        try {
            statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            while(resultSet.next()) {
                String[] task = {resultSet.getString("task"), resultSet.getString("label")};
                tasksDay.add(task);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return tasksDay;
    }

    /**
     * updates a day in the database
     * @param dayCal - date for which to update
     * @param tasks - ArrayList<String[]> with the new tasks
     */
    public void putTasksDay(Calendar dayCal, ArrayList<String[]> tasks) {

        // first remove all the items for this day that are currently in the database before we add the new ones,
        // so we don't get double tasks
        deleteTasksDay(dayCal);

        // then add the new tasks
        for (int i = 0; i < tasks.size(); i++) {
            insertTask(dayCal, tasks.get(i)[0], tasks.get(i)[1], i);
        }

    }

    /**
     * Sets countID to the highest ID that's currently in the database.
     * To prevent double IDs
     */
    public void getHighestID() {
        String sql = "SELECT * FROM tasks ORDER BY id DESC";

        setConnection();
        try {
            statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            if(resultSet.isBeforeFirst()) {
                countID = resultSet.getInt("id") + 1;
            } else {
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
     * @param id - id of a task (primary key in the database)
     */
    public void deleteTask(int id) {
        String sql = "DELETE FROM tasks WHERE id = " + id;
        query(sql);
    }

    /**
     * Deletes all tasks from the database for a given day.
     * @param day - the day of which all tasks have to be deleted. Calendar object.
     */
    public void deleteTasksDay(Calendar day) {

        String dayString = calendarToString(day);
        String sql = "DELETE FROM tasks WHERE day = '" + dayString + "'";
        query(sql);
    }

    /**
     * Creates table with all the tasks, if it doesn't exist yet.
     */
    public void createTable() {
        String sql = "CREATE TABLE IF NOT EXISTS tasks(" +
                "id INT PRIMARY KEY," +
                "day DATE," +
                "task CHAR(255)," +
                "label CHAR(10)," +
                "orderInDay INT)";
        query(sql);
    }

    /**
     * Delete a given table from the database.
     * @param tableName
     */
    public void deleteTable(String tableName) {
        String sql = "DROP TABLE IF EXISTS " + tableName;
        query(sql);
    }

    /**
     * Sends query to the database.
     * Don't use this when selecting data from the database.
     *      (No method for that because we need to do something with the data in the try-block).
     * @param sql
     */
    public void query(String sql) {
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

    /**
     * Connect to the database.
     * Use this before sending a query to the database.
     */
    public void setConnection() {
        try {
            Class.forName("org.sqlite.JDBC");
            connection = DriverManager.getConnection("jdbc:sqlite:plep.db");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }


    /**
     * Converts Calendar object to String object.
     * @param calendar
     * @return String with eg 2017-03-25
     */
    public String calendarToString(Calendar calendar) {
        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
        String calendarString = format.format(calendar.getTime());
        return calendarString;
    }


    /**
     * sets up listviews for each day, initializes drag and drop, editing items
     */
    private void setupGridPane() {

        //some debug defaults

        // convert ArrayList<String[]> to ArrayList<String>, for now
        ArrayList<String> temp = new ArrayList<>();
        ArrayList<String[]> dayOne = getTasksDay(Calendar.getInstance());
        for (int i = 0; i < dayOne.size(); i++) {
            temp.add(dayOne.get(i)[0]);
        }
        ObservableList<String> day1List = FXCollections.observableArrayList(temp);

        temp.clear();
        Calendar cal = Calendar.getInstance();
        cal.add(Calendar.DAY_OF_MONTH,1);
        ArrayList<String[]> dayTwo = getTasksDay(cal);
        for (int i = 0; i < dayTwo.size(); i++) {
            temp.add(dayTwo.get(i)[0]);
        }
        ObservableList<String> day2List = FXCollections.observableArrayList(temp);
        ObservableList<Task> day1Tasks = FXCollections.observableArrayList(
                new Task("gdv","2WA70"),
                new Task("methods","2IPC0")
        );
        ObservableList<Task> day2Tasks = FXCollections.observableArrayList(
                new Task("discrete","2WF50"),
                new Task("robot","0LAUK0")
        );
        day1.setItems(day1Tasks);
        day2.setItems(day2Tasks);

        //setup drag and drop for all children of gridview
        gridpane.getChildren().stream().filter(node -> node instanceof ListView).forEach(node -> {
            ListView<Task> list = (ListView<Task>) node;
            setupListView(list);
            list.setOnEditCommit(t -> list.getItems().set(t.getIndex(), t.getNewValue()));
            cleanUp(list);
        });
    }

    private void setupListView(ListView<Task> list) {
        //no idea why the callback needs a ListCell and not a TextFieldListCell
        //anyway, editing is enabled by using TextFieldListCell instead of ListCell
        list.setCellFactory(new Callback<ListView<Task>, ListCell<Task>>() {
            @Override
            public LabelCell call(ListView<Task> param) {
                LabelCell labelCell = new LabelCell() {};

                //update text on changes
                labelCell.setConverter(new TaskConverter(labelCell));

                // update label on changes
                labelCell.comboBox.valueProperty().addListener(new ChangeListener<String>() {
                    @Override
                    public void changed(ObservableValue<? extends String> observable, String oldValue, String newValue) {
                        labelCell.getItem().setLabel(newValue);
                    }
                });

                labelCell.setOnDragDetected( (MouseEvent event) -> {
                    if (!labelCell.getItem().getText().equals("")) {
                        Dragboard db = labelCell.startDragAndDrop(TransferMode.MOVE);
                        ClipboardContent content = new ClipboardContent();
                        content.put(dataFormat,labelCell.getItem());
                        db.setContent(content);
                    }
                    event.consume();
                });

                labelCell.setOnDragOver(event -> {
                    if (event.getGestureSource() != labelCell && event.getDragboard().hasContent(dataFormat)) {
                        event.acceptTransferModes(TransferMode.MOVE);
                    }
                    event.consume();
                });

                labelCell.setOnDragEntered(event -> {
                    if (event.getGestureSource() != labelCell && event.getDragboard().hasContent(dataFormat)) {
                        System.out.println("TODO: change color of listview"); //todo
                    }
                    event.consume();
                });

                labelCell.setOnDragExited(event -> {
                    System.out.println("TODO reset color of listview"); //todo
                    event.consume();
                });

                labelCell.setOnDragDropped(event -> {
                    Dragboard db = event.getDragboard();
                    boolean success = false;
                    if (db.hasContent(dataFormat)) {
                        Task newTask = (Task) db.getContent(dataFormat);
                        //insert new task, removing will happen in onDragDone
                        int index = min(labelCell.getIndex(),list.getItems().size()); // item can be dropped way below the existing list
                        //we have put an empty item instead of no items
                        //because otherwise there are no listCells that can receive an item
                        if (list.getItems().get(index).getText().equals("")) {
                            list.getItems().set(index,newTask); //replace empty item
                        } else {
                            list.getItems().add(index, newTask);
                        }
                        success = true;
                    }
                    event.setDropCompleted(success);
                    event.consume();
                    cleanUp(list);
                });

                labelCell.setOnDragDone(event -> {
                    //ensures the original element is only removed on a valid copy transfer (no dropping outside listviews)
                    if (event.getTransferMode() == TransferMode.MOVE) {
                        Dragboard db = event.getDragboard();
                        Task newTask = (Task) db.getContent(dataFormat);
                        Task emptyTask = new Task("","");
                        //remove original item
                        //item can have been moved up (so index becomes one too much)
                        // or such that the index didn't change, like to another day
                        if (list.getItems().get(labelCell.getIndex()).getText().equals(newTask.getText())) {
                            list.getItems().set(labelCell.getIndex(),emptyTask);
                            labelCell.setGraphic(null);
                        } else {
                            list.getItems().set(labelCell.getIndex() + 1, emptyTask);
                        }
                        //prevent an empty list from refusing to receive items, as it wouldn't contain any listcell
                        if (list.getItems().size() < 1) {
                            list.getItems().add(emptyTask);
                        }
                    }
                    event.consume();
                    cleanUp(list);
                });

                return labelCell;
            }
        });
        cleanUp(list);
    }

    /**
     * removes empty rows, and then fills up with empty rows
     * @param list to clean up
     */
    private void cleanUp(ListView<Task> list) {
        int maxListLength = 6; //todo variable listview length
        int i;
        //first remove empty items
        for (i=0; i < list.getItems().size(); i++) {
            if (list.getItems().get(i).getText().equals("")) {
                list.getItems().remove(i);
            }
        }
        //fill up if necessary
        for (i = 0; i<maxListLength; i++) {
            if (i >= list.getItems().size()) {
                list.getItems().add(i, new Task("",""));
            }
        }
    }

    private class LabelCell extends TextFieldListCell<Task> {
        HBox hbox = new HBox();
        Label text = new Label("");
        Pane pane = new Pane();
        ObservableList<String> comboList = FXCollections.observableArrayList(
                "0LAUK0","2WF50","2WA70","2IPC0");
        ComboBox<String> comboBox = new ComboBox<>(comboList);

        private LabelCell() {
            super();
            hbox.getChildren().addAll(text, pane, comboBox);
            HBox.setHgrow(pane, Priority.ALWAYS);
        }

        /**
         * called when starting edit with (null, true)
         * and when finished edit with (task, false)
         * @param task
         * @param empty
         */
        @Override
        public void updateItem(Task task, boolean empty) {
            super.updateItem(task, empty);
            setText(null);
            if (empty) {
                setGraphic(null);
            } else {
                text.setText(task.getText() != null ? task.getText() : "<null>");
                comboBox.setValue(task.getLabel() != null ? task.getLabel() : "<null>");
                setGraphic(hbox);
            }
        }
    }

    /**
     * custom stringconverter to define what editing a listcell means
     * this converter is set on each listcell
     */
    private class TaskConverter extends StringConverter<Task> {
        private final ListCell<Task> cell;
        private TaskConverter(ListCell<Task> cell) {
            this.cell = cell;
        }
        @Override
        public String toString(Task task) {
            return task.getText();
        }

        @Override
        public Task fromString(String string) {
            Task task = cell.getItem();
            task.setText(string);
            return task;
        }
    }


    //when transferred with the dragboard, the object is serialized
    //which I think means that a new object is created and you lose the reference to the old one
    //which I think should be fine here, as only content matters
    static class Task implements Serializable {
        private String text;
        private String label;

        private Task(String text, String label) {
            this.text = text;
            this.label = label;
        }

        private String getText() {
            return text;
        }

        private void setText(String text) {
            this.text = text;
        }

        private String getLabel() {
            return label;
        }

        private void setLabel(String label) {
            this.label = label;
        }
    }

}

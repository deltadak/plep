package deltadak;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.cell.TextFieldListCell;
import javafx.scene.input.*;
import javafx.scene.layout.GridPane;
import javafx.util.Callback;
import javafx.util.converter.DefaultStringConverter;

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
    @FXML ListView<String> day1;
    @FXML ListView<String> day2;
    @FXML GridPane gridpane;

    Connection connection;
    Statement statement;
    int countID = 1;
    ArrayList<String[]> dayOne = new ArrayList<>();


    /**
     * Initialization method for the controller.
     */
    @FXML public void initialize(URL location, ResourceBundle resourceBundle){
        setupGridPane();

//        createTable();
        Calendar calendar = Calendar.getInstance();
//        insertTask(calendar, "Freak out", "2WS20");
//        insertTask(calendar, "exam1", "2WA60");
//        insertTask(calendar, "exam2", "2WA60");
//        insertTask(calendar, "exam1", "2WA30");
//        insertTask(calendar, "exam2", "2WA30");
//        deleteTasksDay(calendar);

        getTasksDay(calendar);
        for (int i = 0; i < dayOne.size(); i++) {
            for (int j = 0; j < dayOne.get(i).length; j++) {
                System.out.println("[" + dayOne.get(i)[1] + "] " + dayOne.get(i)[0]);
            }
        }

    }

    /**
     * database communication below
     */

    public void insertTask(Calendar dayCal, String task, String subject) {
        getHighestID();

        String dayString = calendarToString(dayCal);
        System.out.println(dayString);

        setConnection();
        String sql = "INSERT INTO tasks(id, day, task, subject) " +
                    "VALUES (" + countID + ", '" + dayString + "', '" + task + "','" + subject + "')";
        countID++;
        query(sql);
    }

    public void getTasksDay(Calendar dayCal) {
        String dayString = calendarToString(dayCal);
        String sql = "SELECT task, subject FROM tasks WHERE day = '" + dayString + "'";

        setConnection();
        try {
            statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            while(resultSet.next()) {
                String[] task = {resultSet.getString("task"), resultSet.getString("subject")};
                dayOne.add(task);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void getHighestID() {
        String sql = "SELECT * FROM tasks ORDER BY id DESC";
        ArrayList<Integer>  ids = new ArrayList<>();

        setConnection();
        try {
            statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            if(resultSet.isBeforeFirst()) {
                countID = resultSet.getInt("id") + 1;
            }
            System.out.println(countID);
            statement.close();
            connection.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void deleteTask(int id) {
        String sql = "DELETE FROM tasks WHERE id = " + id;
        query(sql);
    }

    public void deleteTasksDay(Calendar day) {
        setConnection();

        String dayString = calendarToString(day);
        String sql = "DELETE FROM tasks WHERE day = '" + dayString + "'";
        query(sql);
    }

    public void createTable() {
        String sql = "CREATE TABLE IF NOT EXISTS tasks(" +
                "id INT PRIMARY KEY," +
                "day DATE," +
                "task CHAR(255)," +
                "subject CHAR(10))";
        query(sql);
    }

    public void deleteTable(String tableName) {
        String sql = "DROP TABLE IF EXISTS " + tableName;
        query(sql);
    }

    public void query(String sql) {
        setConnection();
        try {
            statement = connection.createStatement();
            statement.executeUpdate(sql);
            statement.close();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void setConnection() {
        try {
            Class.forName("org.sqlite.JDBC");
            connection = DriverManager.getConnection("jdbc:sqlite:plep.db");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }



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
        ObservableList<String> day1List = FXCollections.observableArrayList("task1","task2","","","","","","");
        ObservableList<String> day2List = FXCollections.observableArrayList("task3","task4","","","","","","");
        day1.setItems(day1List);
        day2.setItems(day2List);

        //setup drag and drop for all children of gridview
        gridpane.getChildren().stream().filter(node -> node instanceof ListView).forEach(node -> {
            ListView<String> list = (ListView<String>) node;
            setupListView(list);
            list.setOnEditCommit(t -> list.getItems().set(t.getIndex(), t.getNewValue()));
        });
    }

    private void setupListView(ListView<String> list) {
        //no idea why the callback needs a ListCell and not a TextFieldListCell
        //anyway, editing is enabled by using TextFieldListCell instead of ListCell
        list.setCellFactory(new Callback<ListView<String>, ListCell<String>>() {
            @Override
            public TextFieldListCell<String> call(ListView<String> param) {
                TextFieldListCell<String> listCell = new TextFieldListCell<String>() {
                    @Override
                    public void updateItem( String item, boolean empty) {
                        super.updateItem(item, empty);
                        setText(item);
                    }
                };

                //set converter to convert text input into object and back when editing
                listCell.setConverter(new DefaultStringConverter());

                listCell.setOnDragDetected( (MouseEvent event) -> {
                    if (!listCell.getItem().equals("")) {
                        Dragboard db = listCell.startDragAndDrop(TransferMode.COPY);
                        ClipboardContent content = new ClipboardContent();
                        content.putString(listCell.getItem());
                        db.setContent(content);
                    }
                    event.consume();
                });

                listCell.setOnDragOver(event -> {
                    if (event.getGestureSource() != listCell && event.getDragboard().hasString()) {
                        event.acceptTransferModes(TransferMode.COPY_OR_MOVE);
                    }
                    event.consume();
                });

                listCell.setOnDragEntered(event -> {
                    if (event.getGestureSource() != listCell && event.getDragboard().hasString()) {
                        System.out.println("TODO: change color of listview"); //todo
                    }
                    event.consume();
                });

                listCell.setOnDragExited(event -> {
                    System.out.println("TODO reset color of listview"); //todo
                    event.consume();
                });

                listCell.setOnDragDropped(event -> {
                    Dragboard db = event.getDragboard();
                    boolean success = false;
                    if (db.hasString()) {
                        String newvalue = db.getString();
                        //insert new task, removing will happen in onDragDone
                        int index = min(listCell.getIndex(),list.getItems().size()); // item can be dropped way below the existing list
                        //we have put an empty item instead of no items
                        //because otherwise there are no listCells that can receive an item
                        if (list.getItems().get(index).equals("")) {
                            list.getItems().set(index,newvalue); //replace empty item
                        } else {
                            list.getItems().add(index, newvalue);
                        }
                        success = true;
                    }
                    event.setDropCompleted(success);
                    event.consume();
                });

                listCell.setOnDragDone(event -> {
                    //ensures the original element is only removed on a valid copy transfer (no dropping outside listviews)
                    if (event.getTransferMode() == TransferMode.COPY) {
                        Dragboard db = event.getDragboard();
                        String draggedvalue = db.getString();
                        //remove original item
                        //item can have been moved up (so index becomes one too much)
                        // or such that the index didn't change, like to another day
                        if (list.getItems().get(listCell.getIndex()).equals(draggedvalue)) {
                            list.getItems().set(listCell.getIndex(),"");
                        } else {
                            list.getItems().set(listCell.getIndex()+1,"");
                        }
                        //prevent an empty list from refusing to receive items, as it wouldn't contain any listcell
                        if (list.getItems().size() < 1) {
                            list.getItems().add("");
                        }
                    }
                    event.consume();
                });

                return listCell;
            }
        });
    }


}

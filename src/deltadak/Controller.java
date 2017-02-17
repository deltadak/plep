package deltadak;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.geometry.Rectangle2D;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.Dialog;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.TextFieldListCell;
import javafx.scene.input.*;
import javafx.scene.layout.*;
import javafx.scene.text.Text;
import javafx.stage.Screen;
import javafx.util.Callback;
import javafx.util.StringConverter;

//import java.awt.*;
import java.io.File;
import java.io.Serializable;
import java.net.URL;
import java.security.CodeSource;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.ResourceBundle;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;

import static java.lang.Integer.min;

/**
 * Class to control the UI
 */
// incorrect warning about LocalDate may be weakened to ChronoLocalDate (not
// true)
@SuppressWarnings("TypeMayBeWeakened")
public class Controller implements Initializable {
    
    // main element of the UI is declared in interface.fxml
    @FXML GridPane gridPane;
    // used to transfer tasks with drag and drop
    private DataFormat dataFormat = new DataFormat("com.deltadak.Task");
    
    private String databasePath;
    
    // database globals
    private Connection connection;
    private Statement statement;
    private int countID = 1;
    
    // layout globals
    private static final int NUMBER_OF_DAYS = 9;
    private static final int MAX_COLUMNS = 3;
    private static final int MAX_LIST_LENGTH = 7;
    
    private static final int DIALOG_WIDTH = 300;
    private static final int DIALOG_HEIGHT = 100;
    
    /**
     * Initialization method for the controller.
     */
    @FXML
    public void initialize(final URL location,
                           final ResourceBundle resourceBundle) {
        
        // some optional insertion for testing purposes
    
//        LocalDate today = LocalDate.now();
//        LocalDate tomorrow = today.plusDays(1);
//        insertTask(today, "exam1", "2WA60",1);
//        insertTask(today, "exam2", "2WA60",2);
//        insertTask(today, "exam3", "2WA30",3);
//        insertTask(today, "exam4", "2WA30",4);
//        insertTask(tomorrow, "one", "2WA60",1);
//        insertTask(tomorrow, "two", "2WA60",2);
//        insertTask(tomorrow, "three", "2WA30",3);
//        insertTask(tomorrow, "boom", "2WA30",4);
        
        setDefaultDatabasePath();
        createTable(); // if not already exists
        setupGridPane();
    }

    /*
     * Database communication starts here -----------------------------------
     */
    
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
    private void insertTask(final LocalDate day, Task task, final int order) {
        setHighestID(); // sets countID
        
        String dayString = localDateToString(day);
        
        String sql = "INSERT INTO tasks(id, day, task, label, color, orderInDay) "
                + "VALUES (" + countID + ", '" + dayString + "', '" + task.getText()
                + "','" + task.getLabel() + "','" + task.getColor() + "'," + order + ")";
        countID++;
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
    private List<Task> getTasksDay(final LocalDate day) {
        
        String dayString = localDateToString(day);
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
    private void updateTasksDay(final LocalDate day, final List<Task> tasks) {
        
        long startTime = System.currentTimeMillis();
        System.out.println("updateTasksDay " + localDateToString(day));
        
        // first remove all the items for this day that are currently in the
        // database before we add the new ones,
        // so we don't get double tasks
        deleteTasksDay(day);
        
        // then add the new tasks
        for (int i = 0; i < tasks.size(); i++) {
            insertTask(day, tasks.get(i), i);
        }
    
        long stopTime = System.currentTimeMillis();
        long elapsedTime = stopTime - startTime;
        System.out.println(elapsedTime);
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
    public void deleteTask(final int id) {
        String sql = "DELETE FROM tasks WHERE id = " + id;
        query(sql);
    }
    
    /**
     * deletes all the tasks from the database where field task is empty
     * used when updating a day of which an item has been removed (by dragging)
     */
    private void deleteEmptyTasks() {
        String sql = "DELETE FROM tasks WHERE task = '' ";
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
        
        String dayString = localDateToString(day);
        String sql = "DELETE FROM tasks WHERE day = '" + dayString + "'";
        query(sql);
    }
    
    /**
     * sets the default path of the database to the directory the jar file is in
     */
    private void setDefaultDatabasePath() {
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
    
    /**
     * used to change the directory of the database
     * not used yet because we only set default database
     */
    //    private void changeDirectory() {
//            Dialog chooseDialog = new Dialog();
//            chooseDialog.setHeight(100);
//            chooseDialog.setWidth(300);
////            chooseDialog.setResizable(true);
//            chooseDialog.setTitle("Decisions!");
//
//            GridPane grid = new GridPane();
//            grid.setPrefHeight(chooseDialog.getHeight());
//            grid.setPrefWidth(chooseDialog.getWidth());
//
//            Button browseButton = new Button("Browse");
//            Text text = new Text("Choose database directory...");
//
//            ButtonType browseButtonType = new ButtonType("OK",
//                                                         ButtonBar.ButtonData.OK_DONE);
//            chooseDialog.getDialogPane().getButtonTypes().add(browseButtonType);
//            chooseDialog.getDialogPane().lookupButton(browseButtonType).setDisable(true);
//
//            browseButton.setOnMouseClicked(event -> {
//
//                System.out.println("button clicked");
//                DirectoryChooser directoryChooser = new DirectoryChooser();
//                directoryChooser.setTitle("Choose Directory");
//                File directory = directoryChooser.showDialog(new Stage());
//                String databaseDirectory = directory.getAbsolutePath();
//                text.setText(databaseDirectory);
//                databasePath = "jdbc:sqlite:";
//                databasePath += databaseDirectory + "\\plep.db";
//                System.out.println(databasePath);
//                chooseDialog.getDialogPane().lookupButton(browseButtonType).setDisable(false);
//
//            });
//
//
//            grid.add(browseButton,0,1);
//            grid.add(text,0,0);
//            chooseDialog.getDialogPane().setContent(grid);
//
//            chooseDialog.showAndWait();
//    }
    
    /**
     * Creates table with all the tasks, if it doesn't exist yet.
     */
    private void createTable() {
            String sql = "CREATE TABLE IF NOT EXISTS tasks(" + "id INT PRIMARY KEY,"
                    + "day DATE," + "task CHAR(255)," + "label CHAR(10),"
                    + "color CHAR(50)," + "orderInDay INT)";
            query(sql);
    
    }
    
    /**
     * Delete a given table from the database.
     *
     * @param tableName
     *         - name of the table that has to be deleted
     */
    public void deleteTable(final String tableName) {
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
    
    /**
     * Connect to the database.
     * Use this before sending a query to the database.
     */
    private void setConnection() {
        try {
            Class.forName("org.sqlite.JDBC");
            connection = DriverManager.getConnection(databasePath);
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /*
     * End of database communication ----------------------------------------
     */
    
    /**
     * Converts LocalDate object to String object.
     *
     * @param localDate
     *         to be converted
     *
     * @return String with eg 2017-03-25
     */
    private String localDateToString(final LocalDate localDate) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        return localDate.format(formatter);
    }
    
    /**
     * sets up listviews for each day, initializes drag and drop, editing items
     */
    private void setupGridPane() {
        for (int index = 0; index < NUMBER_OF_DAYS; index++) {
            
            // add days immediately, otherwise we can't use localDate in a
            // lambda expression (as it is not final)
            LocalDate localDate = LocalDate.now().plusDays(index - 1);
            
            ListView<Task> list = new ListView<>();
            VBox vbox = setTitle(list, localDate);
            addVBoxToGridPane(vbox, index);
            
            List<Task> tasks = getTasksDay(localDate);
            list.setItems(convertArrayToObservableList(tasks));
            list.setEditable(true);
            list.setPrefWidth(getListViewWidth());
            list.setPrefHeight(getListViewHeight());
            addLabelCells(list, localDate);
            //update database when editing is finished
            list.setOnEditCommit(event -> {
                updateTasksDay(localDate,
                               convertObservableToArrayList(list.getItems()));
                deleteEmptyTasks(); //from database
            });
            addDeleteKeyListener(list, localDate);
            cleanUp(list);
        }
    }
    
    /**
     * add title to listview
     *
     * @param list
     *         to use
     * @param localDate
     *         from which to make a title
     *
     * @return VBox with listview and title
     */
    private VBox setTitle(final ListView<Task> list,
                          final LocalDate localDate) {
        // vbox will contain a title above a list of tasks
        VBox vbox = new VBox();
        Label title = new Label(localDateToString(localDate));
        // the pane is used to align both properly (I think)
        Pane pane = new Pane();
        vbox.getChildren().addAll(title, pane, list);
        VBox.setVgrow(pane, Priority.ALWAYS);
        return vbox;
    }
    
    /**
     * add a box containing listview and title
     *
     * @param vbox
     *         to be added
     * @param index
     *         at the i'th place (left to right, top to bottom)
     */
    private void addVBoxToGridPane(final VBox vbox, final int index) {
        int row = index / MAX_COLUMNS;
        int column = index % MAX_COLUMNS;
        gridPane.add(vbox, column, row);
    }
    
    private void addDeleteKeyListener(final ListView<Task> list,
                                      final LocalDate localDate) {
        //add option to delete a task
        list.setOnKeyPressed(event -> {
            if (event.getCode() == KeyCode.DELETE) {
                list.getItems()
                        .remove(list.getSelectionModel().getSelectedIndex());
                updateTasksDay(localDate,
                               convertObservableToArrayList(list.getItems()));
                cleanUp(list); //cleaning up has to happen in the listener
                deleteEmptyTasks(); // from database
            }
        });
    }
    
    /**
     * convert ObservableList to ArrayList
     *
     * @param list
     *         to convert
     *
     * @return converted ObservableList
     */
    private List<Task> convertObservableToArrayList(
            final ObservableList<Task> list) {
        return new ArrayList<>(list);
    }
    
    /**
     * convert (Array)List to ObservableList
     *
     * @param list
     *         - List to be converted
     *
     * @return ObservableList
     */
    private ObservableList<Task> convertArrayToObservableList(
            final List<Task> list) {
        return FXCollections.observableList(list);
    }
    
    /**
     * get height by total screen size
     *
     * @return intended listview height
     */
    private int getListViewHeight() {
        Rectangle2D primaryScreenBounds = Screen.getPrimary().getVisualBounds();
        int totalHeight = (int)primaryScreenBounds.getHeight();
        return totalHeight / (NUMBER_OF_DAYS / MAX_COLUMNS);
    }
    
    /**
     * get width by total screen size
     *
     * @return intended listview width
     */
    private int getListViewWidth() {
        Rectangle2D primaryScreenBounds = Screen.getPrimary().getVisualBounds();
        int totalWidth = (int)primaryScreenBounds.getWidth();
        return totalWidth / MAX_COLUMNS;
    }
    
    /**
     * sets up labelCells for
     * @param list a ListView
     * @param day and a specific date
     */
    private void addLabelCells(final ListView<Task> list, final LocalDate day) {
        //no idea why the callback needs a ListCell and not a TextFieldListCell
        //anyway, editing is enabled by using TextFieldListCell instead of
        // ListCell
        list.setCellFactory(new Callback<ListView<Task>, ListCell<Task>>() {
            @Override
            public LabelCell call(final ListView<Task> param) {
                LabelCell labelCell = new LabelCell();
                
                //update text on changes
                labelCell.setConverter(new TaskConverter(labelCell));
                
                // update label on changes
                labelCell.comboBox.valueProperty().addListener(
                        (observable, oldValue, newValue) -> labelCell.getItem()
                                .setLabel(newValue));
                
                setOnLabelChangeListener(labelCell, list, day);
                
                setOnDragDetected(labelCell);
                setOnDragOver(labelCell);
                setOnDragEntered(labelCell);
                setOnDragExited(labelCell);
                setOnDragDropped(labelCell, list, day);
                setOnDragDone(labelCell, list, day);
//                setRightMouseClickListener(labelCell, day);
                
                setRightMouseClickListener(labelCell, list, day);
                
                return labelCell;
            }
        });
        cleanUp(list);
    }
    
    private void setRightMouseClickListener(LabelCell labelCell, LocalDate day) {
        labelCell.addEventHandler(MouseEvent.MOUSE_CLICKED, event -> {
            if (event.getButton() == MouseButton.SECONDARY) {
                showPopup(labelCell, day);
            }
        });
    }
    
    /**
     * used to change the directory of the database
     * not used yet because we only set default database
     */
    private void showPopup(LabelCell labelCell, LocalDate day) {
        Dialog chooseDialog = new Dialog();
        
        chooseDialog.setHeight(DIALOG_HEIGHT);
        chooseDialog.setWidth(DIALOG_WIDTH);
//            chooseDialog.setResizable(true);d
        chooseDialog.setTitle("Options");

        GridPane grid = new GridPane();
        grid.setPrefHeight(chooseDialog.getHeight());
        grid.setPrefWidth(chooseDialog.getWidth());

        ButtonType browseButtonType = new ButtonType("OK",
                                                     ButtonBar.ButtonData.OK_DONE);
        chooseDialog.getDialogPane().getButtonTypes().add(browseButtonType);
        chooseDialog.getDialogPane().lookupButton(browseButtonType).setDisable(true);
        
        // when user edits textfield, allow 'ok' button
        // the button will check for valid entry
        TextField numberRepeatsTextField = new TextField();
        if (labelCell.getItem().getText().equals("")) {
            grid.add(new Text("You cannot repeat an empty task."), 0, 0);
        } else {
            numberRepeatsTextField.textProperty().addListener((observable, oldValue, newValue) -> {
                // only do something on valid task
    
                // only allow numbers to be entered
                if (!newValue.matches("\\d")) {
                    // only repeat non-empty tasks
                    numberRepeatsTextField
                            .setText(newValue.replaceAll("[^\\d]", ""));
        
                }
                // enable ok button after any input
                chooseDialog.getDialogPane().lookupButton(browseButtonType).setDisable(false);
    
            });
    
            grid.add(new Text("Repeat ends after "), 0, 0);
            grid.add(numberRepeatsTextField, 1, 0);
            grid.add(new Text("occurrences, every week"), 0, 1);
        }
        
        chooseDialog.getDialogPane().setContent(grid);
    
        // stick dialog to mouse, unfortunately does not work before show()
//        Point mouseLocation = MouseInfo.getPointerInfo().getLocation();
//        chooseDialog.setX(mouseLocation.getX());
//        chooseDialog.setY(mouseLocation.getY());
        
        chooseDialog.showAndWait();
        
        // button has been clicked, we already ensured the textfield only
        // contains digits, and the task to repeat is not empty
        int repeatNumber = Integer.parseInt(numberRepeatsTextField.getText());
        // insert all next tasks in database
        for (int i = 1; i < repeatNumber; i++) {
            // find the day to which to copy the task to
            day = day.plusWeeks(1);
            // task that was right-clicked
            Task taskToRepeat = labelCell.getItem();
            List<Task> tasks = getTasksDay(day);
            tasks.add(taskToRepeat);
            updateTasksDay(day, tasks);
        }
        refreshAllDays();
    }
    
    /**
     * @return all ListViews in the gridPane
     */
    private List<ListView<Task>> getAllListViews() {
        ArrayList<ListView<Task>> listViews = new ArrayList<>();
        for (Node node : gridPane.getChildren()) {
            //gridpane contains vbox contains label, pane and listview
            if (node instanceof VBox) {
                // we try to dig up the listviews in this vbox
                for (Node subNode : ((VBox)node).getChildren()) {
                    if (subNode instanceof ListView) {
                        listViews.add((ListView) subNode);
                    }
                }
            }
        }
        return listViews;
    }
    
    /**
     * refreshes all listviews using data from the database
     */
    private void refreshAllDays() {
        // find all listviews
        List<ListView<Task>> listViews = getAllListViews();
        
        for (int i = 0; i < NUMBER_OF_DAYS; i++) {
            ListView<Task> list = listViews.get(i);
            // refresh the listview from database
            LocalDate localDate = LocalDate.now().plusDays(i - 1);
            List<Task> tasks = getTasksDay(localDate);
            list.setItems(convertArrayToObservableList(tasks));
            addLabelCells(list, localDate);
            cleanUp(list);
        }
    }
    
    private void setOnLabelChangeListener(final LabelCell labelCell,
                                          final ListView<Task> list,
                                          final LocalDate day) {
        // update label in database when selecting a different one
        labelCell.comboBox.getSelectionModel().selectedIndexProperty()
                .addListener((observable, oldValue, newValue) -> {
                    updateTasksDay(day, convertObservableToArrayList(
                            list.getItems()));
                    deleteEmptyTasks(); // from database
                    cleanUp(list);
                });
    }
    
    private void setRightMouseClickListener(final LabelCell labelCell,
                                            final ListView<Task> list,
                                            final LocalDate day) {
        
        labelCell.addEventHandler(MouseEvent.MOUSE_CLICKED, event -> {
            if (event.getButton() == MouseButton.SECONDARY) {
                createContextMenu(event, labelCell, list, day);
                
                
//                for (MenuItem menuItem : menuItems) {
//                    menuItem.setOnAction(event1 -> {
//                        System.out.println(menuItem + " clicked");
//                        System.out.println(menuItem.getStyleClass());
//                        String colorWord = menuItem.getText();
//                        String colorString = convertColorToHex(colorWord);
//                        if (colorString.equals("#ffffffff")) {
//                            labelCell.setStyle("-fx-text-fill: none");
//                        } else {
//                            labelCell.setStyle(
//                                    "-fx-control-inner-background: "
//                                            + colorString);
//                        }
//                        labelCell.getItem().setColor(colorWord);
//                        updateTasksDay(day, convertObservableToArrayList(
//                                list.getItems()));
//                        deleteEmptyTasks(); // from database
//                        cleanUp(list);
//                    });
//                }
                }
//
//
//
        });
    }
    
    private void createContextMenu(final MouseEvent event,
                                   final LabelCell labelCell,
                                   final ListView<Task> list,
                                   final LocalDate day) {
        
        ContextMenu contextMenu = new ContextMenu();
        
        Menu repeatTasksMenu = new Menu("Repeat for x weeks");
        MenuItem oneMenuItem = new MenuItem("1");
        MenuItem twoMenuItem = new MenuItem("2");
        MenuItem threeMenuItem = new MenuItem("3");
        MenuItem fourMenuItem = new MenuItem("4");
        MenuItem fiveMenuItem = new MenuItem("5");
        MenuItem sixMenuItem = new MenuItem("6");
        MenuItem sevenMenuItem = new MenuItem("7");
        MenuItem eightMenuItem = new MenuItem("8");
        repeatTasksMenu.getItems().addAll(oneMenuItem, twoMenuItem,threeMenuItem,
                                          fourMenuItem, fiveMenuItem, sixMenuItem,
                                          sevenMenuItem, eightMenuItem);
        
        List<MenuItem> repeatMenuItems = repeatTasksMenu.getItems();
        for (MenuItem repeatMenuItem : repeatMenuItems) {
            repeatMenuItem.setOnAction(new EventHandler<ActionEvent>() {
                @Override
                public void handle(ActionEvent event) {
                    System.out.println(repeatMenuItem.getText() + " clicked");
                }
            });
        }
        
        SeparatorMenuItem separatorMenuItem = new SeparatorMenuItem();
        
        MenuItem firstColor = new MenuItem("Green");
        MenuItem secondColor = new MenuItem("Blue");
        MenuItem thirdColor = new MenuItem("Red");
        MenuItem defaultColor = new MenuItem("White");
        
        
        contextMenu.getItems().addAll(repeatTasksMenu, separatorMenuItem,
                                      firstColor, secondColor, thirdColor,defaultColor);
        contextMenu.show(labelCell, event.getScreenX(), event.getScreenY());
    }
    
    private void setOnDragDetected(final LabelCell labelCell) {
        labelCell.setOnDragDetected((MouseEvent event) -> {
            if (!labelCell.getItem().getText().equals("")) {
                Dragboard db = labelCell.startDragAndDrop(TransferMode.MOVE);
                ClipboardContent content = new ClipboardContent();
                content.put(dataFormat, labelCell.getItem());
                db.setContent(content);
            }
            event.consume();
        });
    }
    
    private void setOnDragOver(final LabelCell labelCell) {
        labelCell.setOnDragOver(event -> {
            if ((!Objects.equals(event.getGestureSource(), labelCell)) && event
                    .getDragboard().hasContent(dataFormat)) {
                event.acceptTransferModes(TransferMode.MOVE);
            }
            event.consume();
        });
    }
    
    private void setOnDragEntered(final LabelCell labelCell) {
        labelCell.setOnDragEntered(event -> {
            if ((!Objects.equals(event.getGestureSource(), labelCell)) && event
                    .getDragboard().hasContent(dataFormat)) {
                System.out.println("TODO: change color of listview"); //todo
            }
            
            event.consume();
        });
    }
    
    private void setOnDragExited(final LabelCell labelCell) {
        labelCell.setOnDragExited(event -> {
            System.out.println("TODO reset color of listview"); //todo
            event.consume();
        });
    }
    
    private void setOnDragDropped(final LabelCell labelCell,
                                  final ListView<Task> list,
                                  final LocalDate day) {
        labelCell.setOnDragDropped(event -> {
            Dragboard db = event.getDragboard();
            boolean success = false;
            if (db.hasContent(dataFormat)) {
                Task newTask = (Task)db.getContent(dataFormat);
                //insert new task, removing will happen in onDragDone
                int index = min(labelCell.getIndex(), list.getItems()
                        .size()); // item can be dropped way below
                // the existing list
                //we have put an empty item instead of no items
                //because otherwise there are no listCells that can
                // receive an item
                if (list.getItems().get(index).getText().equals("")) {
                    list.getItems().set(index, newTask); //replace empty item
                } else {
                    list.getItems().add(index, newTask);
                }
                success = true;
                // update tasks in database
                updateTasksDay(day,
                               convertObservableToArrayList(list.getItems()));
            }
            event.setDropCompleted(success);
            event.consume();
            cleanUp(list);
        });
    }
    
    private void setOnDragDone(final LabelCell labelCell,
                               final ListView<Task> list, final LocalDate day) {
        labelCell.setOnDragDone(event -> {
            //ensures the original element is only removed on a
            // valid copy transfer (no dropping outside listviews)
            if (event.getTransferMode() == TransferMode.MOVE) {
                Dragboard db = event.getDragboard();
                Task newTask = (Task)db.getContent(dataFormat);
                Task emptyTask = new Task("", "", "White");
                //remove original item
                //item can have been moved up (so index becomes one
                // too much)
                // or such that the index didn't change, like to
                // another day
                if (list.getItems().get(labelCell.getIndex()).getText()
                        .equals(newTask.getText())) {
                    list.getItems().set(labelCell.getIndex(), emptyTask);
                    labelCell.setGraphic(null);
                    // update in database
                    updateTasksDay(day, convertObservableToArrayList(
                            list.getItems()));
                    // deleting blank row from database updating creates
                    deleteEmptyTasks();
                } else {
                    list.getItems().set(labelCell.getIndex() + 1, emptyTask);
                }
                //prevent an empty list from refusing to receive
                // items, as it wouldn't contain any listcell
                if (list.getItems().size() < 1) {
                    list.getItems().add(emptyTask);
                }
            }
            event.consume();
            cleanUp(list);
        });
    }
    
    /**
     * removes empty rows, and then fills up with empty rows
     *
     * @param list
     *         to clean up
     */
    private void cleanUp(final ListView<Task> list) {
        int i;
        //first remove empty items
        for (i = 0; i < list.getItems().size(); i++) {
            if (list.getItems().get(i).getText().equals("")) {
                list.getItems().remove(i);
            }
        }
        //fill up if necessary
        for (i = 0; i < MAX_LIST_LENGTH; i++) {
            if (i >= list.getItems().size()) {
                list.getItems().add(i, new Task("", "", "White"));
            }
        }
        
    }
    
    private String convertColorToHex(final String colorName) {
        String hex;
        switch (colorName) {
            case "Green": hex = "#7ef202";
                break;
            case "Blue": hex = "#4286f4";
                break;
            case "Red": hex = "#e64d4d";
                break;
            case "White" : hex = "#ffffffff";
                break;
            default: hex = "#ffffffff";
        }
        return hex;
    }
    
    /**
     * custom ListCell
     */
    private class LabelCell extends TextFieldListCell<Task> {
        
        HBox hbox = new HBox();
        Label text = new Label("");
        Pane pane = new Pane();
        ObservableList<String> comboList = FXCollections
                .observableArrayList("0LAUK0", "2WF50", "2WA70", "2IPC0");
        ComboBox<String> comboBox = new ComboBox<>(comboList);
        
        private LabelCell() {
            super();
            hbox.getChildren().addAll(text, pane, comboBox);
            HBox.setHgrow(pane, Priority.ALWAYS);
        }
        
        /**
         * called when starting edit with (null, true)
         * and when finished edit with (task, false)
         *
         * @param task
         *         to be updated
         * @param empty
         *         whether to set empty?
         */
        @Override
        public void updateItem(final Task task, final boolean empty) {
            super.updateItem(task, empty);
            setText(null);
            if (empty) {
                setGraphic(null);
            } else {
                text.setText(
                        (task.getText() != null) ? task.getText() : "<null>");
                comboBox.setValue(
                        (task.getLabel() != null) ? task.getLabel() : "<null>");
                setGraphic(hbox);
                setStyle("-fx-control-inner-background: "
                                 + convertColorToHex(task.getColor()));
            }
        }
    }
    
    /**
     * custom stringconverter to define what editing a listcell means
     * this converter is set on each listcell
     */
    private class TaskConverter extends StringConverter<Task> {
        
        private final ListCell<Task> cell;
        
        private TaskConverter(final ListCell<Task> cell) {
            this.cell = cell;
        }
        
        @Override
        public String toString(final Task task) {
            return task.getText();
        }
        
        @Override
        public Task fromString(final String string) {
            Task task = cell.getItem();
            task.setText(string);
            
            return task;
        }
    }
    
    //when transferred with the dragboard, the object is serialized
    //which I think means that a new object is created and you lose the
    // reference to the old one
    //which I think should be fine here, as only content matters
    static class Task implements Serializable {
        
        private String text;
        private String label;
        private String color;
        
        private Task(final String text, final String label, final String color) {
            this.text = text;
            this.label = label;
            this.color = color;
        }
        
        private String getText() {
            return text;
        }
        
        private void setText(final String text) {
            this.text = text;
        }
        
        private String getLabel() {
            return label;
        }
        
        private void setLabel(final String label) {
            this.label = label;
        }
        
        private String getColor() {
            return color;
        }
        
        private void setColor(final String color) {
            this.color = color;
        }
    }
    
}

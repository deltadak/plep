package deltadak;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.concurrent.Service;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.geometry.Pos;
import javafx.geometry.Rectangle2D;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.Label;
import javafx.scene.input.*;
import javafx.scene.layout.*;
import javafx.stage.Screen;
import javafx.util.Callback;

import javax.xml.crypto.Data;
import java.net.URL;
import java.time.LocalDate;
import java.util.List;
import java.util.ResourceBundle;
import java.util.ArrayList;

/**
 * Class to control the UI
 */
// incorrect warning about LocalDate may be weakened to ChronoLocalDate (not
// true)
@SuppressWarnings("TypeMayBeWeakened")
public class Controller implements Initializable {
    
    // main element of the UI is declared in interface.fxml
    @FXML GridPane gridPane;
    @FXML ProgressIndicator progressIndicator;
    // used to transfer tasks with drag and drop
    DataFormat dataFormat = new DataFormat("com.deltadak.Task");
    
    // layout globals
    private static final int NUMBER_OF_DAYS = 9;
    private static final int MAX_COLUMNS = 3;
    private static final int MAX_LIST_LENGTH = 7;
    
    
    /**
     * Initialization method for the controller.
     */
    @FXML
    public void initialize(final URL location,
                           final ResourceBundle resourceBundle) {
        
        setDefaultDatabasePath();
        createTable(); // if not already exists
        setupGridPane();
        progressIndicator.setVisible(false);
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
            // disable leaving half-selected listcells around
//            list.setFocusTraversable(false);
            setupLabelCells(list, localDate);
            //update database when editing is finished
            list.setOnEditCommit(event -> updateTasksDay(
                    localDate, convertObservableToArrayList(list.getItems())));
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
        Label title = new Label(localDate.getDayOfWeek() + " " + localDate);
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
    
    /**
     * add a Listener to a list for the delete key
     *
     * @param list ListView to add the Listener to
     * @param localDate so we know for what day to update the database
     */
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
    List<Task> convertObservableToArrayList(
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
     *
     * @param list a ListView
     * @param day and a specific date
     */
    private void setupLabelCells(final ListView<Task> list, final LocalDate day) {
        //no idea why the callback needs a ListCell and not a TextFieldListCell
        //anyway, editing is enabled by using TextFieldListCell instead of
        // ListCell
        list.setCellFactory(new Callback<ListView<Task>, ListCell<Task>>() {
            @Override
            public LabelCell call(final ListView<Task> param) {
                LabelCell labelCell = new LabelCell(Controller.this);
                labelCell.setup(list, day);
                return labelCell;
            }
        });
        cleanUp(list);
    }
    
    /**
     * @return all ListViews in the gridPane
     */
    private List<ListView<Task>> getAllListViews() {
        List<ListView<Task>> listViews = new ArrayList<>();
        for (Node node : gridPane.getChildren()) {
            //gridpane contains vbox contains label, pane and listview
            if (node instanceof VBox) {
                // we try to dig up the listviews in this vbox
                for (Node subNode : ((Pane)node).getChildren()) {
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
    void
    refreshAllDays() {
        // find all listviews
        List<ListView<Task>> listViews = getAllListViews();
        
        for (int i = 0; i < NUMBER_OF_DAYS; i++) {
            ListView<Task> list = listViews.get(i);
            // refresh the listview from database
            LocalDate localDate = LocalDate.now().plusDays(i - 1);
            List<Task> tasks = getTasksDay(localDate);
            list.setItems(convertArrayToObservableList(tasks));
            cleanUp(list);
        }
    }
    
    /**
     * Makes the menu with options to repeat for 1-8 weeks.
     * @param labelCell task to repeat
     * @param day the day to repeat
     * @return the menu with those options
     */
    private Menu makeRepeatMenu(LabelCell labelCell, LocalDate day) {
        Menu repeatTasksMenu = new Menu("Repeat for x weeks");
        for (int i = 1; i < 9; i++) {
            MenuItem menuItem = new MenuItem(String.valueOf(i));
            repeatTasksMenu.getItems().add(menuItem);
        }
    
        List<MenuItem> repeatMenuItems = repeatTasksMenu.getItems();
        for (MenuItem repeatMenuItem : repeatMenuItems) {
            repeatMenuItem.setOnAction(event12 -> {
                int repeatNumber = Integer.valueOf(repeatMenuItem.getText());
                System.out.println(repeatNumber + " clicked");
                Task taskToRepeat = labelCell.getItem();
                repeatTask(repeatNumber, taskToRepeat, day);
            });
        }
        return repeatTasksMenu;
    }
    
    /**
     * create a context menu
     * @param event show context menu at place of mouse event
     * @param labelCell to know which labelCell to color or repeat or ...
     * @param list to update and cleanup after changing labelCell
     * @param day needed for updating the database
     */
    void createContextMenu(final MouseEvent event,
                                   final LabelCell labelCell,
                                   final ListView<Task> list,
                                   final LocalDate day) {
        
        ContextMenu contextMenu = new ContextMenu();
        Menu repeatTasksMenu = makeRepeatMenu(labelCell, day);
        SeparatorMenuItem separatorMenuItem = new SeparatorMenuItem();
        
        MenuItem firstColor = new MenuItem("Green");
        MenuItem secondColor = new MenuItem("Blue");
        MenuItem thirdColor = new MenuItem("Red");
        MenuItem defaultColor = new MenuItem("White");
        
        contextMenu.getItems().addAll(repeatTasksMenu, separatorMenuItem,
                                      firstColor, secondColor,
                                      thirdColor, defaultColor);
        
        for (int i = 1; i < contextMenu.getItems().size(); i++) {
                MenuItem colorMenuItem = contextMenu.getItems().get(i);
                colorMenuItem.setOnAction(event1 -> {
                System.out.println(colorMenuItem.getText() + " clicked");
                setBackgroundColor(colorMenuItem, labelCell);
                updateTasksDay(day, convertObservableToArrayList(list.getItems()));
                cleanUp(list);

            });
        }
        contextMenu.show(labelCell, event.getScreenX(), event.getScreenY());
    }
    
    /**
     * sets the background color of a LabelCell
     *
     * @param menuItem MenuItem to retrieve the color from
     * @param labelCell LabelCell of which to change the background color
     */
    private void setBackgroundColor(final MenuItem menuItem,
                                    final LabelCell labelCell) {
        String colorWord = menuItem.getText();
        String colorString = convertColorToHex(colorWord);
        if (colorString.equals("#ffffffff")) {
            labelCell.setStyle("-fx-text-fill: none");
        } else {
            labelCell.setStyle(
                    "-fx-control-inner-background: "
                            + colorString);
        }
        labelCell.getItem().setColor(colorWord);
    
    }
    
    /**
     * repeats a task for a number of weeks
     *
     * @param repeatNumber how many times to repeat a task
     * @param task the task to repeat
     * @param day the current day is needed to update the days on which the
     *            task will be repeated
     */
    private void repeatTask(final int repeatNumber, final Task task, LocalDate day) {
        for (int i = 0; i < repeatNumber; i++) {
            day = day.plusWeeks(1);
            List<Task> tasks = getTasksDay(day);
            tasks.add(task);
            updateTasksDay(day, tasks);
        }
        refreshAllDays();
    }
    
    /**
     * removes empty rows, and then fills up with empty rows
     *
     * @param list
     *         to clean up
     */
    void cleanUp(final ListView<Task> list) {
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
    
    /**
     * converts a String containing a color (e.g. Green) to a String with the
     * hex code of that color, so the styling can use it
     *
     * @param colorName String containing the color
     * @return String with the hex code of
     */
    String convertColorToHex(final String colorName) {
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
    
    
    /*
     * Database methods, Database is a singleton using the enum structure.
     * For corresponding javadoc see Database.
     */
    
    private void setDefaultDatabasePath() {
        Database.getInstance().setDefaultDatabasePath();
    }
    
    private void createTable() {
        Database.getInstance().createTable();
    }
    
    private List<Task> getTasksDay(final LocalDate localDate) {
        return Database.getInstance().getTasksDay(localDate);
    }
    
    void updateTasksDay(final LocalDate day,
                                final List<Task> tasks) {
        final Service service = new Service() {
            @Override
            protected javafx.concurrent.Task<String> createTask() {
                return new javafx.concurrent.Task<String>() {
                    @Override
                    protected String call() throws Exception {
                        System.out.println("running");
                        Database.getInstance().updateTasksDay(day, tasks);
                        return "bla";
                    }
                };
    
            }
        };
    
        progressIndicator.visibleProperty().bind(service.runningProperty());
    
    
        service.setOnSucceeded(event -> System.out.println("done"));
        
        service.restart();
    
    }
    
    /*
     * End of database methods
     */
    
}

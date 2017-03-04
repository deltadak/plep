package deltadak;

import javafx.animation.TranslateTransition;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.geometry.Rectangle2D;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.Label;
import javafx.scene.control.cell.TextFieldListCell;
import javafx.scene.input.*;
import javafx.scene.layout.*;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.util.Callback;
import javafx.util.Duration;

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
    @FXML AnchorPane settingsPane;
        @FXML GridPane editLabelsPane;
            private ListView<String> labelsList;
            @FXML Button settingsButton;
            @FXML Button removeLabelButton;
    
    // used to transfer tasks with drag and drop
    DataFormat dataFormat = new DataFormat("com.deltadak.Task");
    
    // layout globals
    private static final int NUMBER_OF_DAYS = 9;
    private static final int MAX_COLUMNS = 3;
    private static final int MAX_LIST_LENGTH = 7;
    
    // layout globals for the settings pane
    private static final int SETTINGS_WIDTH = 350;
    private static final int LISTVIEW_ROW_HEIGHT = 29;
    private static final int MAX_NUMBER_LABELS = 5;
    // the duration of the animation when opening and closing the settings pane
    private static final int TOGGLE_SETTINGS_DURATION = 350;
    
    private LocalDate focusDay;
    private LocalDate today;
    private static final int NUMBER_OF_MOVING_DAYS = 7;
    
    /**
     * Initialization method for the controller.
     */
    @FXML
    public void initialize(final URL location,
                           final ResourceBundle resourceBundle) {
        
        setDefaultDatabasePath();
        createTable(); // if not already exists
        createLabelsTable();
        
        focusDay = LocalDate.now(); // set focus day to today
        setupGridPane(focusDay);
        setupSettingsMenu();
        
        // Notice that the listener which listens for day changes is called from
        // Main, because it needs the primary Stage.
    
        prepareToggleSettings();
    }
    
    /**
     * sets up listviews for each day, initializes drag and drop, editing items
     * @param focusDate date that is the top middle one (is today on default)
     */
    private void setupGridPane(LocalDate focusDate) {
        // first clear the gridpane so we don't get titles overlaying each other
        gridPane.getChildren().clear();
        for (int index = 0; index < NUMBER_OF_DAYS; index++) {
            
            // add days immediately, otherwise we can't use localDate in a
            // lambda expression (as it is not final)
            LocalDate localDate = focusDate.plusDays(index - 1);
            
            ListView<Task> list = new ListView<>();
            VBox vbox = setTitle(list, localDate);
            addVBoxToGridPane(vbox, index);
            
            List<Task> tasks = getTasksDay(localDate);
            list.setItems(convertArrayToObservableList(tasks));
            list.setEditable(true);
            list.setPrefWidth(getListViewWidth());
            list.setPrefHeight(getListViewHeight());
            setupLabelCells(list, localDate);
            //update database when editing is finished
            list.setOnEditCommit(event -> updateTasksDay(
                    localDate, convertObservableToArrayList(list.getItems())));
            addDeleteKeyListener(list, localDate);
            cleanUp(list);
        }
    }
    
    /**
     * Sets a listener which checks if it is a new day,
     * when the window becomes focused.
     *
     * @param primaryStage Stage to set listener on.
     */
    public void setDayChangeListener(Stage primaryStage) {
        // debug line, also comment out the line to reset 'today'
//        today = LocalDate.now().plusDays(-1);
        today = LocalDate.now();
        primaryStage.focusedProperty().addListener((observable, wasFocused, isFocused) -> {
            if (isFocused) { // if becomes focused
                if (!today.equals(LocalDate.now())) {
                    // then reset view
                    today = LocalDate.now();
                    setupGridPane(today);
                }
            }
        });
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
    void refreshAllDays() {
        // find all listviews
        List<ListView<Task>> listViews = getAllListViews();
        
        for (int i = 0; i < NUMBER_OF_DAYS; i++) {
            ListView<Task> list = listViews.get(i);
            // refresh the listview from database
            LocalDate localDate = focusDay.plusDays(i - 1);
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
    
    /**
     * called by the backward button
     * moves the planner a (few) day(s) back
     */
    @FXML protected void dayBackward() {
        focusDay = focusDay.plusDays(-NUMBER_OF_MOVING_DAYS);
        setupGridPane(focusDay);
    }
    
    /**
     * called by the today button
     * focuses the planner on today
     */
    @FXML protected void goToToday() {
        focusDay = LocalDate.now();
        setupGridPane(focusDay);
    }
    
    /**
     * called by the forward button
     * moves the planner a (few) day(s) forward
     */
    @FXML protected void dayForward() {
        focusDay = focusDay.plusDays(NUMBER_OF_MOVING_DAYS);
        setupGridPane(focusDay);
    }
    
    /*
     * Settings ----------------------------------------------------------------
     */
    
    /**
     * Sets up the animations for the settings pane, so we can open and close
     * the settings menu.
     */
    private void prepareToggleSettings() {
        settingsPane.setPrefWidth(SETTINGS_WIDTH);
        // set the left x coordinate of the settings pane at -SETTINGS_WIDTH
        // on initialization, so the entire pane is outside of the window
        settingsPane.setTranslateX(-SETTINGS_WIDTH);
        
        // setup the animation to open the settings pane
        TranslateTransition openNav =
                new TranslateTransition(new Duration(TOGGLE_SETTINGS_DURATION),
                                        settingsPane);
        openNav.setToX(0);
        
        // setup the animation to close the settings pane
        TranslateTransition closeNav =
                new TranslateTransition(new Duration(TOGGLE_SETTINGS_DURATION),
                                        settingsPane);
        
        settingsButton.setOnAction((ActionEvent evt)->{
            if(settingsPane.getTranslateX()!=0){
                openNav.play();
            }else{
                closeNav.setToX(-(settingsPane.getWidth()));
                closeNav.play();
            }
        });
    }
    
    /**
     * Sets up the content of the settings menu.
     */
    public void setupSettingsMenu() {
        setupEditLabelsListView();
    }
    
    /**
     * Sets up the editable ListView to edit the labels/items we want to see
     * in the comboboxes on the main screen.
     */
    private void setupEditLabelsListView() {
        labelsList = new ListView<>();
        // first set up the listview with empty labels
        ObservableList<String> itemsLabelsList =
                FXCollections.observableArrayList("","","","","");
    
        // get the labels from the database and store them in the listview
        ArrayList<String> labelStrings = getLabels();
        for (int i = 0; i < labelStrings.size(); i++) {
            itemsLabelsList.set(i, labelStrings.get(i));
        }
    
        // set a CellFactory on the listview to be able make the cells editable
        // using setEditable(true) isn't enough
        labelsList.setCellFactory(TextFieldListCell.forListView());
        labelsList.setEditable(true);
        
        // give the listview an id (FXML) so we can look it up by its id, and
        // toggle the visibility
        labelsList.setId("labelsListView");
        labelsList.setItems(itemsLabelsList);
        labelsList.setVisible(false);
        labelsList.setPrefWidth(120);
        labelsList.setPrefHeight((LISTVIEW_ROW_HEIGHT * MAX_NUMBER_LABELS) + 18);
        
        // position the listview in the settings pane
        GridPane.setColumnIndex(labelsList, 1);
        GridPane.setRowIndex(labelsList, 0);
        GridPane.setRowSpan(labelsList, 2);
    
        // when editing a label in the listview, update the value
        // in the database and setup the main gridpane with the new items in the
        // comboboxed
        labelsList.setOnEditCommit(event -> {
            labelsList.getItems()
                    .set(event.getIndex(), event.getNewValue());
            updateLabel(event.getIndex(), event.getNewValue());
            setupGridPane(focusDay);
        });
    
        editLabelsPane.getChildren().add(labelsList);
    }
    
    /**
     * Toggles the visibility of the listview with labels.
     */
    @FXML protected void editCourseLabels() {
        toggleVisibilityFXMLObject("labelsListView");
        toggleVisibilityFXMLObject("removeLabelButton");
    }
    
    /**
     * Removes the selected label from all the items in the combobox, also
     * removes it from the database
     */
    @FXML protected void removeLabel() {
        int selectedIndex = labelsList.getSelectionModel().getSelectedIndex();
        // to remove an item from the listview, we replace it with an empty
        // string, so we can edit it again
        labelsList.getItems().set(selectedIndex,"");
        updateLabel(selectedIndex, "");
        setupGridPane(focusDay);
    }
    
    /**
     * Toggles the visibility of an object.
     * @param id String with a FXML id of the object to be toggled.
     */
    private void toggleVisibilityFXMLObject(String id) {
        Boolean isVisible = editLabelsPane.lookup("#" + id).isVisible();
        editLabelsPane.lookup("#" + id).setVisible(!isVisible);
    }
    
    /*
     * End of settings ---------------------------------------------------------
     */
    
    
    /*
     * Database methods, Database is a singleton using the enum structure.
     * For corresponding javadoc see Database.
     */
    
    /**
     * See {@link Database#setDefaultDatabasePath()}.
     */
    private void setDefaultDatabasePath() {
        Database.INSTANCE.setDefaultDatabasePath();
    }
    
    /**
     * See {@link Database#createTable()}.
     */
    private void createTable() {
        Database.INSTANCE.createTable();
    }
    
    /**
     * See {@link Database#createLabelsTable()}
     */
    private void createLabelsTable() {
        Database.INSTANCE.createLabelsTable();
    }
    
    /**
     * See {@link Database#getTasksDay(LocalDate)}.
     * @param localDate Same.
     * @return Same.
     */
    private List<Task> getTasksDay(final LocalDate localDate) {
        return Database.INSTANCE.getTasksDay(localDate);
    }
    
    /**
     * See {@link Database#updateTasksDay(LocalDate, List)}
     * @param day Same.
     * @param tasks Same.
     */
    void updateTasksDay(final LocalDate day,
                                final List<Task> tasks) {
        Database.INSTANCE.updateTasksDay(day, tasks);
    }
    
    /**
     * See {@link Database#getLabels()}.
     * @return Same.
     */
    ArrayList<String> getLabels() {
        return Database.INSTANCE.getLabels();
    }
    
    /**
     * See {@link Database#updateLabel(int, String)}
     * @param id Same.
     * @param label Same.
     */
    void updateLabel(int id, String label) {
        Database.INSTANCE.updateLabel(id, label);
    }
    
    /*
     * End of database methods
     */
    
}

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
import javafx.scene.text.Text;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.util.Callback;
import javafx.util.Duration;
import javafx.concurrent.Task;

import javax.xml.crypto.Data;
import java.net.URL;
import java.time.LocalDate;
import java.util.List;
import java.util.Objects;
import java.util.ResourceBundle;
import java.util.ArrayList;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

/**
 * Class to control the UI
 */
// incorrect warning about LocalDate may be weakened to ChronoLocalDate (not
// true)
@SuppressWarnings("TypeMayBeWeakened")
public class Controller implements Initializable {
    
    // main element of the UI is declared in interface.fxml
    @FXML AnchorPane main;
    @FXML GridPane gridPane;
    @FXML ToolBar toolBar;
    @FXML ProgressIndicator progressIndicator;
    @FXML AnchorPane settingsPane;
        @FXML GridPane editLabelsPane;
            @FXML Button editLabelsButton;
            private ListView<String> labelsList;
            @FXML Button settingsButton;
            @FXML Button removeLabelButton;
        @FXML GridPane editDaysPane;
            @FXML Text numberOfMovingDaysText;
            private Spinner<Integer> numberOfMovingDaysSpinner;
            private Spinner<Integer> numberOfShowDaysSpinner;
    
    // used to transfer tasks with drag and drop
    DataFormat dataFormat = new DataFormat("com.deltadak.HomeworkTask");
    
    // layout globals
    private int NUMBER_OF_DAYS;
    private  int NUMBER_OF_MOVING_DAYS;
    
    private int MAX_COLUMNS = 3;
    private static final int MAX_LIST_LENGTH = 7;
    
    private static final String NUMBER_OF_DAYS_NAME = "number_of_days";
    private static final String NUMBER_OF_MOVING_DAYS_NAME
            = "number_of_moving_days";
    
    // layout globals for the settings pane
    private static final int SETTINGS_WIDTH = 400;
    private static final int LISTVIEW_ROW_HEIGHT = 29;
    private static final int MAX_NUMBER_LABELS = 5;
    // the duration of the animation when opening and closing the settings pane
    private static final int TOGGLE_SETTINGS_DURATION = 350;
    
    private LocalDate focusDay;
    private LocalDate today;
    // Multithreading
    private Executor exec;
    
    /**
     * Initialization method for the controller.
     */
    @Override
    @FXML
    public void initialize(final URL location,
                           final ResourceBundle resourceBundle) {
    
        // Initialize multithreading.
        exec = Executors.newCachedThreadPool(runnable -> {
            Thread t = new Thread(runnable);
            t.setDaemon(true);
            return t;
        });
        
        setDefaultDatabasePath();
        createTables(); // if not already exists
    
        NUMBER_OF_DAYS = Integer.valueOf(getSetting(NUMBER_OF_DAYS_NAME));
        calculateMaxColumns();
        NUMBER_OF_MOVING_DAYS = Integer.valueOf(getSetting(
                NUMBER_OF_MOVING_DAYS_NAME));
        
        focusDay = LocalDate.now(); // set focus day to today
        setupGridPane(focusDay);
        setupSettingsMenu();
        
        progressIndicator.setVisible(false);

        // Notice that the listener which listens for day changes is called from
        // Main, because it needs the primary Stage.
    
        prepareToggleSettings();
    }
    
    /**
     * sets up listviews for each day, initializes drag and drop, editing items
     * @param focusDate date that is the top middle one (is today on default)
     */
    private void setupGridPane(LocalDate focusDate) {
    
        AnchorPane.setTopAnchor(gridPane, toolBar.getPrefHeight());
    
        // first clear the gridpane so we don't get titles overlaying each other
        gridPane.getChildren().clear();
        for (int index = 0; index < NUMBER_OF_DAYS; index++) {
            
            // add days immediately, otherwise we can't use localDate in a
            // lambda expression (as it is not final)
            LocalDate localDate = focusDate.plusDays(index - 1);
            
            ListView<HomeworkTask> list = new ListView<>();
            VBox vbox = setTitle(list, localDate);
            addVBoxToGridPane(vbox, index);
            
            // Request content on a separate thread, and hope the content
            // will be set eventually.
            refreshDay(list, localDate);
            
            list.setEditable(true);
            list.setPrefWidth(getListViewWidth());
            list.setPrefHeight(getListViewHeight());
            setupLabelCells(list, localDate);
            //update database when editing is finished
            list.setOnEditCommit(event -> updateDatabase(
                    localDate, convertObservableToArrayList(list.getItems())));
            addDeleteKeyListener(list, localDate);
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
//        focusDay.plusDays(-1);
        today = LocalDate.now();
        primaryStage.focusedProperty().addListener((observable, wasFocused, isFocused) -> {
            if (isFocused) { // if becomes focused
                if (!today.equals(LocalDate.now())) {
                    // then reset view
                    today = LocalDate.now();
                    // Also update focusDay, before refreshing.
                    focusDay = today;
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
    private VBox setTitle(final ListView<HomeworkTask> list,
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
    private void addDeleteKeyListener(final ListView<HomeworkTask> list,
                                      final LocalDate localDate) {
        //add option to delete a task
        list.setOnKeyPressed(event -> {
            if (event.getCode() == KeyCode.DELETE) {
                list.getItems()
                        .remove(list.getSelectionModel().getSelectedIndex());
                updateDatabase(localDate,
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
    List<HomeworkTask> convertObservableToArrayList(
            final ObservableList<HomeworkTask> list) {
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
    private ObservableList<HomeworkTask> convertArrayToObservableList(
            final List<HomeworkTask> list) {
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
    private void setupLabelCells(final ListView<HomeworkTask> list, final LocalDate day) {
        //no idea why the callback needs a ListCell and not a TextFieldListCell
        //anyway, editing is enabled by using TextFieldListCell instead of
        // ListCell
        list.setCellFactory(new Callback<ListView<HomeworkTask>, ListCell<HomeworkTask>>() {
            @Override
            public LabelCell call(final ListView<HomeworkTask> param) {
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
    private List<ListView<HomeworkTask>> getAllListViews() {
        List<ListView<HomeworkTask>> listViews = new ArrayList<>();
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
     * Refreshes all listviews using data from the database.
     */
    void refreshAllDays() {
        // find all listviews
        List<ListView<HomeworkTask>> listViews = getAllListViews();
        
        for (int i = 0; i < NUMBER_OF_DAYS; i++) {
            ListView<HomeworkTask> list = listViews.get(i);
            // refresh the listview from database
            LocalDate localDate = focusDay.plusDays(i - 1);
            refreshDay(list, localDate);
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
                HomeworkTask homeworkTaskToRepeat = labelCell.getItem();
                repeatTask(repeatNumber, homeworkTaskToRepeat, day);
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
                                   final ListView<HomeworkTask> list,
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
                updateDatabase(day, convertObservableToArrayList(list.getItems()));
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
     * repeats a homeworkTask for a number of weeks
     *
     * @param repeatNumber how many times to repeat a homeworkTask
     * @param homeworkTask the homeworkTask to repeat
     * @param day the current day is needed to update the days on which the
     *            homeworkTask will be repeated
     */
    private void repeatTask(final int repeatNumber, final HomeworkTask homeworkTask, LocalDate day) {
        for (int i = 0; i < repeatNumber; i++) {
            day = day.plusWeeks(1);
            List<HomeworkTask> homeworkTasks = getDatabaseSynced(day);
            homeworkTasks.add(homeworkTask);
            updateDatabase(day, homeworkTasks);
        }
        refreshAllDays();
    }
    
    /**
     * removes empty rows, and then fills up with empty rows
     *
     * @param list
     *         to clean up
     */
    void cleanUp(ListView<HomeworkTask> list) {
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
                list.getItems().add(i, new HomeworkTask("", "", "White"));
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
        switch (colorName) {
            case "Green":
                return "#7ef202";
            case "Blue":
                return "#00cbef";
            case "Red":
                return "#ff2600";
            case "White" :
                return "#ffffffff";
            default:
                return "#ffffffff";
        }
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
    
        // EventHandler to close the settings pane when the user clicks
        // somewhere outside the settings pane
        EventHandler<MouseEvent> filter = event -> {
            // check if the region in the gridpane, outside the settings
            // pane is clicked
            if(!inHierarchy(event.getPickResult().getIntersectedNode(), settingsPane)) {
                // fire the settings button so it will close the settings
                // pane and remove this EventHandler
                settingsButton.fire();
                event.consume();
            }
        
        };
        
        settingsButton.setOnAction((ActionEvent evt)->{
            if(settingsPane.getTranslateX()!=0){
                
                // add the event filter to close the settings pane
                main.addEventFilter(MouseEvent.MOUSE_CLICKED, filter);
                
                setEnable(gridPane, false);
                openNav.play();
    
    
            }else{
                closeNav.setToX(-settingsPane.getWidth());
                closeNav.play();
                // remove the event filter to close the settings pane
                main.removeEventFilter(MouseEvent.MOUSE_CLICKED, filter);
                setEnable(gridPane, true);
            }
        });
        
    }
    
    /**
     * Sets up the content of the settings menu.
     */
    public void setupSettingsMenu() {
        
        AnchorPane.setTopAnchor(settingsPane, toolBar.getPrefHeight());
        addEditLabelsPane();
        addChangeNumberOfDaysSettings();
        
    }
    
    /**
     * Sets up the editable ListView to edit the labels/items we want to see
     * in the comboboxes on the main screen.
     */
    private void addEditLabelsPane() {
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
        // "magik numbers" are figured out by observations
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
     * Adds the Spinners to be able to change the number of days to move and
     * number of days to show to the settings pane.
     */
    private void addChangeNumberOfDaysSettings() {
        // Adding the spinner to change the number of days to move.
        numberOfMovingDaysSpinner = new Spinner<>();
        SpinnerValueFactory<Integer> valueFactory = new SpinnerValueFactory
                .IntegerSpinnerValueFactory(1, 14, NUMBER_OF_MOVING_DAYS);
    
        numberOfMovingDaysSpinner.setValueFactory(valueFactory);
        numberOfMovingDaysSpinner.setId("numberOfMovingDaysSpinner");
        numberOfMovingDaysSpinner.setPrefWidth(70);
        GridPane.setColumnIndex(numberOfMovingDaysSpinner, 1);
        editDaysPane.getChildren().add(numberOfMovingDaysSpinner);
    
        
        // Adding the spinner to change the number of days to show.
        numberOfShowDaysSpinner = new Spinner<>();
        // magik value 31 is the length of the longest month, just to be sure
        SpinnerValueFactory<Integer> valueShowFactory = new SpinnerValueFactory
                .IntegerSpinnerValueFactory(1, 31, NUMBER_OF_DAYS);
    
        numberOfShowDaysSpinner.setValueFactory(valueShowFactory);
        numberOfShowDaysSpinner.setId("numberOfShowDaysSpinner");
        numberOfShowDaysSpinner.setPrefWidth(70);
        GridPane.setColumnIndex(numberOfShowDaysSpinner, 1);
        GridPane.setRowIndex(numberOfShowDaysSpinner,1);
        editDaysPane.getChildren().add(numberOfShowDaysSpinner);
    }
    
    /**
     * Toggles the visibility of the listview with labels.
     */
    @FXML protected void editCourseLabels() {
        toggleVisibilityFXMLObject("labelsListView");
        toggleVisibilityFXMLObject("removeLabelButton");
        
        toggleYsettingsObject("editDaysPane");
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
     * Applies the value of the numberOfMovingDaysSpinner to the main
     * GridPane.
     */
    @FXML protected void applyNumberOfMovingDaysChange() {
        NUMBER_OF_MOVING_DAYS = numberOfMovingDaysSpinner.getValue();
        // update the value in the database
        updateSetting(NUMBER_OF_MOVING_DAYS_NAME,
                      String.valueOf(NUMBER_OF_MOVING_DAYS));
    
        setupGridPane(focusDay);
    }
    
    /**
     * Applies the value of the numberOfShowDaysSpinner to the main GridPane.
     */
    @FXML protected void applyNumberOfShowDaysChange() {
        NUMBER_OF_DAYS = numberOfShowDaysSpinner.getValue();
        calculateMaxColumns();
        
        updateSetting(NUMBER_OF_DAYS_NAME,
                      String.valueOf(NUMBER_OF_DAYS));
        setupGridPane(focusDay);
    
    }
    
    /**
     * Calculates and sets the value of MAX_COLUMNS
     */
    private void calculateMaxColumns() {
        MAX_COLUMNS = (int) Math.ceil(Math.sqrt(NUMBER_OF_DAYS));
    }
    
    /**
     * Toggles the visibility of an object.
     * @param id String with a FXML id of the object to be toggled.
     */
    private void toggleVisibilityFXMLObject(String id) {
        Boolean isVisible = editLabelsPane.lookup("#" + id).isVisible();
        editLabelsPane.lookup("#" + id).setVisible(!isVisible);
    }
    
    /**
     * Moves the FXML object up or down as much as the height the listview to
     * edit the labels needs.
     * @param id The fx:id of the object to be moved.
     */
    private void toggleYsettingsObject(String id) {
        Node node = settingsPane.lookup("#" + id);
        if(node.getTranslateY() == 0) {
            node.setTranslateY(editLabelsPane.getHeight());
        } else {
            node.setTranslateY(0);
        }
    }
    
    /*
     * End of settings ---------------------------------------------------------
     */
    
    /**
     * Returns whether a MouseEvent happened in a certain node or not.
     * @param node The node the event happened in.
     * @param potentialHierarchyElement The node to check if the event
     *                                  happened in.
     * @return True if the event happened in the checked node, false
     * otherwise.
     */
    public static boolean inHierarchy(Node node, Node potentialHierarchyElement) {
        if (potentialHierarchyElement == null) {
            return true;
        }
        while (node != null) {
            if (Objects.equals(node, potentialHierarchyElement)) {
                return true;
            }
            node = node.getParent();
        }
        return false;
    }
    
    //todo should these methods be in Database class?
    /**
     * Requests tasks from database, and when done updates the listview.
     *
     * @param list ListView to be updated.
     * @param localDate The day for which to request tasks.
     */
    public void refreshDay(ListView<HomeworkTask> list, LocalDate localDate) {
        progressIndicator.setVisible(true);
        Task<List<HomeworkTask>> task = new Task<List<HomeworkTask>>() {
            @Override
            public List<HomeworkTask> call() throws Exception {
                return getDatabaseSynced(localDate);
            }
        };
        task.setOnSucceeded(e -> {
            // Update the listview with the result from the database.
            list.setItems(convertArrayToObservableList(task.getValue()));
            cleanUp(list);
            progressIndicator.setVisible(false);
        });
        exec.execute(task);
    }
    
    /**
     * Updates database using the given homework tasks for a day.
     * @param day Date from which the tasks are.
     * @param homeworkTasks Tasks to be put in the database.
     */
    public void updateDatabase(LocalDate day, List<HomeworkTask> homeworkTasks) {
        progressIndicator.setVisible(true);
        Task<List<HomeworkTask>> task = new Task<List<HomeworkTask>>() {
            @Override
            public List<HomeworkTask> call() throws Exception {
                updateDatabaseSynced(day, homeworkTasks);
                return null;
            }
        };
        task.setOnSucceeded(e -> {
            progressIndicator.setVisible(false);
        });
        exec.execute(task);
    }
    
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
     * See {@link Database#createTables()}.
     */
    private void createTables() {
        Database.INSTANCE.createTables();
    }
    
    /**
     * See {@link Database#getTasksDay(LocalDate)}.
     * @param localDate Same.
     * @return Same.
     */
    private synchronized List<HomeworkTask> getDatabaseSynced(final LocalDate localDate) {
        return Database.INSTANCE.getTasksDay(localDate);
    }
    
    /**
     * See {@link Database#updateTasksDay(LocalDate, List)}
     * @param day Same.
     * @param homeworkTasks Same.
     */
    synchronized void updateDatabaseSynced(final LocalDate day,
                                             final List<HomeworkTask> homeworkTasks) {
        Database.INSTANCE.updateTasksDay(day, homeworkTasks);
    }
    
    /**
     * See {@link Database#getSetting(String)}
     * @param name Same.
     * @return Same.
     */
    private String getSetting(String name) {
        return Database.INSTANCE.getSetting(name);
    }
    
    /**
     * See {@link Database#updateSetting(String, String)}
     * @param name Same.
     * @param newValue Same.
     */
    private void updateSetting(String name, String newValue) {
        Database.INSTANCE.updateSetting(name, newValue);
    }
    
    /**
     * See {@link Database#getLabels()}.
     * @return Same.
     */
    private ArrayList<String> getLabels() {
        return Database.INSTANCE.getLabels();
    }
    
    /**
     * See {@link Database#updateLabel(int, String)}
     * @param id Same.
     * @param label Same.
     */
    private void updateLabel(int id, String label) {
        Database.INSTANCE.updateLabel(id, label);
    }
    
    /*
     * End of database methods
     */
    
    /**
     * Enables or disables a node.
     * @param node The node to enable to disable.
     * @param enable True if the node should be enabled, false if
     *               the node should be disabled.
     */
    private void setEnable(Node node, boolean enable) {
        node.setDisable(!enable);
    }
    
}

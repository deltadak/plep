package deltadak.ui;

import deltadak.Database;
import deltadak.HomeworkTask;
import deltadak.commands.DeleteCommand;
import deltadak.commands.UndoFacility;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.geometry.Rectangle2D;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.Label;
import javafx.scene.input.*;
import javafx.scene.layout.*;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.util.Callback;
import javafx.concurrent.Task;

import java.net.URL;
import java.time.LocalDate;
import java.util.List;
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
public class Controller implements Initializable, AbstractController {
    // main element of the UI is declared in interface.fxml
    @FXML AnchorPane main;
    @FXML GridPane gridPane;
    @FXML ToolBar toolBar;
    @FXML ProgressIndicator progressIndicator;

    // All these references have to be declared in controller because of fxml,
    // and then be passed on to the SlidingPane. Ah well.

    // help pane
    @FXML AnchorPane helpPane;
    @FXML Button helpButton;

    // settings pane
    @FXML AnchorPane settingsPane;
    @FXML Button settingsButton;

    @FXML GridPane editLabelsPane;
    @FXML Button editLabelsButton;
    @FXML GridPane editDaysPane;
    @FXML Button removeLabelButton;
    @FXML Button applyNumberOfDays;
    @FXML Button applyNumberOfShowDays;
    @FXML CheckBox autoColumnCheckBox;
    @FXML Button applyMaxColumns;

    /** used to transfer tasks with drag and drop */
    public static final DataFormat DATA_FORMAT = new DataFormat("com.deltadak.HomeworkTask");

    // layout globals, are public for the SettingsPane to access them
    /** number of days shown */
    public int NUMBER_OF_DAYS;
    /** number of days to skip when using the forward/backward buttons */
    public int NUMBER_OF_MOVING_DAYS;

    /** number of columns to fill with lists with tasks */
    public int MAX_COLUMNS;
    private static final int MAX_LIST_LENGTH = 7;

    /** name of setting in the database */
    public static final String NUMBER_OF_DAYS_NAME = "number_of_days";
    /** name of setting in the database */
    public static final String NUMBER_OF_MOVING_DAYS_NAME
            = "number_of_moving_days";
    /** name of setting in the database */
    public static final String MAX_COLUMNS_NAME = "max_columns";
    /** name of setting in the database */
    public static final String MAX_COLUMNS_AUTO_NAME = "max_columns_auto";

    /** Day on which the gridpane is 'focused': the second day shown will be this day */
    public LocalDate focusDay;
    private LocalDate today;
    // Multithreading
    private Executor exec;

    /** keep a reference to the undo facility */
    private UndoFacility undoFacility = new UndoFacility();

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

        // get the current settings from the database
        NUMBER_OF_DAYS = Integer.valueOf(getSetting(NUMBER_OF_DAYS_NAME));
        NUMBER_OF_MOVING_DAYS = Integer.valueOf(getSetting(
                NUMBER_OF_MOVING_DAYS_NAME));
        MAX_COLUMNS = Integer.valueOf(getSetting(MAX_COLUMNS_NAME));

        focusDay = LocalDate.now(); // set focus day to today
        setupGridPane(focusDay);

        progressIndicator.setVisible(false);

        // setup the settings page
        SlidingSettingsPane settingsPane = new SlidingSettingsPane(this);
        copySettingsPaneComponents(settingsPane);
        settingsPane.setup();

        // setup help page
        SlidingPane helpPane = new SlidingPane(this);
        copyHelpPageComponents(helpPane);
        helpPane.setup();

        // Notice that the listener which listens for day changes is called from
        // Main, because it needs the primary Stage.

        addUndoKeyListener();

    }

    /**
     * Copy references from fxml components needed to the SlidingSettingsPane
     * @param settingsPane which needs the references
     */
    private void copySettingsPaneComponents(SlidingSettingsPane settingsPane) {
        settingsPane.main = this.main;
        settingsPane.gridPane = this.gridPane;
        settingsPane.toolBar = this.toolBar;
        settingsPane.slidingPane = this.settingsPane;
        settingsPane.editDaysPane = this.editDaysPane;
        settingsPane.editLabelsPane = this.editLabelsPane;
        settingsPane.editLabelsButton = this.editLabelsButton;
        settingsPane.removeLabelButton = this.removeLabelButton;
        settingsPane.openCloseButton = this.settingsButton;
        settingsPane.applyNumberOfDays = this.applyNumberOfDays;
        settingsPane.applyNumberOfShowDays = this.applyNumberOfShowDays;
        settingsPane.autoColumnsCheckBox = this.autoColumnCheckBox;
        settingsPane.applyMaxColumns = this.applyMaxColumns;

    }

    /**
     * Copy references from fxml components needed to the SlidingPane
     * @param helpPane which needs the references
     */
    private void copyHelpPageComponents(SlidingPane helpPane) {
        helpPane.main = this.main;
        helpPane.gridPane = this.gridPane;
        helpPane.toolBar = this.toolBar;
        helpPane.slidingPane = this.helpPane;
        helpPane.openCloseButton = this.helpButton;
    }

    private void addUndoKeyListener() {
        gridPane.setOnKeyPressed(event -> {
            if (event.isControlDown() && (event.getCode() == KeyCode.Z)) {
                undoFacility.undo();
            }
        });
    }

    /**
     * sets up listviews for each day, initializes drag and drop, editing items
     *
     * @param focusDate date that is the top middle one (is today on default)
     */
    public void setupGridPane(LocalDate focusDate) {
        // check if the number of columns should be calculated, or retrieved
        // from the database
        boolean isAuto = Boolean.valueOf(
                getSetting(MAX_COLUMNS_AUTO_NAME));
        if(isAuto) {
            MAX_COLUMNS = maxColumns(NUMBER_OF_DAYS);
        } else {
            MAX_COLUMNS = Integer.valueOf(getSetting(MAX_COLUMNS_NAME));
        }

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
                    localDate, convertObservableListToArrayList(list.getItems())));
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
     * @param list      to use
     * @param localDate from which to make a title
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
     * @param vbox  to be added
     * @param index at the i'th place (left to right, top to bottom)
     */
    private void addVBoxToGridPane(final VBox vbox, final int index) {
        int row = index / MAX_COLUMNS;
        int column = index % MAX_COLUMNS;
        gridPane.add(vbox, column, row);
    }

    /**
     * Calculates and sets the value of MAX_COLUMNS
     * @param numberOfDays number of days in total
     * @return int for MAX_COLUMNS
     */
    private int maxColumns(int numberOfDays) {
        return (int) Math.ceil(Math.sqrt(numberOfDays));
    }

    /**
     * add a Listener to a list for the delete key
     *
     * @param list      ListView to add the Listener to
     * @param localDate so we know for what day to update the database
     */
    private void addDeleteKeyListener(final ListView<HomeworkTask> list,
                                      final LocalDate localDate) {
        //add option to delete a task
        list.setOnKeyPressed(event -> {
            if (event.getCode() == KeyCode.DELETE) {
                DeleteCommand command = new DeleteCommand(this, localDate,
                        convertObservableListToArrayList(list.getItems()), list.getSelectionModel().getSelectedIndex(), list);
                undoFacility.execute(command);

                cleanUp(list); //cleaning up has to happen in the listener
            }
        });
    }

    /**
     * convert ObservableList to ArrayList
     *
     * @param list to convert
     * @return converted ObservableList
     */
    public List<HomeworkTask> convertObservableListToArrayList(
            final ObservableList<HomeworkTask> list) {
        return new ArrayList<>(list);
    }

    /**
     * convert (Array)List to ObservableList
     *
     * @param list - List to be converted
     * @return ObservableList
     */
    private ObservableList<HomeworkTask> convertListToObservableList(
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
        int totalHeight = (int) primaryScreenBounds.getHeight();
        return totalHeight / (NUMBER_OF_DAYS / MAX_COLUMNS);
    }

    /**
     * get width by total screen size
     *
     * @return intended listview width
     */
    private int getListViewWidth() {
        Rectangle2D primaryScreenBounds = Screen.getPrimary().getVisualBounds();
        int totalWidth = (int) primaryScreenBounds.getWidth();
        return totalWidth / MAX_COLUMNS;
    }

    /**
     * sets up labelCells for
     *
     * @param list a ListView
     * @param day  and a specific date
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
                for (Node subNode : ((Pane) node).getChildren()) {
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
     *
     * @param labelCell task to repeat
     * @param day       the day to repeat
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
     *
     * @param event     show context menu at place of mouse event
     * @param labelCell to know which labelCell to color or repeat or ...
     * @param list      to update and cleanup after changing labelCell
     * @param day       needed for updating the database
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
                setBackgroundColor(colorMenuItem, labelCell);
                updateDatabase(day, convertObservableListToArrayList(list.getItems()));
                cleanUp(list);

            });
        }
        contextMenu.show(labelCell, event.getScreenX(), event.getScreenY());
    }

    /**
     * sets the background color of a LabelCell
     *
     * @param menuItem  MenuItem to retrieve the color from
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
     * @param day          the current day is needed to update the days on which the
     *                     homeworkTask will be repeated
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
     * @param list to clean up
     */
    public void cleanUp(ListView<HomeworkTask> list) {
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
    public String convertColorToHex(final String colorName) {
        switch (colorName) {
            case "Green":
                return "#7ef202";
            case "Blue":
                return "#00cbef";
            case "Red":
                return "#ff2600";
            case "White":
                return "#ffffffff";
            default:
                return "#ffffffff";
        }
    }

    /**
     * called by the backward button
     * moves the planner a (few) day(s) back
     */
    @FXML
    protected void dayBackward() {
        focusDay = focusDay.plusDays(-NUMBER_OF_MOVING_DAYS);
        setupGridPane(focusDay);
    }

    /**
     * called by the today button
     * focuses the planner on today
     */
    @FXML
    protected void goToToday() {
        focusDay = LocalDate.now();
        setupGridPane(focusDay);
    }

    /**
     * called by the forward button
     * moves the planner a (few) day(s) forward
     */
    @FXML
    protected void dayForward() {
        focusDay = focusDay.plusDays(NUMBER_OF_MOVING_DAYS);
        setupGridPane(focusDay);
    }

    //todo should these methods be in Database class?

    /**
     * Requests tasks from database, and when done updates the listview.
     *
     * @param list      ListView to be updated.
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
            list.setItems(convertListToObservableList(task.getValue()));
            cleanUp(list);
            progressIndicator.setVisible(false);
        });
        exec.execute(task);
    }

    /**
     * Updates database using the given homework tasks for a day.
     *
     * @param day           Date from which the tasks are.
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
     *
     * @param localDate Same.
     * @return Same.
     */
    private synchronized List<HomeworkTask> getDatabaseSynced(final LocalDate localDate) {
        return Database.INSTANCE.getTasksDay(localDate);
    }

    /**
     * See {@link Database#updateTasksDay(LocalDate, List)}
     *
     * @param day           Same.
     * @param homeworkTasks Same.
     */
    synchronized void updateDatabaseSynced(final LocalDate day,
                                           final List<HomeworkTask> homeworkTasks) {
        Database.INSTANCE.updateTasksDay(day, homeworkTasks);
    }

    /**
     * See {@link Database#getSetting(String)}
     *
     * @param name Same.
     * @return Same.
     */
    private String getSetting(String name) {
        return Database.INSTANCE.getSetting(name);
    }
}
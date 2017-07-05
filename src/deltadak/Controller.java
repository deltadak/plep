package deltadak;

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
import javafx.scene.text.Text;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.concurrent.Task;

import javax.xml.crypto.Data;
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

    private CustomSettingsPane settingsPane;

    // these have to be declared in controller because of fxml,
    // and then be passed on to the SettingsPane. Ah well.
    @FXML AnchorPane anchorPane;
    @FXML GridPane editLabelsPane;
    @FXML Button editLabelsButton;
    @FXML Button settingsButton;
    @FXML GridPane editDaysPane;
    @FXML Button removeLabelButton;
    @FXML Button applyNumberOfDays;
    @FXML Button applyNumberOfShowDays;
    @FXML CheckBox autoColumnCheckBox;
    @FXML Button applyMaxColumns;

    @FXML Text numberOfMovingDaysText;
    @FXML Text numberOfShowDaysText;
<<<<<<< HEAD
    
=======

    /** used to transfer tasks with drag and drop */
    static final DataFormat DATA_FORMAT = new DataFormat("com.deltadak.HomeworkTask");

>>>>>>> master
    // layout globals, are public for the SettingsPane to access them
    public int NUMBER_OF_DAYS; // number of days shown
    public int NUMBER_OF_MOVING_DAYS; // number of days to skip when using the forward/backward buttons

    public int MAX_COLUMNS; // number of columns to fill with lists with tasks
    private static final int MAX_LIST_LENGTH = 7;

    // name of setting in the database
    public static final String NUMBER_OF_DAYS_NAME = "number_of_days";
    public static final String NUMBER_OF_MOVING_DAYS_NAME
            = "number_of_moving_days";
<<<<<<< HEAD
    
    /** used to transfer tasks with drag and drop */
    static final DataFormat DATA_FORMAT = new DataFormat("com.deltadak.HomeworkTask");
=======
    public static final String MAX_COLUMNS_NAME = "max_columns";
    public static final String MAX_COLUMNS_AUTO_NAME = "max_columns_auto";
>>>>>>> master

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

        // setup the settings pange
        settingsPane = new CustomSettingsPane(this);
        copySettingsPaneComponents(settingsPane);
        settingsPane.setup();

        // Notice that the listener which listens for day changes is called from
        // Main, because it needs the primary Stage.

        addUndoKeyListener();

    }

    /**
     * Copy references from fxml components needed to the CustomSettingsPane
     * @param settingsPane which needs the references
     */
    private void copySettingsPaneComponents(CustomSettingsPane settingsPane) {
        settingsPane.main = this.main;
        settingsPane.gridPane = this.gridPane;
        settingsPane.toolBar = this.toolBar;
        settingsPane.settingsPane = this.anchorPane;
        settingsPane.editDaysPane = this.editDaysPane;
        settingsPane.editLabelsPane = this.editLabelsPane;
        settingsPane.editLabelsButton = this.editLabelsButton;
        settingsPane.removeLabelButton = this.removeLabelButton;
        settingsPane.settingsButton = this.settingsButton;
        settingsPane.applyNumberOfDays = this.applyNumberOfDays;
        settingsPane.applyNumberOfShowDays = this.applyNumberOfShowDays;
        settingsPane.autoColumnsCheckBox = this.autoColumnCheckBox;
        settingsPane.applyMaxColumns = this.applyMaxColumns;

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
            
//            ListView<HomeworkTask> list = new ListView<>();
            TreeItem<HomeworkTask> rootItem = new TreeItem<>(
                    new HomeworkTask(false, "root", "root", "white"));
            rootItem.setExpanded(true);
            final TreeView<HomeworkTask> tree = new TreeView<>(rootItem);
    
            tree.setEditable(true);
            tree.setCellFactory(param -> {
                CustomTreeCell treeCell = new CustomTreeCell(this, tree.getRoot());
                treeCell.setup(tree, localDate);
                return treeCell;
            });
            
            tree.setShowRoot(false);
            
            VBox vbox = setTitle(tree, localDate);
            addVBoxToGridPane(vbox, index);

            // Request content on a separate thread, and hope the content
            // will be set eventually.
<<<<<<< HEAD
            refreshDay(tree, localDate);
            
            // add the delete key listener
            addDeleteKeyListener(tree, localDate);

            tree.setPrefWidth(getListViewWidth());
            tree.setPrefHeight(getListViewHeight());
            // update the database when editing the text of a task is done
//            tree.setOnEditCommit(event -> {
//                System.out.println("edited");
//
//                // gets a list from the children of the root
//                // (the main tasks) and updates that list in the database
//                updateDatabase(localDate,
//                            convertTreeItemListToArrayList(
//                                tree.getRoot().getChildren()));
//            });
//            list.setEditable(true);
//            list.setPrefWidth(getListViewWidth());
//            list.setPrefHeight(getListViewHeight());
//            setupLabelCells(list, localDate);
//            //update database when editing is finished
//            list.setOnEditCommit(event -> updateDatabase(
//                    localDate, convertObservableToArrayList(list.getItems())));
//            addDeleteKeyListener(list, localDate);
=======
            refreshDay(list, localDate);

            list.setEditable(true);
            list.setPrefWidth(getListViewWidth());
            list.setPrefHeight(getListViewHeight());
            setupLabelCells(list, localDate);
            //update database when editing is finished
            list.setOnEditCommit(event -> updateDatabase(
                    localDate, convertObservableListToArrayList(list.getItems())));
            addDeleteKeyListener(list, localDate);
>>>>>>> master
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
     * @param tree      to use
     * @param localDate from which to make a title
     * @return VBox with listview and title
     */
    private VBox setTitle(final TreeView<HomeworkTask> tree,
                          final LocalDate localDate) {
        // vbox will contain a title above a list of tasks
        VBox vbox = new VBox();
        Label title = new Label(localDate.getDayOfWeek() + " " + localDate);
        // the pane is used to align both properly (I think)
        Pane pane = new Pane();
        vbox.getChildren().addAll(title, pane, tree);
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
     * @param tree      ListView to add the Listener to
     * @param localDate so we know for what day to update the database
     */
    private void addDeleteKeyListener(final TreeView<HomeworkTask> tree,
                                      final LocalDate localDate) {
        //add option to delete a task
        tree.setOnKeyPressed(event -> {
            if (event.getCode() == KeyCode.DELETE) {
<<<<<<< HEAD
                tree.getRoot().getChildren()
                        .remove(tree.getSelectionModel().getSelectedIndex());
                updateDatabase(localDate,
                        convertTreeItemListToArrayList(tree.getRoot().getChildren()));
                cleanUp(tree); //cleaning up has to happen in the listener
=======
                DeleteCommand command = new DeleteCommand(this, localDate,
                        convertObservableListToArrayList(list.getItems()), list.getSelectionModel().getSelectedIndex(), list);
                undoFacility.execute(command);

                cleanUp(list); //cleaning up has to happen in the listener
>>>>>>> master
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
    
    List<HomeworkTask> convertTreeItemListToArrayList(
            ObservableList<TreeItem<HomeworkTask>> list) {
        ArrayList<HomeworkTask> arrayList = new ArrayList<>();
        for (TreeItem<HomeworkTask> aList : list) {
            arrayList.add(aList.getValue());
        }
        return arrayList;
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
//    private void setupLabelCells(final ListView<HomeworkTask> list, final LocalDate day) {
//        //no idea why the callback needs a ListCell and not a TextFieldListCell
//        //anyway, editing is enabled by using TextFieldListCell instead of
//        // ListCell
//        list.setCellFactory(new Callback<ListView<HomeworkTask>, ListCell<HomeworkTask>>() {
//            @Override
//            public LabelCell call(final ListView<HomeworkTask> param) {
//                LabelCell labelCell = new LabelCell(Controller.this);
//                labelCell.setup(list, day);
//                return labelCell;
//            }
//        });
//        cleanUp(list);
//    }

    /**
     * @return all ListViews in the gridPane
     */
    private List<TreeView<HomeworkTask>> getAllTreeViews() {
        List<TreeView<HomeworkTask>> listViews = new ArrayList<>();
        for (Node node : gridPane.getChildren()) {
            //gridpane contains vbox contains label, pane and listview
            if (node instanceof VBox) {
                // we try to dig up the listviews in this vbox
                for (Node subNode : ((Pane) node).getChildren()) {
                    if (subNode instanceof TreeView) {
                        listViews.add((TreeView) subNode);
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
        // find all treeviews from the gridpane
        List<TreeView<HomeworkTask>> listViews = getAllTreeViews();

        for (int i = 0; i < NUMBER_OF_DAYS; i++) {
            TreeView<HomeworkTask> list = listViews.get(i);
            // refresh the listview from database
            LocalDate localDate = focusDay.plusDays(i - 1);
            refreshDay(list, localDate);
        }
<<<<<<< HEAD
        
=======
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
                System.out.println(colorMenuItem.getText() + " clicked");
                setBackgroundColor(colorMenuItem, labelCell);
                updateDatabase(day, convertObservableListToArrayList(list.getItems()));
                cleanUp(list);

            });
        }
        contextMenu.show(labelCell, event.getScreenX(), event.getScreenY());
>>>>>>> master
    }

    /**
     * sets the background color of a LabelCell
     *
     * @param menuItem  MenuItem to retrieve the color from
     * @param customTreeCell LabelCell of which to change the background color
     */
    public void setBackgroundColor(final MenuItem menuItem,
                                    final CustomTreeCell customTreeCell) {
        String colorWord = menuItem.getText();
        String colorString = convertColorToHex(colorWord);
        if (colorString.equals("#ffffffff")) {
            customTreeCell.setStyle("-fx-text-fill: none");
        } else {
            customTreeCell.setStyle(
                    "-fx-control-inner-background: "
                            + colorString);
        }
        customTreeCell.getItem().setColor(colorWord);

    }
    
    void cleanUp(TreeView<HomeworkTask> tree) {
        int i;
        TreeItem<HomeworkTask> root = tree.getRoot();
        for(i = 0; i < root.getChildren().size(); i++) {
            if(tree.getTreeItem(i).getValue().getText().equals("")) {
                removeItemFromTreeView(tree.getTreeItem(i));
            }
        }
    
        for(i = 0; i < MAX_LIST_LENGTH; i++) {
            if(i >= tree.getRoot().getChildren().size()) {
                TreeItem<HomeworkTask> item = new TreeItem<>(
                        new HomeworkTask(false,"", "", "White"));
                tree.getRoot().getChildren().add(item);
            }
        }
    }
    
    /**
     * Removes the TreeItem from the TreeView it's in.
     * @param item The TreeItem to be removed.
     */
    void removeItemFromTreeView(TreeItem<HomeworkTask> item) {
        item.getParent().getChildren().remove(item);
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
                list.getItems().add(i, new HomeworkTask(
                        false, "", "", "White"));
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
//        refreshAllDays();
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
     * @param tree      TreeView to be updated.
     * @param localDate The day for which to request tasks.
     */
    public void refreshDay(TreeView<HomeworkTask> tree, LocalDate localDate) {
        progressIndicator.setVisible(true);
        // get tasks from the database
        Task<List<HomeworkTask>> task = new Task<List<HomeworkTask>>() {
            @Override
            public List<HomeworkTask> call() throws Exception {
                return getDatabaseSynced(localDate);
            }
        };
        task.setOnSucceeded(e -> {
<<<<<<< HEAD
            // list with the homework tasks
            ObservableList<HomeworkTask> list =
                    convertArrayToObservableList(task.getValue());
            // clear all the items currently showing in the TreeView
            tree.getRoot().getChildren().clear();
            // add the items from the database to the TreeView
            for (HomeworkTask aList : list) {
                TreeItem<HomeworkTask> item = new TreeItem<>(aList);
                tree.getRoot().getChildren().add(item);
            }
            
            cleanUp(tree);
=======
            // Update the listview with the result from the database.
            list.setItems(convertListToObservableList(task.getValue()));
            cleanUp(list);
>>>>>>> master
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
    public synchronized List<HomeworkTask> getDatabaseSynced(final LocalDate
                                                                localDate) {
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
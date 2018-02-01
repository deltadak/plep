package deltadak.ui;

import deltadak.Database;
import deltadak.HomeworkTask;
import deltadak.commands.DeleteCommand;
import deltadak.commands.DeleteSubtaskCommand;
import deltadak.commands.UndoFacility;
import deltadak.ui.taskcell.TaskCell;
import javafx.application.Platform;
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
import javafx.concurrent.Task;

import java.net.URL;
import java.time.LocalDate;
import java.util.List;
import java.util.ResourceBundle;
import java.util.ArrayList;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;

import static deltadak.ui.util.STATIC.ConvertersKt.convertTreeToList;

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
    
    // All these references have to be declared in controller because of fxml.
    
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
    
    @FXML GridPane colorsPane;
    @FXML ColorPicker colorOne;
    @FXML ColorPicker colorTwo;
    @FXML ColorPicker colorThree;
    @FXML ColorPicker colorFour;
    @FXML ColorPicker colorFive;
    
    /**
     * used to transfer tasks with drag and drop
     */
    public static final DataFormat DATA_FORMAT = new DataFormat(
            "com.deltadak.HomeworkTask");
    
    // layout globals, are public for the SettingsPane to access them
    /**
     * number of days shown
     */
    public int numberOfDays;
    /**
     * number of days to skip when using the forward/backward buttons
     */
    public int numberOfMovingDays;
    
    /**
     * number of columns to fill with lists with tasks
     */
    public int maxColumns;
    private static final int MAX_LIST_LENGTH = 6;
    
    /**
     * name of setting in the database
     */
    public static final String NUMBER_OF_DAYS_NAME = "number_of_days";
    /**
     * name of setting in the database
     */
    public static final String NUMBER_OF_MOVING_DAYS_NAME
            = "number_of_moving_days";
    /**
     * name of setting in the database
     */
    public static final String MAX_COLUMNS_NAME = "max_columns";
    /**
     * name of setting in the database
     */
    public static final String MAX_COLUMNS_AUTO_NAME = "max_columns_auto";

    /** Default (initial) colors */
    public static final String[] DEFAULT_COLORS = new String[] {
            "ff1a00",
            "00cbef",
            "7df202",
            "f444a7",
            "ffffff"
    };
    
    /**
     * Day on which the gridpane is 'focused': the second day shown will be this
     * day
     */
    public LocalDate focusDay;
    private LocalDate today;
    // Multithreading
    private Executor exec;
    
    /**
     * keep a reference to the undo facility
     */
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
        
        numberOfDays = Integer.valueOf(getSetting(NUMBER_OF_DAYS_NAME));
        numberOfMovingDays = Integer
                .valueOf(getSetting(NUMBER_OF_MOVING_DAYS_NAME));
        
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
     * Copy references from fxml components needed to the SettingsPane
     *
     * @param settingsPane
     *         which needs the references
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
        settingsPane.colorsPane = this.colorsPane;
        settingsPane.colorOne = this.colorOne;
        settingsPane.colorTwo = this.colorTwo;
        settingsPane.colorThree = this.colorThree;
        settingsPane.colorFour = this.colorFour;
        settingsPane.colorFive = this.colorFive;
        
    }
    
    /**
     * Copy references from fxml components needed to the SlidingPane
     *
     * @param helpPane
     *         which needs the references
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
     * @param focusDate
     *         date that is the top middle one (is today on default)
     */
    public void setupGridPane(LocalDate focusDate) {
        
        boolean isAuto = Boolean.valueOf(getSetting(MAX_COLUMNS_AUTO_NAME));
        if (isAuto) {
            maxColumns = maxColumns(numberOfDays);
        } else {
            maxColumns = Integer.valueOf(getSetting(MAX_COLUMNS_NAME));
        }
        
        AnchorPane.setTopAnchor(gridPane, toolBar.getPrefHeight());
        
        // first clear the gridpane so we don't get titles overlaying each other
        gridPane.getChildren().clear();
        for (int index = 0; index < numberOfDays; index++) {
            
            // add days immediately, otherwise we can't use localDate in a
            // lambda expression (as it is not final)
            LocalDate localDate = focusDate.plusDays(index - 1);
            
            TreeItem<HomeworkTask> rootItem = new TreeItem<>(
                    new HomeworkTask());
            rootItem.setExpanded(true);
            final TreeView<HomeworkTask> tree = new TreeView<>(rootItem);
            
            tree.setEditable(true);
            tree.setCellFactory(param -> {
                TaskCell treeCell = new TaskCell(this,
                                                             tree.getRoot());
                treeCell.setup(tree, localDate);
                return treeCell;
            });
            
            tree.setShowRoot(false);
            
            VBox vbox = setTitle(tree, localDate);
            addVBoxToGridPane(vbox, index);
            
            // Request content on a separate thread, and hope the content
            // will be set eventually.
            refreshDay(tree, localDate);
            
            // add the delete key listener
            addDeleteKeyListener(tree, localDate);
            
            tree.setPrefWidth(getTreeViewWidth());
            tree.setPrefHeight(getTreeViewHeight());
        }
        
    }
    
    /**
     * Sets a listener which checks if it is a new day, when the window becomes
     * focused.
     *
     * @param primaryStage
     *         Stage to set listener on.
     */
    public void setDayChangeListener(Stage primaryStage) {
        // debug line, also comment out the line to reset 'today'
        //        today = LocalDate.now().plusDays(-1);
        //        focusDay.plusDays(-1);
        today = LocalDate.now();
        primaryStage.focusedProperty()
                .addListener((observable, wasFocused, isFocused) -> {
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
     * @param tree
     *         to use
     * @param localDate
     *         from which to make a title
     *
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
     * @param vbox
     *         to be added
     * @param index
     *         at the i'th place (left to right, top to bottom)
     */
    private void addVBoxToGridPane(final VBox vbox, final int index) {
        int row = index / maxColumns;
        int column = index % maxColumns;
        gridPane.add(vbox, column, row);
    }
    
    /**
     * Calculates and sets the value of maxColumns
     *
     * @param numberOfDays
     *         number of days in total
     *
     * @return int for maxColumns
     */
    public int maxColumns(int numberOfDays) {
        return (int)Math.ceil(Math.sqrt(numberOfDays));
    }
    
    /**
     * add a Listener to a list for the delete key
     *
     * @param tree
     *         ListView to add the Listener to
     * @param localDate
     *         so we know for what day to update the database
     */
    private void addDeleteKeyListener(final TreeView<HomeworkTask> tree,
                                      final LocalDate localDate) {
        //add option to delete a task
        tree.setOnKeyPressed(event -> {
            
            if (event.getCode() == KeyCode.DELETE) {
                // Check whether we want to delete a parent task or subtask.
                if (tree.getSelectionModel().getSelectedItem().getParent()
                        .equals(tree.getRoot())) {
                    // Delete a parent task.
                    deleteParentTask(tree, localDate);
                } else {
                    // We want to delete a subtask.
                    deleteSubtask(tree, localDate);
                }
                
            }
        });
    }
    
    /**
     * Delete the selected parent task, and save to database.
     *
     * @param tree
     *         TreeView in which the task lives.
     * @param localDate
     *         Date of the TreeView.
     */
    private void deleteParentTask(TreeView<HomeworkTask> tree,
                                  LocalDate localDate) {
        
        // Get the selected item.
        TreeItem<HomeworkTask> selectedItem = tree.getSelectionModel()
                .getSelectedItem();
        // Get the index of the selected item.
        // Note: using selectionModel().getSelectedIndex() returns the index
        // when also counting subtasks, so this does not work.
        int parentIndex = tree.getRoot().getChildren().indexOf(selectedItem);
        
        undoFacility.execute(
                new DeleteCommand(this, localDate, convertTreeToList(tree),
                                  parentIndex, tree));
        
    }
    
    /**
     * Delete the selected subtask.
     *
     * @param tree
     *         TreeView in which the task lives.
     * @param localDate
     *         Date of the TreeView.
     */
    private void deleteSubtask(TreeView<HomeworkTask> tree,
                               LocalDate localDate) {
        
        undoFacility.execute(
                new DeleteSubtaskCommand(
                        this,
                        localDate,
                        convertTreeToList(tree),
                        tree.getSelectionModel() .getSelectedIndex(),
                        tree));
    }
    
    /**
     * Get index of parent in the list of TreeItems.
     *
     * @param tree
     *         TreeView which contains the parent
     * @param parentItem
     *         The item to find.
     *
     * @return the index of the parent in the treeview.
     */
    private int getParentIndex(TreeView<HomeworkTask> tree,
                               TreeItem<HomeworkTask> parentItem) {
        ObservableList<TreeItem<HomeworkTask>> parentList = tree.getRoot()
                .getChildren();
        int parentIndex = 0;
        for (int i = 0; i < parentList.size(); i++) {
            if (parentList.get(i).equals(parentItem)) {
                parentIndex = i;
            }
        }
        return parentIndex;
    }
    
    /**
     * convert ObservableList to ArrayList
     *
     * @param list
     *         to convert
     *
     * @return converted ObservableList
     */
    public List<HomeworkTask> convertObservableListToArrayList(
            final ObservableList<HomeworkTask> list) {
        return new ArrayList<>(list);
    }
    
    /**
     * Get all the parents (or head tasks) when given a list of lists of
     * HomeworkTasks.
     *
     * @param homeworkFamilies
     *         The list of lists to get the parent tasks from.
     *
     * @return A list of HomeworkTasks, which are the parent tasks.
     */
    public List<HomeworkTask> getParentTasks(
            List<List<HomeworkTask>> homeworkFamilies) {
        
        // create the list with HomeworkTasks to return
        List<HomeworkTask> parentTasks = new ArrayList<>();
        
        // add the first item of each list to parentTasks
        for (List<HomeworkTask> homeworkFamily : homeworkFamilies) {
            parentTasks.add(homeworkFamily.get(0));
        }
        return parentTasks;
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
     * @return intended treeview height
     */
    private int getTreeViewHeight() {
        Rectangle2D primaryScreenBounds = Screen.getPrimary().getVisualBounds();
        int totalHeight = (int)primaryScreenBounds.getHeight();
        return totalHeight / (numberOfDays / maxColumns);
    }
    
    /**
     * get width by total screen size
     *
     * @return intended treeview width
     */
    private int getTreeViewWidth() {
        Rectangle2D primaryScreenBounds = Screen.getPrimary().getVisualBounds();
        int totalWidth = (int)primaryScreenBounds.getWidth();
        return totalWidth / maxColumns;
    }
    
    /**
     * @return all TreeViews in the gridPane
     */
    private List<TreeView<HomeworkTask>> getAllTreeViews() {
        List<TreeView<HomeworkTask>> listViews = new ArrayList<>();
        for (Node node : gridPane.getChildren()) {
            //gridpane contains vbox contains label, pane and treeview
            if (node instanceof VBox) {
                // we try to dig up the treeview in this vbox
                for (Node subNode : ((Pane)node).getChildren()) {
                    if (subNode instanceof TreeView) {
                        listViews.add((TreeView<HomeworkTask>)subNode);
                    }
                }
            }
        }
        return listViews;
    }
    
    /**
     * Refreshes all treeviews using data from the database.
     */
    public void refreshAllDays() {
        // Use this so updating the UI works like it should, and the JavaFX
        // Application thread doesn't hang.
        Platform.runLater(() -> {
            // find all treeviews from the gridpane
            List<TreeView<HomeworkTask>> treeViews = getAllTreeViews();
            
            for (int i = 0; i < numberOfDays; i++) {
                TreeView<HomeworkTask> tree = treeViews.get(i);
                // create a list to store if the items are expanded
                List<Boolean> expanded = new ArrayList<>();
                
                for (int j = 0; j < tree.getRoot().getChildren().size(); j++) {
                    // loop through the tree to add all the booleans
                    expanded.add(
                            tree.getRoot().getChildren().get(j).isExpanded());
                }
                
                // refresh the treeview from database
                LocalDate localDate = focusDay.plusDays(i - 1);
                refreshDay(tree, localDate);
            }
            
        });
        
    }
    
    /**
     * Sets the background color of a LabelCell.
     *
     * @param colorID
     *         ID of the color to set as background color.
     * @param taskCell
     *         LabelCell of which to change the background color.
     */
    public void setBackgroundColor(int colorID, TaskCell taskCell) {
        
        Platform.runLater(() -> {
            
            String colorString = getColorFromDatabase(colorID);
            
            if (colorID == 4) {
                taskCell.setStyle("-fx-text-fill: none");
            } else {
                taskCell.setStyle(
                        "-fx-control-inner-background: #" + colorString);
            }
            
            taskCell.getItem().setColorID(colorID);
            
        });
        
    }
    
    /**
     * Removes empty items in the tree view, and then fills it up with empty
     * items. To avoid gaps.
     *
     * @param tree
     *         The treeview to be cleaned up.
     */
    @Override
    public void cleanUp(TreeView<HomeworkTask> tree) {
        int i;
        TreeItem<HomeworkTask> root = tree.getRoot();
        for (i = 0; i < root.getChildren().size(); i++) {
            if (tree.getTreeItem(i).getValue().getText().equals("")) {
                removeItemFromTreeView(tree.getTreeItem(i));
            } else {
                // If there are subtasks, make sure there is one empty one.

            }

        }
        
        for (i = 0; i < MAX_LIST_LENGTH; i++) {
            if (i >= tree.getRoot().getChildren().size()) {
                TreeItem<HomeworkTask> item = new TreeItem<>(
                        new HomeworkTask());
                tree.getRoot().getChildren().add(item);
            }
        }
    }
    
    /**
     * Removes the TreeItem from the TreeView it's in.
     *
     * @param item
     *         The TreeItem to be removed.
     */
    void removeItemFromTreeView(TreeItem<HomeworkTask> item) {
        item.getParent().getChildren().remove(item);
    }
    
    /**
     * converts a String containing a color (e.g. Green) to a String with the
     * hex code of that color, so the styling can use it
     *
     * @param colorName
     *         String containing the color
     *
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
     * called by the backward button moves the planner a (few) day(s) back
     */
    @FXML
    protected void dayBackward() {
        focusDay = focusDay.plusDays(-numberOfMovingDays);
        setupGridPane(focusDay);
    }
    
    /**
     * called by the today button focuses the planner on today
     */
    @FXML
    protected void goToToday() {
        //        refreshAllDays();
        focusDay = LocalDate.now();
        setupGridPane(focusDay);
    }
    
    /**
     * called by the forward button moves the planner a (few) day(s) forward
     */
    @FXML
    protected void dayForward() {
        focusDay = focusDay.plusDays(numberOfMovingDays);
        setupGridPane(focusDay);
    }
    
    /**
     * Requests tasks from database, and when done updates the treeview.
     *
     * @param tree
     *         TreeView to be updated.
     * @param localDate
     *         The day for which to request tasks.
     */
    public void refreshDay(TreeView<HomeworkTask> tree, LocalDate localDate) {
        progressIndicator.setVisible(true);
        
        Task task = new Task() {
            @Override
            public Boolean call() throws Exception {
                // below two database accessors take noticable time
                
                // get tasks from the database
                List<List<HomeworkTask>> allTasks = getDatabaseSynced(
                        localDate);
                // get the homework task ids and their corresponding expanded
                // state from the database, as tuples
                //                List<Map.Entry<Integer, Boolean>>
                // allExpandedTasks = getExpandedFromDatabase();
                // list with the parent tasks
                ObservableList<HomeworkTask> list
                        = convertArrayToObservableList(
                        getParentTasks(allTasks));
                // clear all the items currently showing in the TreeView
                tree.getRoot().getChildren().clear();
                
                // add the items from the database to the TreeView
                for (int i = 0; i < list.size(); i++) {
                    // add the parent task to the tree
                    TreeItem<HomeworkTask> item = new TreeItem<>(list.get(i));
                    tree.getRoot().getChildren().add(item);
                    
                    item.setExpanded(item.getValue().getExpanded());
                    
                    int orderInDay = i;
                    // When expanded state changes, save to database.
                    item.expandedProperty()
                            .addListener((observable, oldValue, newValue) -> {
                                // update the task in the database, with the
                                // new value for expanded
                                HomeworkTask task = item.getValue();
                                task.setExpanded(newValue);
                                insertOrUpdateTask(localDate, task, orderInDay);
                            });
                    
                    // get the size of the current family, or the number of
                    // subtasks + 1 (the parent)
                    int familySize = allTasks.get(i).size();
                    // add every subtask to the tree as a child of the parent
                    // task
                    // we start at j=1 because the first item is the parent task
                    for (int j = 1; j < familySize; j++) {
                        // get the subtask
                        TreeItem<HomeworkTask> childTask = new TreeItem<>(
                                allTasks.get(i).get(j));
                        // add the subtask
                        tree.getRoot().getChildren().get(i).getChildren()
                                .add(childTask);
                    }
                    
                    // Insert an empty subtask at the end to allow the user
                    // to easily add more.
                    if (familySize > 1) {
                        tree.getRoot().getChildren().get(i).getChildren()
                                .add(new TreeItem<>(new HomeworkTask()));
                    }
                }
                
                return true;
            }
        };
        
        task.setOnSucceeded(e -> {
            cleanUp(tree);
            progressIndicator.setVisible(false);
            
        });
        
        exec.execute(task);
    }
    
    /**
     * Gets a TreeItem from the database, using its id.
     *
     * @param tree
     *         The tree to search for the tree item.
     * @param id
     *         The id of the tree item.
     *
     * @return TreeItem<HomeworkTask>
     */
    private TreeItem<HomeworkTask> findTreeItemById(TreeView<HomeworkTask> tree,
                                                    int id) {
        
        List<TreeItem<HomeworkTask>> parents = tree.getRoot().getChildren();
        
        for (TreeItem<HomeworkTask> parent : parents) {
            if (parent.getValue().getDatabaseID() == id) {
                return parent;
            }
        }
        return null;
    }
    
    /**
     * Update the parent tasks in the database. Used after dragging a task, we
     * only have to update the parents, because the subtasks only depend on
     * their parents, and are independent of the day and the order in the day.
     *
     * @param day
     *         The day of which to update the tasks.
     * @param parentTasks
     *         The list with parents to update.
     */
    public void updateParentDatabase(LocalDate day,
                                     List<HomeworkTask> parentTasks) {
        progressIndicator.setVisible(true);
        Task<HomeworkTask> task = new Task<HomeworkTask>() {
            @Override
            public HomeworkTask call() throws Exception {
                updateParentsSynced(day, parentTasks);
                return null;
            }
        };
        task.setOnSucceeded(e -> progressIndicator.setVisible(false));
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
     * @param localDate
     *         Same.
     *
     * @return Same.
     */
    public synchronized List<List<HomeworkTask>> getDatabaseSynced(
            final LocalDate localDate) {
        return Database.INSTANCE.getTasksDay(localDate);
    }
    
    /**
     * See {@link Database#updateParentsDay(LocalDate, List)}
     *
     * @param day
     *         Same.
     * @param parentTasks
     *         Same.
     */
    synchronized void updateParentsSynced(final LocalDate day,
                                          final List<HomeworkTask> parentTasks) {
        Database.INSTANCE.updateParentsDay(day, parentTasks);
    }
    
    /**
     * See {@link Database#insertOrUpdateTask(LocalDate, HomeworkTask, int)}
     *
     * @param day
     *         Same.
     * @param task
     *         Same.
     * @param orderInDay
     *         Same.
     */
    synchronized void insertOrUpdateTask(final LocalDate day,
                                         final HomeworkTask task,
                                         final int orderInDay) {
        Database.INSTANCE.insertOrUpdateTask(day, task, orderInDay);
    }
    
    /**
     * See {@link Database#getParentTasksDay(LocalDate)}
     *
     * @param day
     *         Same.
     *
     * @return Same.
     */
    public List<HomeworkTask> getParentTasksDay(final LocalDate day) {
        return Database.INSTANCE.getParentTasksDay(day);
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

    /**
     * See {@link Database#getColorFromDatabase(int)}
     * @param colorID same
     * @return same
     */
    public String getColorFromDatabase(int colorID) {
        return Database.INSTANCE.getColorFromDatabase(colorID);
    }

    /**
     * Getters for the fxml references.
     */
    public ProgressIndicator getProgressIndicator() {
        return this.progressIndicator;
    }
}
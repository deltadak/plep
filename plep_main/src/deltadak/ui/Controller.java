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
import javafx.concurrent.Task;

import javax.lang.model.type.ArrayType;
import javax.xml.crypto.Data;
import java.net.URL;
import java.time.LocalDate;
import java.util.List;
import java.util.ResourceBundle;
import java.util.ArrayList;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

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

        NUMBER_OF_DAYS = Integer.valueOf(getSetting(NUMBER_OF_DAYS_NAME));
        NUMBER_OF_MOVING_DAYS = Integer.valueOf(getSetting(
                NUMBER_OF_MOVING_DAYS_NAME));

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
                    new HomeworkTask(false, "root", "root", "white", -1));
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
                tree.getRoot().getChildren()
                        .remove(tree.getSelectionModel().getSelectedIndex());
                updateDatabase(localDate,
                        convertTreeToArrayList(tree));
                cleanUp(tree); //cleaning up has to happen in the listener
            }
        });
    }

    /**
     * convert ObservableList to ArrayList
     *
     * @param list to convert
     * @return converted ObservableList
     */
    List<HomeworkTask> convertObservableToArrayList(
            final ObservableList<HomeworkTask> list) {
        return new ArrayList<>(list);
    }
    
    /**
     * Convert TreeItemList to ArrayList.
     * @param list to convert
     * @return converted ArrayList
     */
    List<HomeworkTask> convertTreeItemListToArrayList(
            ObservableList<TreeItem<HomeworkTask>> list) {
        
        ArrayList<HomeworkTask> arrayList = new ArrayList<>();
        
        for (TreeItem<HomeworkTask> aList : list) {
            arrayList.add(aList.getValue());
        }
        
        return arrayList;
    }
    
    List<List<HomeworkTask>> convertTreeToArrayList
            (TreeView<HomeworkTask> tree) {
        
        // create a list with the tree items of the parent tasks
        ObservableList<TreeItem<HomeworkTask>> parentItems =
                tree.getRoot().getChildren();
        // create a list with homework tasks of the parent tasks
        List<HomeworkTask> parentTasks = 
                convertTreeItemListToArrayList(parentItems);
        
        // create the list to eventually return
        List<List<HomeworkTask>> tasks = new ArrayList<>();
        
        
        for (int i = 0; i < parentItems.size(); i++) {
            
            // get the sub tree items of parent task i, and store them in a list
            ObservableList<TreeItem<HomeworkTask>> childItems = parentItems.get(i).getChildren();
            // store the subtasks of parent task i in a list
            List<HomeworkTask> childTasks =
                    convertTreeItemListToArrayList(childItems);
            
            // create a list containing one parent and its children
            List<HomeworkTask> oneFamily = new ArrayList<>();
            
            oneFamily.add(parentTasks.get(i)); // add the parent to the family
            oneFamily.addAll(childTasks); // add its children to the family
            
            tasks.add(oneFamily); // add the family to the nested list of tasks
        }
        
        return tasks;
    }
    
    private List<HomeworkTask> getParentTasks(List<List<HomeworkTask>> homeworkFamilies) {
        List<HomeworkTask> parentTasks = new ArrayList<>();
        for (int i = 0; i < homeworkFamilies.size(); i++) {
            parentTasks.add(homeworkFamilies.get(i).get(0));
        }
        return parentTasks;
    }
    

    /**
     * convert (Array)List to ObservableList
     *
     * @param list - List to be converted
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
            cleanUp(list); // TODO wasn't in subtasks branch, do we need it?
        }
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
    
    /**
     * Removes empty items in the tree view, and then fills it up with empty
     * items. To avoid gaps.
     *
     * @param tree The treeview to be cleaned up.
     */
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
                        new HomeworkTask(false,"", "", "White", -1));
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
                list.getItems().add(i, new HomeworkTask(
                        false, "", "", "White", -1));
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
     * Requests tasks from database, and when done updates the treeview.
     *
     * @param tree      TreeView to be updated.
     * @param localDate The day for which to request tasks.
     */
    public void refreshDay(TreeView<HomeworkTask> tree, LocalDate localDate) {
        progressIndicator.setVisible(true);
        // get tasks from the database
        Task<List<List<HomeworkTask>>> task = new Task<List<List<HomeworkTask>>>() {
            @Override
            public List<List<HomeworkTask>> call() throws Exception {
                return getDatabaseSynced(localDate);
            }
        };
        
        task.setOnSucceeded(e -> {
            // list with the parent tasks
            ObservableList<HomeworkTask> list =
                    convertArrayToObservableList(getParentTasks(task.getValue()));
            // clear all the items currently showing in the TreeView
            tree.getRoot().getChildren().clear();
            
            // add the items from the database to the TreeView
            for (int i = 0; i < list.size(); i++) {
                // add the parent task to the tree
                TreeItem<HomeworkTask> item = new TreeItem<>(list.get(i));
                tree.getRoot().getChildren().add(item);
                
                int familySize = task.getValue().get(i).size();
                
                for (int j = 1; j < familySize; j++) {
                    TreeItem<HomeworkTask> childTask = new TreeItem<>(
                            task.getValue().get(i).get(j));
                    tree.getRoot().getChildren().get(i).getChildren().add
                            (childTask);
                }
            }
            
            cleanUp(tree);
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
    public void updateDatabase(LocalDate day, List<List<HomeworkTask>> homeworkTasks) {
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
    public synchronized List<List<HomeworkTask>> getDatabaseSynced(
            final LocalDate localDate) {
        return Database.INSTANCE.getTasksDay(localDate);
    }

    /**
     * See {@link Database#updateTasksDay(LocalDate, List)}
     *
     * @param day           Same.
     * @param homeworkTasks Same.
     */
    synchronized void updateDatabaseSynced(final LocalDate day,
                                           final List<List<HomeworkTask>> homeworkTasks) {
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
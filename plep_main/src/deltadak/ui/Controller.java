package deltadak.ui;

import deltadak.Database;
import deltadak.HomeworkTask;
import deltadak.commands.DeleteCommand;
import deltadak.commands.DeleteSubtaskCommand;
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

import java.net.URL;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
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
    public int numberOfDays;
    /** number of days to skip when using the forward/backward buttons */
    public int numberOfMovingDays;

    /** number of columns to fill with lists with tasks */
    public int maxColumns;
    private static final int MAX_LIST_LENGTH = 6;

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
    /** Multithreading */
    public Executor exec;

    /** The GridPane which contains everything. */
    private MainGridPane mainGridPane;


    /**
     * Initialization method for the controller.
     */
    @Override
    @FXML
    public void initialize(final URL location,
                           final ResourceBundle resourceBundle) {
        
        mainGridPane = new MainGridPane(this, gridPane, toolBar, progressIndicator);

        // Initialize multithreading.
        exec = Executors.newCachedThreadPool(runnable -> {
            Thread t = new Thread(runnable);
            t.setDaemon(true);
            return t;
        });

        setDefaultDatabasePath();
        createTables(); // if not already exists

        numberOfDays = Integer.valueOf(getSetting(NUMBER_OF_DAYS_NAME));
        numberOfMovingDays = Integer.valueOf(getSetting(
                NUMBER_OF_MOVING_DAYS_NAME));

        focusDay = LocalDate.now(); // set focus day to today
        setupGridPane(focusDay, numberOfDays);

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

    /**
     * See {@link MainGridPane#setup(LocalDate, int)}
     * @param focusDate Same.
     * @param numberOfDays Same.
     */
    public void setupGridPane(LocalDate focusDate, int numberOfDays) {
        mainGridPane.setup(focusDate, numberOfDays);
    }

    /**
     * See {@link MainGridPane#setup(LocalDate, int)}
     */
    public void setupGridPane() {
        mainGridPane.setup(focusDay, numberOfDays);
    }

    /**
     * Refreshes all listviews using data from the database.
     */
    void refreshAllDays() {
        mainGridPane.refreshAllDays(numberOfDays, focusDay);
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
                    setupGridPane(today, numberOfDays);
                }
            }
        });
    }

    /**
     * Calculates and sets the value of maxColumns
     * @param numberOfDays number of days in total
     * @return int for maxColumns
     */
    public static int maxColumns(int numberOfDays) {
        return (int) Math.ceil(Math.sqrt(numberOfDays));
    }

    /**
     * Get index of parent in the list of TreeItems.
     * @param tree TreeView which contains the parent
     * @param parentItem The item to find.
     * @return the index of the parent in the treeview.
     */
    private int getParentIndex(TreeView<HomeworkTask> tree, TreeItem<HomeworkTask> parentItem) {
        ObservableList<TreeItem<HomeworkTask>> parentList = tree.getRoot().getChildren();
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
     * @param list to convert
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
     * @param homeworkFamilies The list of lists to get the parent tasks from.
     * @return A list of HomeworkTasks, which are the parent tasks.
     */
    public static List<HomeworkTask> getParentTasks(List<List<HomeworkTask>> homeworkFamilies) {

        // create the list with HomeworkTasks to return
        List<HomeworkTask> parentTasks = new ArrayList<>();

        // add the first item of each list to parentTasks
        for (List<HomeworkTask> homeworkFamily : homeworkFamilies) {
            parentTasks.add(homeworkFamily.get(0));
        }
        return parentTasks;
    }

    /**
     * sets the background color of a LabelCell
     *
     * @param colorWord the color in English, with capital. e.g. Green
     * @param customTreeCell LabelCell of which to change the background color
     */
    public void setBackgroundColor(String colorWord,
                                    CustomTreeCell customTreeCell) {
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
    @Override
    public void cleanUp(TreeView<HomeworkTask> tree) {
        int i;
        TreeItem<HomeworkTask> root = tree.getRoot();
        for(i = 0; i < root.getChildren().size(); i++) {
            if(tree.getTreeItem(i).getValue().getText().equals("")) {
                removeItemFromTreeView(tree.getTreeItem(i));
            }
        }

        for(i = 0; i < MAX_LIST_LENGTH; i++) {
            if(i >= tree.getRoot().getChildren().size()) {
                TreeItem<HomeworkTask> item = new TreeItem<>(new HomeworkTask());
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
        focusDay = focusDay.plusDays(-numberOfMovingDays);
        setupGridPane(focusDay, numberOfDays);
    }

    /**
     * called by the today button
     * focuses the planner on today
     */
    @FXML
    protected void goToToday() {
//        refreshAllDays();
        focusDay = LocalDate.now();
        setupGridPane(focusDay, numberOfDays);
    }

    /**
     * called by the forward button
     * moves the planner a (few) day(s) forward
     */
    @FXML
    protected void dayForward() {
        focusDay = focusDay.plusDays(numberOfMovingDays);
        setupGridPane(focusDay, numberOfDays);
    }

    /**
     * Update the parent tasks in the database.
     * Used after dragging a task, we only have to update the parents,
     * because the subtasks only depend on their parents, and are
     * independent of the day and the order in the day.
     *
     * @param day The day of which to update the tasks.
     * @param parentTasks The list with parents to update.
     */
    public void updateParentDatabase(LocalDate day, List<HomeworkTask> parentTasks) {
        progressIndicator.setVisible(true);
        Task<HomeworkTask> task =  new Task<HomeworkTask>() {
            @Override
            public HomeworkTask call() throws Exception {
                updateParentsSynced(day, parentTasks);
                return null;
            }
        };
        task.setOnSucceeded(e -> progressIndicator.setVisible(false));
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
     * See {@link Database#updateTasksDay(LocalDate, List)}
     *
     * @param day           Same.
     * @param homeworkTasks Same.
     */
    synchronized void updateDatabaseSynced(final LocalDate day, final List<List<HomeworkTask>> homeworkTasks) {
        Database.INSTANCE.updateTasksDay(day, homeworkTasks);
    }
    
    /**
     * See {@link Database#updateParentsDay(LocalDate, List)}
     *
     * @param day         Same.
     * @param parentTasks Same.
     */
    synchronized void updateParentsSynced(final LocalDate day, final List<HomeworkTask> parentTasks) {
        Database.INSTANCE.updateParentsDay(day, parentTasks);
    }

    /**
     * See {@link Database#getParentTasksDay(LocalDate)}
     *
     * @param day Same.
     * @return Same.
     */
    public List<HomeworkTask> getParentTasksDay(final LocalDate day) {
        return Database.INSTANCE.getParentTasksDay(day);
    }

    /**
     * See {@link Database#deleteExpanded(int)}
     *
     * @param id Same.
     */
    @Override
    public void deleteExpanded(int id) {
        Database.INSTANCE.deleteExpanded(id);
    }

    /**
     * See {@link Database#insertTask(LocalDate, HomeworkTask, int)}
     *
     * @param id Same.
     * @param expanded Same.
     */
    @Override
    public void insertExpandedItem(int id, boolean expanded) {
        Database.INSTANCE.insertExpandedItem(id, expanded);
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
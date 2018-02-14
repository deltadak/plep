package deltadak.ui;

import deltadak.Database;
import deltadak.HomeworkTask;
import deltadak.commands.UndoFacility;
import deltadak.database.DatabaseSettings;
import deltadak.ui.gridpane.GridPaneInitializer;
import deltadak.ui.taskcell.TaskCell;
import javafx.application.Platform;
import javafx.collections.ObservableList;
import javafx.concurrent.Task;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.input.DataFormat;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.stage.Stage;

import java.net.URL;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;
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
    /** The main GridPane which contains everything. */
    @FXML public GridPane gridPane;
    /** Top toolbar with buttons. */
    @FXML public ToolBar toolBar;
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
    public UndoFacility undoFacility = new UndoFacility();
    
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

        Database.INSTANCE.setDefaultDatabasePath();
        Database.INSTANCE.createTables(); // if not already exists
        
        numberOfDays = Integer.valueOf(Database.INSTANCE.getSetting(DatabaseSettings.NUMBER_OF_DAYS.getSettingsName()));
        numberOfMovingDays = Integer
                .valueOf(Database.INSTANCE.getSetting(DatabaseSettings.NUMBER_OF_MOVING_DAYS.getSettingsName()));
        
        focusDay = LocalDate.now(); // set focus day to today
        new GridPaneInitializer(this, undoFacility, progressIndicator).setup(gridPane, numberOfDays, focusDay, toolBar.getPrefHeight());
        
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
                            new GridPaneInitializer(this, undoFacility, progressIndicator).setup(gridPane, numberOfDays, focusDay, toolBar.getPrefHeight());
                        }
                    }
                });
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
     * Sets the background color of a LabelCell.
     *
     * @param colorID
     *         ID of the color to set as background color.
     * @param taskCell
     *         LabelCell of which to change the background color.
     */
    public void setBackgroundColor(int colorID, TaskCell taskCell) {
        
        Platform.runLater(() -> {
            
            String colorString = Database.INSTANCE.getColorFromDatabase(colorID);
            
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
        new GridPaneInitializer(this, undoFacility, progressIndicator).setup(gridPane, numberOfDays, focusDay, toolBar.getPrefHeight());
    }
    
    /**
     * called by the today button focuses the planner on today
     */
    @FXML
    protected void goToToday() {
        //        refreshAllDays();
        focusDay = LocalDate.now();
        new GridPaneInitializer(this, undoFacility, progressIndicator).setup(gridPane, numberOfDays, focusDay, toolBar.getPrefHeight());
    }
    
    /**
     * called by the forward button moves the planner a (few) day(s) forward
     */
    @FXML
    protected void dayForward() {
        focusDay = focusDay.plusDays(numberOfMovingDays);
        new GridPaneInitializer(this, undoFacility, progressIndicator).setup(gridPane, numberOfDays, focusDay, toolBar.getPrefHeight());
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
                Database.INSTANCE.updateParentsDay(day, parentTasks);
                return null;
            }
        };
        task.setOnSucceeded(e -> progressIndicator.setVisible(false));
        exec.execute(task);
    }

    /**
     * Getters for the fxml references.
     */
    public ProgressIndicator getProgressIndicator() {
        return this.progressIndicator;
    }
}
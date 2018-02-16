package deltadak.ui;

import deltadak.Database;
import deltadak.commands.UndoFacility;
import deltadak.database.DatabaseSettings;
import deltadak.keylisteners.UndoKeyListener;
import deltadak.ui.gridpane.GridPaneInitializer;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.input.DataFormat;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;

import java.net.URL;
import java.time.LocalDate;
import java.util.ResourceBundle;

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
        
        Database.INSTANCE.setDefaultDatabasePath();
        Database.INSTANCE.createTables(); // if not already exists
        
        numberOfDays = Integer.valueOf(Database.INSTANCE.getSetting(DatabaseSettings.NUMBER_OF_DAYS.getSettingsName()));
        numberOfMovingDays = Integer
                .valueOf(Database.INSTANCE.getSetting(DatabaseSettings.NUMBER_OF_MOVING_DAYS.getSettingsName()));
        
        focusDay = LocalDate.now(); // set focus day to today
        new GridPaneInitializer(this, undoFacility, progressIndicator).setup(gridPane, numberOfDays, focusDay, toolBar.getPrefHeight());
        
        progressIndicator.setVisible(false);
        
        // setup the settings page
        SlidingSettingsPane slidingSettingsPane = new SlidingSettingsPane(this, editLabelsButton, editLabelsPane, settingsPane);
        copySettingsPaneComponents(slidingSettingsPane);
        slidingSettingsPane.setup();
        
        // setup help page
        SlidingPane helpPane = new SlidingPane(this);
        copyHelpPageComponents(helpPane);
        helpPane.setup();
        
        new UndoKeyListener().set(gridPane, undoFacility);
        
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
     * Getters for the fxml references.
     */
    public ProgressIndicator getProgressIndicator() {
        return this.progressIndicator;
    }
}
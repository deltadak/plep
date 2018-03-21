package nl.deltadak.plep.ui;

import javafx.scene.text.Text;
import nl.deltadak.plep.database.DatabaseSettings;
import nl.deltadak.plep.keylisteners.UndoKeyListener;
import nl.deltadak.plep.ui.gridpane.GridPaneInitializer;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.input.DataFormat;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import nl.deltadak.plep.Database;
import nl.deltadak.plep.JavaHelperKt;
import nl.deltadak.plep.commands.UndoFacility;
import nl.deltadak.plep.database.DatabaseSettings;
import nl.deltadak.plep.keylisteners.UndoKeyListener;
import nl.deltadak.plep.ui.gridpane.GridPaneInitializer;
import nl.deltadak.plep.ui.settingspane.panes.SlidingPane;
import nl.deltadak.plep.ui.settingspane.panes.SlidingSettingsPane;

import java.net.URL;
import java.time.LocalDate;
import java.util.ArrayList;
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
    @FXML Text settingsTextTitle;
    
    @FXML GridPane editLabelsPane;
    @FXML Button editLabelsButton;
    @FXML GridPane editDaysPane;
    @FXML Button removeLabelButton;
    /** To adjust the number of days to skip forward/backward. */
    @FXML Button applyNumberOfMovingDays;
    /** To adjust the number of days shown. */
    @FXML Button applyNumberOfDays;
    @FXML CheckBox autoColumnCheckBox;
    @FXML Button applyNumberOfColumns;
    
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
            "E55E5E", // Red.
            "5E78E5", // Blue.
            "60ed79", // Green.
            "ffd253", // Orange.
            "ffffff" // White.
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

        ArrayList<ColorPicker> colorPickers = new ArrayList<>();
        colorPickers.add(colorOne);
        colorPickers.add(colorTwo);
        colorPickers.add(colorThree);
        colorPickers.add(colorFour);
        colorPickers.add(colorFive);

        // setup the settings page
        SlidingSettingsPane slidingSettingsPane = new SlidingSettingsPane(
                this,
                JavaHelperKt.getUIRefresher(this),
                editLabelsButton,
                editLabelsPane,
                editDaysPane,
                settingsPane,
                removeLabelButton,
                applyNumberOfMovingDays,
                applyNumberOfDays,
                applyNumberOfColumns,
                autoColumnCheckBox,
                colorPickers,
                main,
                gridPane,
                toolBar,
                settingsPane,
                settingsButton);

        slidingSettingsPane.setup();
        
        // setup help page
        new SlidingPane(main, gridPane, toolBar, helpPane, helpButton).setup();
        
        new UndoKeyListener().set(gridPane, undoFacility);
        
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
package nl.deltadak.plep.ui;

import javafx.scene.text.Text;
import nl.deltadak.plep.database.DatabaseSettings;
import nl.deltadak.plep.ui.gridpane.GridPaneInitializer;
import nl.deltadak.plep.Database;
import nl.deltadak.plep.ui.settingspane.AutoColumnsAction;
import nl.deltadak.plep.ui.settingspane.applybuttons.ApplyNumberOfColumnsAction;
import nl.deltadak.plep.ui.settingspane.applybuttons.ApplyNumberOfDaysAction;
import nl.deltadak.plep.ui.settingspane.applybuttons.ApplyNumberOfMovingDaysAction;
import nl.deltadak.plep.ui.settingspane.labelslist.EditCourseLabelsAction;
import nl.deltadak.plep.ui.settingspane.labelslist.EditLabelsList;
import nl.deltadak.plep.ui.settingspane.labelslist.RemoveLabelAction;
import nl.deltadak.plep.ui.settingspane.spinners.NumberOfColumnsSpinner;
import nl.deltadak.plep.ui.settingspane.spinners.NumberOfDaysSpinner;
import nl.deltadak.plep.ui.settingspane.spinners.NumberOfMovingDaysSpinner;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import kotlin.Unit;
import kotlin.jvm.functions.Function0;

import java.util.ArrayList;
import java.util.List;

/**
 * A pane which provides settings.
 */
public class SlidingSettingsPane extends SlidingPane {

    // FXML references.
    private Button editLabelsButton;
    private GridPane editLabelsPane;
    private GridPane editDaysPane;
    private AnchorPane settingsPane;
    private Text settingsTextTitle;
    private Button removeLabelButton;
    private Button applyNumberOfMovingDays;
    private Button applyNumberOfDays;
    private Button applyNumberOfColumns;
    private CheckBox autoColumnsCheckBox;

    // References to object generated during runtime.
    private Spinner<Integer> numberOfMovingDaysSpinner;
    private Spinner<Integer> numberOfDaysSpinner;
    private Spinner<Integer> numberOfColumnsSpinner;


    // xml references passed from the controller
    // todo at the end check that all references are assigned something
    public GridPane colorsPane;
    public ColorPicker colorOne;
    public ColorPicker colorTwo;
    public ColorPicker colorThree;
    public ColorPicker colorFour;
    public ColorPicker colorFive;

    /** temporarily public for Java-Kt interop */
    public ListView<String> labelsList;
    private List<ColorPicker> colorPickers = new ArrayList<>();

    private Function0<Unit> refreshUI;
    
    /**
     * Construct a new SlidingSettingsPane.
     *
     * @param controller The controller which controls this.
     * @param refreshUI This is a function which should refresh the UI.
     * The remaining parameters are just FXML references.
     */
    @SuppressWarnings("JavaDoc")
    public SlidingSettingsPane(
            Controller controller,
            Function0<Unit> refreshUI,
            Button editLabelsButton,
            GridPane editLabelsPane,
            GridPane editDaysPane,
            AnchorPane settingsPane,
            Text settingsTextTitle,
            Button removeLabelButton,
            Button applyNumberOfMovingDays,
            Button applyNumberOfDays,
            Button applyNumberOfColumns,
            CheckBox autoColumnsCheckBox) {

        super(controller);

        // todo check at the end that all fxml references are assigned here
        // The references are copied to instance variables so they can be access in the setup hook method - order of initialisation is important!
        this.refreshUI = refreshUI;
        this.editLabelsButton = editLabelsButton;
        this.editLabelsPane = editLabelsPane;
        this.editDaysPane = editDaysPane;
        this.settingsPane = settingsPane;
        this.settingsTextTitle = settingsTextTitle;
        this.removeLabelButton = removeLabelButton;
        this.applyNumberOfMovingDays = applyNumberOfMovingDays;
        this.applyNumberOfDays = applyNumberOfDays;
        this.applyNumberOfColumns = applyNumberOfColumns;
        this.autoColumnsCheckBox = autoColumnsCheckBox;

    }
    
    /**
     * Hook into the setup method, use the instance variables with FXML references to set up components.
     */
    @Override
    public void setupHook() {
        setupSettingsMenu();

        setComponentActions(numberOfMovingDaysSpinner, numberOfDaysSpinner, numberOfColumnsSpinner);

        for (int i = 0; i < colorPickers.size(); i++) {
            ColorPicker colorPicker = colorPickers.get(i);
            colorPicker.setOnAction(event -> editColor(colorPicker));
        }

        boolean isAuto = Boolean
                .valueOf(getSetting(DatabaseSettings.MAX_COLUMNS_AUTO.getSettingsName()));
        autoColumnsCheckBox.setSelected(isAuto);
    }

    /**
     * Sets up the content of the settings menu.
     */
    private void setupSettingsMenu() {

        // todo get initial values from  database in spinners?

        // Add labels to their pane.
        ListView<String> labelsList = new EditLabelsList().getNew(refreshUI);
        editLabelsPane.getChildren().add(labelsList);

        // Add spinners to a separate pane.
        numberOfMovingDaysSpinner = new NumberOfMovingDaysSpinner().getNew();

        numberOfDaysSpinner = new NumberOfDaysSpinner().getNew();

        numberOfColumnsSpinner = new NumberOfColumnsSpinner().getNew();

        editDaysPane.getChildren().addAll(
                numberOfMovingDaysSpinner,
                numberOfDaysSpinner,
                numberOfColumnsSpinner
        );

        addColorsPane();

    }

    /**
     * Set correct actions on various buttons and such.
     * The following spinners are request as parameters so it is clear that they have to be initialised before this methods makes any sense!
     *
     * @param numberOfMovingDaysSpinner Option to choose number of moving days.
     * @param numberOfDaysSpinner Option to choose number of days.
     * @param numberOfColumnsSpinner Option to choose number of columns.
     */
    private void setComponentActions(
            Spinner<Integer> numberOfMovingDaysSpinner,
            Spinner<Integer> numberOfDaysSpinner,
            Spinner<Integer> numberOfColumnsSpinner) {

        new EditCourseLabelsAction(editLabelsButton).set(editLabelsPane, settingsPane);

        // Replace with regular set on converting to Kotlin.
        new RemoveLabelAction(removeLabelButton).javaSet(this, refreshUI);

        new ApplyNumberOfMovingDaysAction(applyNumberOfMovingDays, numberOfMovingDaysSpinner).javaSet(controller, refreshUI);

        new ApplyNumberOfDaysAction(applyNumberOfDays, numberOfDaysSpinner).javaSet(controller, refreshUI);

        new ApplyNumberOfColumnsAction(applyNumberOfColumns, numberOfColumnsSpinner, autoColumnsCheckBox).set(refreshUI);

        new AutoColumnsAction(autoColumnsCheckBox, numberOfColumnsSpinner).javaSet(refreshUI, controller);

    }
    
    private void addColorsPane() {
        colorPickers.add(colorOne);
        colorPickers.add(colorTwo);
        colorPickers.add(colorThree);
        colorPickers.add(colorFour);
        colorPickers.add(colorFive);
        
        // Get the colors from the database, so the colorpickers can be set
        // to those colors.
        String[] colors = Database.INSTANCE.getColorsFromDatabase();
        for (int i = 0; i < colorPickers.size(); i++) {
            colorPickers.get(i).getStyleClass().add("button");
            // Set the color in the database on the color picker.
            colorPickers.get(i).setValue(Color.web(colors[i]));
        }
    }
    
    /**
     * Called when the value of a color picker is changed. Updates the value of
     * the color in the database.
     *
     * @param colorPicker
     *         The ColorPicker that is edited.
     */
    private void editColor(ColorPicker colorPicker) {
        
        int id = colorPickers.indexOf(colorPicker);
        Database.INSTANCE
                .updateColor(id, convertColorToWeb(colorPicker.getValue()));

        new GridPaneInitializer(controller, controller.undoFacility, controller.progressIndicator).setup(gridPane, controller.numberOfDays, controller.focusDay, controller.toolBar.getPrefHeight());
        
    }
    
    /**
     * Converts a Color to a string hex value.
     *
     * @param color
     *         The Color to convert to hex.
     *
     * @return A String with the hex value of color.
     */
    private String convertColorToWeb(Color color) {
        
        return color.toString().substring(2, 8);
    }
    
}

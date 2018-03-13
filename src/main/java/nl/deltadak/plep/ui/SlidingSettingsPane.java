package nl.deltadak.plep.ui;

import nl.deltadak.plep.database.DatabaseSettings;
import nl.deltadak.plep.ui.gridpane.GridPaneInitializer;
import nl.deltadak.plep.Database;
import nl.deltadak.plep.ui.settingspane.AutoColumnsAction;
import nl.deltadak.plep.ui.settingspane.applybuttons.ApplyNumberOfColumnsAction;
import nl.deltadak.plep.ui.settingspane.applybuttons.ApplyNumberOfDaysAction;
import nl.deltadak.plep.ui.settingspane.applybuttons.ApplyNumberOfMovingDaysAction;
import nl.deltadak.plep.ui.settingspane.colors.ColorsList;
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
    private Button removeLabelButton;
    private Button applyNumberOfMovingDays;
    private Button applyNumberOfDays;
    private Button applyNumberOfColumns;
    private CheckBox autoColumnsCheckBox;
    private List<ColorPicker> colorPickers;

    // References to object generated during runtime.
    private Spinner<Integer> numberOfMovingDaysSpinner;
    private Spinner<Integer> numberOfDaysSpinner;
    private Spinner<Integer> numberOfColumnsSpinner;

    /** temporarily public for Java-Kt interop */
    public ListView<String> labelsList;

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
            Button removeLabelButton,
            Button applyNumberOfMovingDays,
            Button applyNumberOfDays,
            Button applyNumberOfColumns,
            CheckBox autoColumnsCheckBox,
            List<ColorPicker> colorPickers) {

        super(controller);

        // The references are copied to instance variables so they can be accessed in the setup hook method - order of initialisation is important!
        this.refreshUI = refreshUI;
        this.editLabelsButton = editLabelsButton;
        this.editLabelsPane = editLabelsPane;
        this.editDaysPane = editDaysPane;
        this.settingsPane = settingsPane;
        this.removeLabelButton = removeLabelButton;
        this.applyNumberOfMovingDays = applyNumberOfMovingDays;
        this.applyNumberOfDays = applyNumberOfDays;
        this.applyNumberOfColumns = applyNumberOfColumns;
        this.autoColumnsCheckBox = autoColumnsCheckBox;
        this.colorPickers = new ArrayList<>(colorPickers); // Copying is safer.

    }
    
    /**
     * Hook into the setup method, use the instance variables with FXML references to set up components.
     */
    @Override
    public void setupHook() {
        setupSettingsMenu();

        setComponentActions(numberOfMovingDaysSpinner, numberOfDaysSpinner, numberOfColumnsSpinner);

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

        // The options to customise colors.
        new ColorsList(colorPickers).setup(refreshUI);

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
    
}

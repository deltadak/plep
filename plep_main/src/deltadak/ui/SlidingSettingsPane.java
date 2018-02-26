package deltadak.ui;

import deltadak.Database;
import deltadak.database.DatabaseSettings;
import deltadak.ui.gridpane.GridPaneInitializer;
import deltadak.ui.settingspane.*;
import deltadak.ui.util.LayoutKt;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.control.cell.TextFieldListCell;
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
    private AnchorPane settingsPane;
    private Button removeLabelButton;
    private Button applyNumberOfMovingDays;
    private Button applyNumberOfDays;
    private Button applyNumberOfColumns;
    private CheckBox autoColumnsCheckBox;


    // xml references passed from the controller
    // todo at the end check that all references are assigned something
    public GridPane editDaysPane;
    public GridPane colorsPane;
    public ColorPicker colorOne;
    public ColorPicker colorTwo;
    public ColorPicker colorThree;
    public ColorPicker colorFour;
    public ColorPicker colorFive;

    // temporarily public for Java-Kt interop
    public ListView<String> labelsList;
    private Spinner<Integer> numberOfMovingDaysSpinner;
    private Spinner<Integer> numberOfDaysSpinner;
    private Spinner<Integer> numberOfColumnsSpinner;
    private List<ColorPicker> colorPickers = new ArrayList<>();

    private Function0<Unit> refreshUI;
    
    private final int MAX_NUMBER_LABELS = 5;
    
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
            AnchorPane settingsPane,
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
        this.settingsPane = settingsPane;
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

        new EditCourseLabelsAction(editLabelsButton).set(editLabelsPane, settingsPane);

        // Replace with regular set on converting to Kotlin.
        new RemoveLabelAction(removeLabelButton).javaSet(this, refreshUI);

        new ApplyNumberOfMovingDaysAction(applyNumberOfMovingDays, numberOfMovingDaysSpinner).javaSet(controller, refreshUI);

        new ApplyNumberOfDaysAction(applyNumberOfDays, numberOfDaysSpinner).javaSet(controller, refreshUI);

        if (applyNumberOfColumns != null) {
            new ApplyNumberOfColumnsAction(applyNumberOfColumns, numberOfColumnsSpinner, autoColumnsCheckBox).set(refreshUI);
        }

        setComponentListeners();

        boolean isAuto = Boolean
                .valueOf(getSetting(DatabaseSettings.MAX_COLUMNS_AUTO.getSettingsName()));
        autoColumnsCheckBox.setSelected(isAuto);
    }
    
    private void setComponentListeners() {
        //        autoColumnsCheckBox.setOnAction(event ->
        //                autoColumnsCheckBoxToggled());
        autoColumnsCheckBox.selectedProperty().addListener(
                (observable, oldValue, newValue) -> autoColumnsCheckBoxToggled(
                        newValue));
        //        colorOne.setOnMouseClicked(event -> editColor(colorOne));
        //        colorTwo.setOnMouseClicked(event -> editColor(colorTwo));
        for (int i = 0; i < colorPickers.size(); i++) {
            ColorPicker colorPicker = colorPickers.get(i);
            colorPicker.setOnAction(event -> editColor(colorPicker));
        }
    }
    
    /**
     * Sets up the content of the settings menu.
     */
    private void setupSettingsMenu() {
        
        addEditLabelsPane();
        addChangeNumberOfDaysSettings();
        addColorsPane();
        
    }
    
    /**
     * Sets up the editable ListView to edit the labels/items we want to see in
     * the comboboxes on the main screen.
     */
    private void addEditLabelsPane() {
        labelsList = new ListView<>();
        // first set up the listview with empty labels in order to allow editing
        ObservableList<String> itemsLabelsList = FXCollections
                .observableArrayList("", "", "", "", "");
        
        // get the labels from the database and store them in the listview
        ArrayList<String> labelStrings = getLabels();
        // add items starting at the top
        for (int i = 0; i < labelStrings.size(); i++) {
            itemsLabelsList.set(i, labelStrings.get(i));
        }
        
        // set a CellFactory on the listview to be able make the cells editable
        // using setEditable(true) isn't enough
        labelsList.setCellFactory(TextFieldListCell.forListView());
        labelsList.setEditable(true);
        
        // give the listview an id (FXML) so we can look it up by its id, and
        // toggle the visibility
        labelsList.setId("labelsListView");
        labelsList.setItems(itemsLabelsList);
        labelsList.setVisible(false);
        // "magik numbers" are figured out by observations
        labelsList.setPrefWidth(120);
        labelsList
                .setPrefHeight((LISTVIEW_ROW_HEIGHT * MAX_NUMBER_LABELS) + 18);
        
        // position the listview in the settings pane
        GridPane.setColumnIndex(labelsList, 1);
        GridPane.setRowIndex(labelsList, 0);
        GridPane.setRowSpan(labelsList, 2);
        
        // when editing a label in the listview, update the value
        // in the database and setup the main gridpane with the new items in the
        // comboboxes
        labelsList.setOnEditCommit(event -> {
            labelsList.getItems().set(event.getIndex(), event.getNewValue());
            updateLabel(event.getIndex(), event.getNewValue());
            new GridPaneInitializer(controller, controller.undoFacility, controller.progressIndicator).setup(gridPane, controller.numberOfDays, controller.focusDay, controller.toolBar.getPrefHeight());
        });
        
        editLabelsPane.getChildren().add(labelsList);
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
    
    /**
     * Adds the Spinners to be able to change the number of days to move and
     * number of days to show to the settings pane.
     */
    private void addChangeNumberOfDaysSettings() {
        // Adding the spinner to change the number of days to move.
        numberOfMovingDaysSpinner = new Spinner<>();
        SpinnerValueFactory<Integer> valueFactory
                = new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 14,
                                                                     controller.numberOfMovingDays);
        
        numberOfMovingDaysSpinner.setValueFactory(valueFactory);
        numberOfMovingDaysSpinner.setId("numberOfMovingDaysSpinner");
        numberOfMovingDaysSpinner.setPrefWidth(70);
        GridPane.setColumnIndex(numberOfMovingDaysSpinner, 2);
        editDaysPane.getChildren().add(numberOfMovingDaysSpinner);
        
        // Adding the spinner to change the number of days to show.
        numberOfDaysSpinner = new Spinner<>();
        // magik value 31 is the length of the longest month, just to be sure
        SpinnerValueFactory<Integer> valueShowFactory
                = new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 31,
                                                                     controller.numberOfDays);
        
        numberOfDaysSpinner.setValueFactory(valueShowFactory);
        numberOfDaysSpinner.setId("numberOfShowDaysSpinner");
        numberOfDaysSpinner.setPrefWidth(70);
        GridPane.setColumnIndex(numberOfDaysSpinner, 2);
        GridPane.setRowIndex(numberOfDaysSpinner, 1);
        editDaysPane.getChildren().add(numberOfDaysSpinner);
        
        // Adding the spinner to change the number of columns.
        numberOfColumnsSpinner = new Spinner<>();
        // Get the previous value from the database.
        int defaultValue = Integer
                .valueOf(getSetting(DatabaseSettings.MAX_COLUMNS.getSettingsName()));
        SpinnerValueFactory<Integer> valueColumnFactory
                = new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 14,
                                                                     defaultValue);
        
        numberOfColumnsSpinner.setValueFactory(valueColumnFactory);
        numberOfColumnsSpinner.setId("maxColumnsSpinner");
        numberOfColumnsSpinner.setPrefWidth(70);
        GridPane.setColumnIndex(numberOfColumnsSpinner, 2);
        GridPane.setRowIndex(numberOfColumnsSpinner, 2);
        editDaysPane.getChildren().add(numberOfColumnsSpinner);
    }
    
    /**
     * Updates the value of 'max_columns_auto' in the database to the new value
     * of the check box.
     *
     * @param newValue
     *         a boolean with true or false. True if the box is selected, false
     *         if it is not selected.
     */
    @FXML
    protected void autoColumnsCheckBoxToggled(boolean newValue) {
        updateSetting(DatabaseSettings.MAX_COLUMNS_AUTO.getSettingsName(),
                      String.valueOf(newValue));
        // The controller will request settings from the database again.
        new GridPaneInitializer(controller, controller.undoFacility, controller.progressIndicator).setup(gridPane, controller.numberOfDays, controller.focusDay, controller.toolBar.getPrefHeight());
        
        // update spinner to reflect auto mode on
        if (newValue) {
            int columns = LayoutKt.getNumberOfColumns(controller.numberOfDays);
            numberOfColumnsSpinner.getValueFactory().setValue(columns);
        } else {
            // use the value which was saved to the database
            int columns = Integer
                    .valueOf(getSetting(DatabaseSettings.MAX_COLUMNS.getSettingsName()));
            numberOfColumnsSpinner.getValueFactory().setValue(columns);
        }
    }
    
}

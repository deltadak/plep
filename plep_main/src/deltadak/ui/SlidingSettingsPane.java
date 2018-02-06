package deltadak.ui;

import deltadak.Database;
import deltadak.database.DatabaseSettings;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.cell.TextFieldListCell;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;

import java.util.ArrayList;
import java.util.List;

/**
 * A pane which provides settings.
 */
public class SlidingSettingsPane extends SlidingPane {
    
    // xml references passed from the controller
    public GridPane editLabelsPane;
    public Button editLabelsButton;
    public Button removeLabelButton;
    public GridPane editDaysPane;
    public Button applyNumberOfDays;
    public Button applyNumberOfShowDays;
    public CheckBox autoColumnsCheckBox;
    public Button applyMaxColumns;
    public GridPane colorsPane;
    public ColorPicker colorOne;
    public ColorPicker colorTwo;
    public ColorPicker colorThree;
    public ColorPicker colorFour;
    public ColorPicker colorFive;
    
    private ListView<String> labelsList;
    private Spinner<Integer> numberOfMovingDaysSpinner;
    private Spinner<Integer> numberOfShowDaysSpinner;
    private Spinner<Integer> maxColumnsSpinner;
    private List<ColorPicker> colorPickers = new ArrayList<>();
    
    private final int MAX_NUMBER_LABELS = 5;
    
    /**
     * Construct a new SlidingSettingsPane.
     *
     * @param controller
     *         The controller which controls this.
     */
    public SlidingSettingsPane(Controller controller) {
        super(controller);
    }
    
    /**
     * Setup components, which are hopefully not null...
     */
    @Override
    public void setupHook() {
        setupSettingsMenu();
        setComponentListeners();
        boolean isAuto = Boolean
                .valueOf(getSetting(DatabaseSettings.MAX_COLUMNS_AUTO.getSettingsName()));
        autoColumnsCheckBox.setSelected(isAuto);
    }
    
    private void setComponentListeners() {
        editLabelsButton.setOnAction(event -> editCourseLabels());
        removeLabelButton.setOnAction(event -> removeLabel());
        applyNumberOfDays.setOnAction(event -> applyNumberOfMovingDaysChange());
        applyNumberOfShowDays
                .setOnAction(event -> applyNumberOfShowDaysChange());
        applyMaxColumns.setOnAction(event -> applyMaxColumnsChange());
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
            controller.setupGridPane(controller.focusDay);
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

        controller.setupGridPane(controller.focusDay);
        
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
        numberOfShowDaysSpinner = new Spinner<>();
        // magik value 31 is the length of the longest month, just to be sure
        SpinnerValueFactory<Integer> valueShowFactory
                = new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 31,
                                                                     controller.numberOfDays);
        
        numberOfShowDaysSpinner.setValueFactory(valueShowFactory);
        numberOfShowDaysSpinner.setId("numberOfShowDaysSpinner");
        numberOfShowDaysSpinner.setPrefWidth(70);
        GridPane.setColumnIndex(numberOfShowDaysSpinner, 2);
        GridPane.setRowIndex(numberOfShowDaysSpinner, 1);
        editDaysPane.getChildren().add(numberOfShowDaysSpinner);
        
        // Adding the spinner to change the number of columns.
        maxColumnsSpinner = new Spinner<>();
        // Get the previous value from the database.
        int defaultValue = Integer
                .valueOf(getSetting(DatabaseSettings.MAX_COLUMNS.getSettingsName()));
        SpinnerValueFactory<Integer> valueColumnFactory
                = new SpinnerValueFactory.IntegerSpinnerValueFactory(1, 14,
                                                                     defaultValue);
        
        maxColumnsSpinner.setValueFactory(valueColumnFactory);
        maxColumnsSpinner.setId("maxColumnsSpinner");
        maxColumnsSpinner.setPrefWidth(70);
        GridPane.setColumnIndex(maxColumnsSpinner, 2);
        GridPane.setRowIndex(maxColumnsSpinner, 2);
        editDaysPane.getChildren().add(maxColumnsSpinner);
    }
    
    /**
     * Toggles the visibility of the listview with labels.
     */
    @FXML
    protected void editCourseLabels() {
        toggleVisibilityFXMLObject("labelsListView");
        toggleVisibilityFXMLObject("removeLabelButton");
        
        toggleYsettingsObject("editDaysPane");
        toggleYsettingsObject("colorsPane");
    }
    
    /**
     * Removes the selected label from all the items in the courselabel, also
     * removes it from the database
     */
    @FXML
    protected void removeLabel() {
        int selectedIndex = labelsList.getSelectionModel().getSelectedIndex();
        // to remove an item from the listview, we replace it with an empty
        // string, so we can edit it again
        labelsList.getItems().set(selectedIndex, "");
        updateLabel(selectedIndex, "");
        controller.setupGridPane(controller.focusDay);
    }
    
    /**
     * Applies the value of the numberOfMovingDaysSpinner to the main GridPane.
     */
    @FXML
    protected void applyNumberOfMovingDaysChange() {
        controller.numberOfMovingDays = numberOfMovingDaysSpinner.getValue();
        // update the value in the database
        updateSetting(DatabaseSettings.NUMBER_OF_MOVING_DAYS.getSettingsName(),
                      String.valueOf(controller.numberOfMovingDays));
        
        controller.setupGridPane(controller.focusDay);
    }
    
    /**
     * Applies the value of the numberOfShowDaysSpinner to the main GridPane.
     */
    @FXML
    protected void applyNumberOfShowDaysChange() {
        controller.numberOfDays = numberOfShowDaysSpinner.getValue();
        
        updateSetting(DatabaseSettings.NUMBER_OF_DAYS.getSettingsName(),
                      String.valueOf(controller.numberOfDays));
        controller.setupGridPane(controller.focusDay);
    }
    
    /**
     * Applies the new number of columns to the main GridPane, and toggles the
     * autoColumnsCheckBox to unselected. So the number of columns isn't
     * automatically calculated, but manually set.
     */
    @FXML
    protected void applyMaxColumnsChange() {
        int maxColumns = maxColumnsSpinner.getValue();
        updateSetting(DatabaseSettings.MAX_COLUMNS.getSettingsName(),
                      String.valueOf(maxColumns));
        autoColumnsCheckBox.setSelected(false);
        controller.setupGridPane(controller.focusDay);
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
        controller.setupGridPane(controller.focusDay);
        
        // update spinner to reflect auto mode on
        if (newValue) {
            int columns = controller.maxColumns(controller.numberOfDays);
            maxColumnsSpinner.getValueFactory().setValue(columns);
        } else {
            // use the value which was saved to the database
            int columns = Integer
                    .valueOf(getSetting(DatabaseSettings.MAX_COLUMNS.getSettingsName()));
            maxColumnsSpinner.getValueFactory().setValue(columns);
        }
    }
    
    /**
     * Toggles the visibility of an object.
     *
     * @param id
     *         String with a FXML id of the object to be toggled.
     */
    private void toggleVisibilityFXMLObject(String id) {
        Boolean isVisible = editLabelsPane.lookup("#" + id).isVisible();
        editLabelsPane.lookup("#" + id).setVisible(!isVisible);
    }
    
    /**
     * Moves the FXML object up or down as much as the height of the listview to
     * edit the labels, because the listview needs to push down the objects
     * below it.
     *
     * @param id
     *         The fx:id of the object to be moved.
     */
    private void toggleYsettingsObject(String id) {
        Node node = slidingPane.lookup("#" + id);
        if (node.getTranslateY() == 0) {
            node.setTranslateY(editLabelsPane.getHeight());
        } else {
            node.setTranslateY(0);
        }
    }
    
}

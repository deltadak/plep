package deltadak;

import com.sun.org.apache.xpath.internal.operations.Bool;
import javafx.animation.TranslateTransition;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.cell.TextFieldListCell;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.util.Duration;

import java.util.ArrayList;
import java.util.Objects;

/**
 * A pane which provides settings.
 */
public class SettingsPane {
    
    /** keep a reference to the controller to update some constants when settings are changed */
    private Controller controller;

    // xml references passed from the controller
    public AnchorPane main;
    public GridPane gridPane;
    public ToolBar toolBar;
    public AnchorPane settingsPane;
    public GridPane editLabelsPane;
    public Button editLabelsButton;
    public Button removeLabelButton;
    public Button settingsButton;
    public GridPane editDaysPane;
    public Button applyNumberOfDays;
    public Button applyNumberOfShowDays;
    public CheckBox autoColumnsCheckBox;
    public Button applyMaxColumns;

    private ListView<String> labelsList;
    private Spinner<Integer> numberOfMovingDaysSpinner;
    private Spinner<Integer> numberOfShowDaysSpinner;
    private Spinner<Integer> maxColumnsSpinner;

    // layout globals for the settings pane
    private static final int SETTINGS_WIDTH = 400;
    private static final int LISTVIEW_ROW_HEIGHT = 29;
    private static final int MAX_NUMBER_LABELS = 5;
    // the duration of the animation when opening and closing the settings pane
    private static final int TOGGLE_SETTINGS_DURATION = 350;

    /**
     * Construct a new SettingsPane.
     * @param controller The controller which controls this.
     */
    public SettingsPane(Controller controller) {
        this.controller = controller;

    }

    /**
     * Setup components, which are hopefully not null...
     */
    public void setup() {
//        controller.MAX_COLUMNS = maxColumns(controller.NUMBER_OF_DAYS);
        setupSettingsMenu();
        prepareToggleSettings();
        setComponentListeners();
        boolean isAuto = Boolean.valueOf(getSetting(
                Controller.MAX_COLUMNS_AUTO_NAME));
        autoColumnsCheckBox.setSelected(isAuto);
    }

    private void setComponentListeners() {
        editLabelsButton.setOnAction(event -> editCourseLabels());
        removeLabelButton.setOnAction(event -> removeLabel());
        applyNumberOfDays.setOnAction(event ->
                applyNumberOfMovingDaysChange());
        applyNumberOfShowDays.setOnAction(event ->
                applyNumberOfShowDaysChange());
        applyMaxColumns.setOnAction(event -> applyMaxColumnsChange());
//        autoColumnsCheckBox.setOnAction(event ->
//                autoColumnsCheckBoxToggled());
        autoColumnsCheckBox.selectedProperty().addListener(
                (observable, oldValue, newValue) ->
                        autoColumnsCheckBoxToggled(newValue));
    }

    /**
     * Sets up the animations for the settings pane, so we can open and close
     * the settings menu.
     */
    private void prepareToggleSettings() {
        settingsPane.setPrefWidth(SETTINGS_WIDTH);
        // set the left x coordinate of the settings pane at -SETTINGS_WIDTH
        // on initialization, so the entire pane is outside of the window
        settingsPane.setTranslateX(-SETTINGS_WIDTH);

        // setup the animation to open the settings pane
        TranslateTransition openNav =
                new TranslateTransition(new Duration(TOGGLE_SETTINGS_DURATION),
                        settingsPane);
        openNav.setToX(0);

        // setup the animation to close the settings pane
        TranslateTransition closeNav =
                new TranslateTransition(new Duration(TOGGLE_SETTINGS_DURATION),
                        settingsPane);

        // EventHandler to close the settings pane when the user clicks
        // somewhere outside the settings pane
        EventHandler<MouseEvent> filter = event -> {
            // check if the region in the gridpane, outside the settings
            // pane is clicked
            if(!inHierarchy(event.getPickResult().getIntersectedNode(), settingsPane)) {
                // fire the settings button so it will close the settings
                // pane and remove this EventHandler
                settingsButton.fire();
                event.consume();
            }

        };

        settingsButton.setOnAction((ActionEvent evt)->{
            if(settingsPane.getTranslateX()!=0){

                // add the event filter to close the settings pane
                main.addEventFilter(MouseEvent.MOUSE_CLICKED, filter);

                setEnable(gridPane, false);
                openNav.play();


            }else{
                closeNav.setToX(-settingsPane.getWidth());
                closeNav.play();
                // remove the event filter to close the settings pane
                main.removeEventFilter(MouseEvent.MOUSE_CLICKED, filter);
                setEnable(gridPane, true);
            }
        });

    }

    /**
     * Sets up the content of the settings menu.
     */
    private void setupSettingsMenu() {

        AnchorPane.setTopAnchor(settingsPane, toolBar.getPrefHeight());
        addEditLabelsPane();
        addChangeNumberOfDaysSettings();

    }

    /**
     * Sets up the editable ListView to edit the labels/items we want to see
     * in the comboboxes on the main screen.
     */
    private void addEditLabelsPane() {
        labelsList = new ListView<>();
        // first set up the listview with empty labels in order to allow editing
        ObservableList<String> itemsLabelsList =
                FXCollections.observableArrayList("","","","","");

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
        labelsList.setPrefHeight((LISTVIEW_ROW_HEIGHT * MAX_NUMBER_LABELS) + 18);

        // position the listview in the settings pane
        GridPane.setColumnIndex(labelsList, 1);
        GridPane.setRowIndex(labelsList, 0);
        GridPane.setRowSpan(labelsList, 2);

        // when editing a label in the listview, update the value
        // in the database and setup the main gridpane with the new items in the
        // comboboxes
        labelsList.setOnEditCommit(event -> {
            labelsList.getItems()
                    .set(event.getIndex(), event.getNewValue());
            updateLabel(event.getIndex(), event.getNewValue());
            controller.setupGridPane(controller.focusDay);
        });

        editLabelsPane.getChildren().add(labelsList);
    }

    /**
     * Adds the Spinners to be able to change the number of days to move and
     * number of days to show to the settings pane.
     */
    private void addChangeNumberOfDaysSettings() {
        // Adding the spinner to change the number of days to move.
        numberOfMovingDaysSpinner = new Spinner<>();
        SpinnerValueFactory<Integer> valueFactory = new SpinnerValueFactory
                .IntegerSpinnerValueFactory(1, 14, controller.NUMBER_OF_MOVING_DAYS);

        numberOfMovingDaysSpinner.setValueFactory(valueFactory);
        numberOfMovingDaysSpinner.setId("numberOfMovingDaysSpinner");
        numberOfMovingDaysSpinner.setPrefWidth(70);
        GridPane.setColumnIndex(numberOfMovingDaysSpinner, 2);
        editDaysPane.getChildren().add(numberOfMovingDaysSpinner);


        // Adding the spinner to change the number of days to show.
        numberOfShowDaysSpinner = new Spinner<>();
        // magik value 31 is the length of the longest month, just to be sure
        SpinnerValueFactory<Integer> valueShowFactory = new SpinnerValueFactory
                .IntegerSpinnerValueFactory(1, 31, controller.NUMBER_OF_DAYS);

        numberOfShowDaysSpinner.setValueFactory(valueShowFactory);
        numberOfShowDaysSpinner.setId("numberOfShowDaysSpinner");
        numberOfShowDaysSpinner.setPrefWidth(70);
        GridPane.setColumnIndex(numberOfShowDaysSpinner, 2);
        GridPane.setRowIndex(numberOfShowDaysSpinner,1);
        editDaysPane.getChildren().add(numberOfShowDaysSpinner);
        
        // Adding the spinner to change the number of columns.
        maxColumnsSpinner = new Spinner<>();
        // Get the previous value from the database.
        int defaultValue = Integer.valueOf(getSetting(
                Controller.MAX_COLUMNS_NAME));
        SpinnerValueFactory<Integer> valueColumnFactory = new
                SpinnerValueFactory.IntegerSpinnerValueFactory(
                        1, 14, defaultValue);
        
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
    }

    /**
     * Removes the selected label from all the items in the combobox, also
     * removes it from the database
     */
    @FXML protected void removeLabel() {
        int selectedIndex = labelsList.getSelectionModel().getSelectedIndex();
        // to remove an item from the listview, we replace it with an empty
        // string, so we can edit it again
        labelsList.getItems().set(selectedIndex,"");
        updateLabel(selectedIndex, "");
        controller.setupGridPane(controller.focusDay);
    }

    /**
     * Applies the value of the numberOfMovingDaysSpinner to the main
     * GridPane.
     */
    @FXML protected void applyNumberOfMovingDaysChange() {
        controller.NUMBER_OF_MOVING_DAYS = numberOfMovingDaysSpinner.getValue();
        // update the value in the database
        updateSetting(Controller.NUMBER_OF_MOVING_DAYS_NAME,
                String.valueOf(controller.NUMBER_OF_MOVING_DAYS));

        controller.setupGridPane(controller.focusDay);
    }

    /**
     * Applies the value of the numberOfShowDaysSpinner to the main GridPane.
     */
    @FXML protected void applyNumberOfShowDaysChange() {
        controller.NUMBER_OF_DAYS = numberOfShowDaysSpinner.getValue();

        updateSetting(Controller.NUMBER_OF_DAYS_NAME,
                String.valueOf(controller.NUMBER_OF_DAYS));
        controller.setupGridPane(controller.focusDay);
    }
    
    /**
     * Applies the new number of columns to the main GridPane, and toggles the
     * autoColumnsCheckBox to unselected. So the number of columns isn't
     * automatically calculated, but manually set.
     */
    @FXML protected void applyMaxColumnsChange() {
        controller.MAX_COLUMNS = maxColumnsSpinner.getValue();
        updateSetting(Controller.MAX_COLUMNS_NAME,
                      String.valueOf(controller.MAX_COLUMNS));
        autoColumnsCheckBox.setSelected(false);
        // No need to explicitly setup the GridPane here, that's already done
        // in autoColumnsCheckBoxToggled().
    }
    
    /**
     * Updates the value of 'max_columns_auto' in the database to the new
     * value of the check box.
     * @param newValue a boolean with true or false. True if the box is
     *                 selected, false if it is not selected.
     */
    @FXML protected void autoColumnsCheckBoxToggled(boolean newValue) {
        updateSetting(Controller.MAX_COLUMNS_AUTO_NAME,
                      String.valueOf(newValue));
        controller.setupGridPane(controller.focusDay);
    }

    /**
     * Toggles the visibility of an object.
     * @param id String with a FXML id of the object to be toggled.
     */
    private void toggleVisibilityFXMLObject(String id) {
        Boolean isVisible = editLabelsPane.lookup("#" + id).isVisible();
        editLabelsPane.lookup("#" + id).setVisible(!isVisible);
    }

    /**
     * Moves the FXML object up or down as much as the height of the listview to
     * edit the labels, because the listview needs to push down the objects below it.
     * @param id The fx:id of the object to be moved.
     */
    private void toggleYsettingsObject(String id) {
        Node node = settingsPane.lookup("#" + id);
        if(node.getTranslateY() == 0) {
            node.setTranslateY(editLabelsPane.getHeight());
        } else {
            node.setTranslateY(0);
        }
    }

    /**
     * Returns whether a MouseEvent happened in a certain node or not.
     * @param node The node the event happened in.
     * @param potentialHierarchyElement The node to check if the event
     *                                  happened in.
     * @return True if the event happened in the checked node, false
     * otherwise.
     */
    public static boolean inHierarchy(Node node, Node potentialHierarchyElement) {
        if (potentialHierarchyElement == null) {
            return true;
        }
        while (node != null) {
            if (Objects.equals(node, potentialHierarchyElement)) {
                return true;
            }
            node = node.getParent();
        }
        return false;
    }

    /**
     * See {@link Database#getSetting(String)}
     * @param name Same.
     * @return Same.
     */
    private String getSetting(String name) {
        return Database.INSTANCE.getSetting(name);
    }

    /**
     * See {@link Database#updateSetting(String, String)}
     * @param name Same.
     * @param newValue Same.
     */
    private void updateSetting(String name, String newValue) {
        Database.INSTANCE.updateSetting(name, newValue);
    }

    /**
     * See {@link Database#getLabels()}.
     * @return Same.
     */
    private ArrayList<String> getLabels() {
        return Database.INSTANCE.getLabels();
    }

    /**
     * See {@link Database#updateLabel(int, String)}
     * @param id Same.
     * @param label Same.
     */
    private void updateLabel(int id, String label) {
        Database.INSTANCE.updateLabel(id, label);
    }

    /*
     * End of database methods
     */

    /**
     * Enables or disables a node, to circumvent double negation.
     * @param node The node to enable to disable.
     * @param enable True if the node should be enabled, false if
     *               the node should be disabled.
     */
    private void setEnable(Node node, boolean enable) {
        node.setDisable(!enable);
    }

}

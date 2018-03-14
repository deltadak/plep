package nl.deltadak.plep.ui.settingspane

import javafx.scene.control.Button
import javafx.scene.control.CheckBox
import javafx.scene.control.ColorPicker
import javafx.scene.control.ListView
import javafx.scene.layout.AnchorPane
import javafx.scene.layout.GridPane
import nl.deltadak.plep.database.DatabaseSettings
import nl.deltadak.plep.ui.Controller
import nl.deltadak.plep.ui.SlidingPane
import nl.deltadak.plep.ui.settingspane.applybuttons.ApplyNumberOfColumnsAction
import nl.deltadak.plep.ui.settingspane.applybuttons.ApplyNumberOfDaysAction
import nl.deltadak.plep.ui.settingspane.applybuttons.ApplyNumberOfMovingDaysAction
import nl.deltadak.plep.ui.settingspane.colors.ColorsList
import nl.deltadak.plep.ui.settingspane.labelslist.EditCourseLabelsAction
import nl.deltadak.plep.ui.settingspane.labelslist.EditLabelsList
import nl.deltadak.plep.ui.settingspane.labelslist.RemoveLabelAction
import nl.deltadak.plep.ui.settingspane.spinners.NumberOfColumnsSpinner
import nl.deltadak.plep.ui.settingspane.spinners.NumberOfDaysSpinner
import nl.deltadak.plep.ui.settingspane.spinners.NumberOfMovingDaysSpinner
import kotlin.reflect.KMutableProperty

/**
 * A pane which provides settings.
 */
class SlidingSettingsPane(
        /** Only needed for SlidingPane. */
        controller: Controller,
        /** Should refresh UI when called. */
        private val refreshUI: () -> Unit,
        /** Should point to the number of days that the forward and backward button skip when pressed. */
        private val numberOfMovingDaysProperty: KMutableProperty<Int>,
        /** Should point to the number of days to be shown. */
        private val numberOfDaysProperty: KMutableProperty<Int>,
        /** The rest are all FXML references from the Controller. */
        private val editLabelsButton: Button,
        private val editLabelsPane: GridPane,
        private val editDaysPane: GridPane,
        private val settingsPane: AnchorPane,
        private val removeLabelButton: Button,
        private val applyNumberOfMovingDays: Button,
        private val applyNumberOfDays: Button,
        private val applyNumberOfColumns: Button,
        private val autoColumnsCheckBox: CheckBox,
        private val colorPickers: List<ColorPicker>) : SlidingPane(controller) {

    @Deprecated("only for Java callers")
    constructor(
            controller: Controller,
            refreshUI: () -> Unit,
            editLabelsButton: Button,
            editLabelsPane: GridPane,
            editDaysPane: GridPane,
            settingsPane: AnchorPane,
            removeLabelButton: Button,
            applyNumberOfMovingDays: Button,
            applyNumberOfDays: Button,
            applyNumberOfColumns: Button,
            autoColumnsCheckBox: CheckBox,
            colorPickers: List<ColorPicker>
    ) : this(
            controller,
            refreshUI,
            controller::numberOfMovingDays,
            controller::numberOfDays,
            editLabelsButton,
            editLabelsPane,
            editDaysPane,
            settingsPane,
            removeLabelButton,
            applyNumberOfMovingDays,
            applyNumberOfDays,
            applyNumberOfColumns,
            autoColumnsCheckBox,
            colorPickers
    )

    @Suppress("MemberVisibilityCanBePrivate") // Making it private will not compile.
    /** Keep a reference to the labels shown. */
    var labelsList: ListView<String> = ListView()

    /**
     * Hook into the setup method, use the instance variables with FXML references to set up components.
     */
    override fun setupHook() {
        setupTaskLabels()
        setupNumberOfMovingDays()
        setupNumberOfDays()
        setupNumberOfColumns()
        setupColors()
    }

    /**
     * Sets components to customise number of days to skip when using forward/backward button.
     */
    private fun setupNumberOfMovingDays() {

        val numberOfMovingDaysSpinner = NumberOfMovingDaysSpinner().getNew()
        editDaysPane.children.add(numberOfMovingDaysSpinner)
        ApplyNumberOfMovingDaysAction(applyNumberOfMovingDays, numberOfMovingDaysSpinner).set(numberOfMovingDaysProperty, refreshUI)

    }

    /**
     * Sets components to customise number of days to be shown.
     */
    private fun setupNumberOfDays() {

        val numberOfDaysSpinner = NumberOfDaysSpinner().getNew()
        editDaysPane.children.add(numberOfDaysSpinner)
        ApplyNumberOfDaysAction(applyNumberOfDays, numberOfDaysSpinner).set(numberOfDaysProperty, refreshUI)

    }

    /**
     * Sets components to customise number of columns.
     */
    private fun setupNumberOfColumns() {

        val numberOfColumnsSpinner = NumberOfColumnsSpinner().getNew()
        editDaysPane.children.add(numberOfColumnsSpinner)
        ApplyNumberOfColumnsAction(applyNumberOfColumns, numberOfColumnsSpinner, autoColumnsCheckBox).set(refreshUI)

        val isAuto = java.lang.Boolean
                .valueOf(getSetting(DatabaseSettings.MAX_COLUMNS_AUTO.settingsName))
        autoColumnsCheckBox.isSelected = isAuto
        AutoColumnsAction(autoColumnsCheckBox, numberOfColumnsSpinner).set(refreshUI, numberOfDaysProperty)

    }

    /**
     * Sets components to customise the labels that can be added to tasks.
     */
    private fun setupTaskLabels() {

        // Add labels to their pane, passing a reference to the list of labels so it can be updated.
        labelsList = EditLabelsList().getNew(::labelsList, refreshUI)
        editLabelsPane.children.add(labelsList)

        // Tell the button to hide/show the labels.
        EditCourseLabelsAction(editLabelsButton).set(editLabelsPane, settingsPane)

        // Button to remove selected label when pressed.
        RemoveLabelAction(removeLabelButton).set(::labelsList, refreshUI)

    }

    /**
     * Set up the options to customise task colors.
     */
    private fun setupColors() {
        ColorsList(colorPickers).setup(refreshUI)

    }
}

package nl.deltadak.plep.ui

import javafx.fxml.FXML
import javafx.scene.control.*
import javafx.scene.layout.AnchorPane
import javafx.scene.layout.GridPane
import nl.deltadak.plep.Database
import nl.deltadak.plep.commands.UndoFacility
import nl.deltadak.plep.database.DatabaseSettings
import nl.deltadak.plep.keylisteners.UndoKeyListener
import nl.deltadak.plep.ui.gridpane.GridPaneInitializer
import nl.deltadak.plep.ui.settingspane.panes.SlidingPane
import nl.deltadak.plep.ui.settingspane.panes.SlidingSettingsPane
import java.time.LocalDate

@Suppress("KDocMissingDocumentation") // FXML references.
/**
 * Class to control the UI
 */
class Controller {

    /** The main element of the UI is declared in interface.fxml. */
    @FXML lateinit var main: AnchorPane
    /** The main GridPane which contains everything.  */
    @FXML lateinit var gridPane: GridPane
    /** Top toolbar with buttons.  */
    @FXML lateinit var toolBar: ToolBar
    /** To show feedback. */
    @FXML lateinit var progressIndicator: ProgressIndicator

    // All these references have to be declared in controller because of fxml.

    /** Help pane. */
    @FXML lateinit var helpPane: AnchorPane
    @FXML lateinit var helpButton: Button

    /** Settings pane. */
    @FXML lateinit var settingsPane: AnchorPane
    @FXML lateinit var settingsButton: Button

    @FXML lateinit var editLabelsPane: GridPane
    @FXML lateinit var editLabelsButton: Button
    @FXML lateinit var editDaysPane: GridPane
    @FXML lateinit var removeLabelButton: Button
    /** To adjust the number of days to skip forward/backward.  */
    @FXML lateinit var applyNumberOfMovingDays: Button
    /** To adjust the number of days shown.  */
    @FXML lateinit var applyNumberOfDays: Button
    @FXML lateinit var autoColumnCheckBox: CheckBox
    @FXML lateinit var applyNumberOfColumns: Button

    @FXML lateinit var colorsPane: GridPane
    @FXML lateinit var colorOne: ColorPicker
    @FXML lateinit var colorTwo: ColorPicker
    @FXML lateinit var colorThree: ColorPicker
    @FXML lateinit var colorFour: ColorPicker
    @FXML lateinit var colorFive: ColorPicker

    /** Number of days shown, default value will be overridden in [initialize]. */
    var numberOfDays: Int = 0

    /** Number of days to skip when using the forward/backward buttons, default value will be overridden in [initialize]. */
    var numberOfMovingDays: Int = 0

    /** Day on which the gridpane is 'focused': the second day shown will be this day. */
    var focusDay: LocalDate = LocalDate.now()

    /** Keep a reference to the same undo facility. */
    val undoFacility = UndoFacility()

    /** Refreshes UI when called. */
    private val refreshUI = { GridPaneInitializer(undoFacility, progressIndicator).setup(gridPane, ::numberOfDays, ::focusDay, toolBar.prefHeight) }

    /**
     * Initialization method for the controller.
     */
    fun initialize() {

        Database.INSTANCE.setDefaultDatabasePath()
        Database.INSTANCE.createTables()

        numberOfDays = Integer.valueOf(Database.INSTANCE.getSetting(DatabaseSettings.NUMBER_OF_DAYS.settingsName).let { if(it=="") "0" else it})

        numberOfMovingDays = Integer
                .valueOf(Database.INSTANCE.getSetting(DatabaseSettings.NUMBER_OF_MOVING_DAYS.settingsName).let { if(it=="") "0" else it})

        refreshUI()

        progressIndicator.isVisible = false

        val colorPickers = arrayListOf(colorOne, colorTwo, colorThree, colorFour, colorFive)

        // Setup the settings page.
        SlidingSettingsPane(
                refreshUI,
                ::numberOfMovingDays,
                ::numberOfDays,
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
                settingsButton
        ).setup()

        // Setup the help page.
        SlidingPane(main, gridPane, toolBar, helpPane, helpButton).setup()

        UndoKeyListener().set(gridPane, undoFacility)

    }

    /**
     * Called by the backward button.
     * Moves the planner a (few) day(s) back.
     */
    @FXML
    fun dayBackward() {
        focusDay = focusDay.plusDays((-numberOfMovingDays).toLong())
        refreshUI()
    }

    /**
     * Called by the today button.
     * Focuses the planner on today.
     */
    @FXML
    fun goToToday() {
        focusDay = LocalDate.now()
        refreshUI()
    }

    /**
     * Called by the forward button.
     * Moves the planner a (few) day(s) forward.
     */
    @FXML
    fun dayForward() {
        focusDay = focusDay.plusDays(numberOfMovingDays.toLong())
        refreshUI()
    }

}
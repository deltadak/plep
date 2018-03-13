@file:Suppress("UNUSED_ANONYMOUS_PARAMETER")

package nl.deltadak.plep

import nl.deltadak.plep.ui.gridpane.GridPaneInitializer
import nl.deltadak.plep.ui.Controller
import javafx.stage.Stage
import java.time.LocalDate

/**
 * Provides listener for change of day.
 */
class DayChangeListener(
        /** The main Controller. */
        val controller: Controller) {

    /** We do need to remember which day is the last known day. */
    var today: LocalDate = LocalDate.now()

    /**
     * Sets a listener which checks if it is a new day, when the window becomes
     * focused.
     *
     * @param primaryStage
     *         Stage to set listener on.
     */
    fun setup(primaryStage: Stage) {

        primaryStage.focusedProperty().addListener {
            observable, wasFocused, isFocused ->
            // If Plep becomes focused and the day has advanced compared to the last known day...
            if (isFocused && today != LocalDate.now()) {
                today = LocalDate.now()
                // Reset the view.

                controller.focusDay = today

                GridPaneInitializer(controller, controller.undoFacility, controller.progressIndicator).setup(controller.gridPane, controller.numberOfDays, today, controller.toolBar.prefHeight)
            }
        }
    }

}
package nl.deltadak.plep

import nl.deltadak.plep.ui.Controller
import nl.deltadak.plep.ui.gridpane.GridPaneInitializer

/**
 * Temporary helper function for Java interop.
 */
fun getUIRefresher(controller: Controller): Function0<Unit> {
    return { GridPaneInitializer(controller, controller.undoFacility, controller.progressIndicator).setup(controller.gridPane, controller::numberOfDays, controller::focusDay, controller.toolBar.prefHeight) }
}

package deltadak

import deltadak.ui.Controller
import deltadak.ui.gridpane.GridPaneInitializer

/**
 * Temporary helper function for Java interop.
 */
fun getUIRefresher(controller: Controller): Function0<Unit> {
    return { GridPaneInitializer(controller, controller.undoFacility, controller.progressIndicator).setup(controller.gridPane, controller::numberOfDays, controller::focusDay, controller.toolBar.prefHeight) }
}
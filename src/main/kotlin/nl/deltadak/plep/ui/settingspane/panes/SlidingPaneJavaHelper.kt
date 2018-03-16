package nl.deltadak.plep.ui.settingspane.panes

import javafx.animation.TranslateTransition
import javafx.event.EventHandler
import javafx.scene.input.MouseEvent
import javafx.scene.layout.AnchorPane
import javafx.scene.layout.GridPane

/**
 * Helper for SlidingPane, Java has to call this because it didn't work when calling from Kotlin.
 */
fun slidingPaneJavaHelper(openNav: TranslateTransition, closeNav: TranslateTransition, filter: EventHandler<MouseEvent>, slidingPane: AnchorPane, main: AnchorPane, gridPane: GridPane) {
    if (slidingPane.translateX != 0.0) {

        // Add the event filter to close the settings pane.
        main.addEventFilter(MouseEvent.MOUSE_CLICKED, filter)

        gridPane.disable()
        openNav.play()

    } else {
        closeNav.toX = -slidingPane.width
        closeNav.play()
        // Remove the event filter to close the settings pane.
        main.removeEventFilter(MouseEvent.MOUSE_CLICKED, filter)
        gridPane.enable()
    }
}

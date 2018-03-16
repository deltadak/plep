package nl.deltadak.plep.ui.settingspane.panes

import javafx.animation.TranslateTransition
import javafx.scene.control.Button
import javafx.scene.control.ToolBar
import javafx.scene.input.MouseEvent
import javafx.scene.layout.AnchorPane
import javafx.scene.layout.GridPane
import javafx.util.Duration
import nl.deltadak.plep.ui.settingspane.PANE_WIDTH
import nl.deltadak.plep.ui.settingspane.TOGGLE_DURATION

/**
 * A general Pane which slides in from the left.
 */
open class SlidingPane(
        /** FXML references. */
        private val main: AnchorPane,
        private val gridPane: GridPane,
        private val toolBar: ToolBar,
        private val slidingPane: AnchorPane,
        private val openCloseButton: Button) {

    /**
     * Setup components, which are hopefully not null...
     */
    fun setup() {
        AnchorPane.setTopAnchor(slidingPane, toolBar.prefHeight)
        setupPaneToggle()
        setupHook()
    }

    /**
     * Allow hooking into [.setup].
     */
    open fun setupHook() {}

    /**
     * Sets up the animations for the settings pane, so we can open and close the settings menu.
     */
    private fun setupPaneToggle() {



        slidingPane.prefWidth = PANE_WIDTH.toDouble()
        // Set the left x coordinate of the settings pane at -PANE_WIDTH on initialization, so the entire pane is outside of the window.
        // For some strange reason the extra buffer of 10 is needed, otherwise the pane starts like 10 pixels in view.
        slidingPane.translateX = (-PANE_WIDTH - 10).toDouble()

        // Setup the animation to open the settings pane.
        val openNav = TranslateTransition(Duration(TOGGLE_DURATION.toDouble()), slidingPane)
        openNav.toX = 0.0

        // Setup the animation to close the settings pane.
        val closeNav = TranslateTransition(Duration(TOGGLE_DURATION.toDouble()), slidingPane)

        // EventHandler to close the settings pane when the user clicks somewhere outside the settings pane.
        val filter = { event: MouseEvent ->
            // Check if the region in the gridpane, outside the settings pane is clicked.
            if (!isInHierarchy(event.pickResult.intersectedNode, slidingPane)) {
                // Fire the settings button so it will close the settingspane and remove this EventHandler.
                openCloseButton.fire()
                event.consume()
            }

        }

        /*
        Below code did not work in Kotlin, button action kept firing endlessly so SlidingPaneKotlinHelper is used for the setOnAction line, which calls slidingPaneJavaHelper().
         */

        SlidingPaneKotlinHelper().setOpenCloseButtonAction(openCloseButton, openNav, closeNav, filter, slidingPane, main, gridPane)


//        openCloseButton.setOnAction {
//            println("clicked")
//            if (slidingPane.translateX != 0.0) {
//
//                // Add the event filter to close the settings pane.
//                main.addEventFilter(MouseEvent.MOUSE_CLICKED, filter)
//
//                gridPane.disable()
//                openNav.play()
//
//            } else {
//                closeNav.toX = -slidingPane.width
//                closeNav.play()
//                // Remove the event filter to close the settings pane.
//                main.removeEventFilter(MouseEvent.MOUSE_CLICKED, filter)
//                gridPane.enable()
//            }


//        }



    }
}

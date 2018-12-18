package nl.deltadak.plep.ui.settingspane.panes;

import javafx.animation.TranslateTransition;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.Button;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;

import static nl.deltadak.plep.ui.settingspane.panes.SlidingPaneJavaHelperKt.slidingPaneJavaHelper;

/**
 * A few lines will be written in Java as they didn't work in Kotlin.
 */
public class SlidingPaneKotlinHelper {


    /**
     * Set open/close button action.
     */
    @SuppressWarnings("JavaDoc")
    public void setOpenCloseButtonAction(Button openCloseButton, TranslateTransition openNav, TranslateTransition closeNav, EventHandler<MouseEvent> filter, AnchorPane slidingPane, AnchorPane main, GridPane gridPane) {

        openCloseButton.setOnAction((ActionEvent evt) -> slidingPaneJavaHelper(openNav, closeNav, filter, slidingPane, main, gridPane));

    }

}

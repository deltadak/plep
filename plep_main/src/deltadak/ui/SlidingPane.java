package deltadak.ui;

import deltadak.Database;
import javafx.animation.TranslateTransition;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.util.Duration;

import java.util.ArrayList;
import java.util.Objects;

/**
 * A general Pane which slides in from the left.
 */
public class SlidingPane {
    /** keep a reference to the controller to update some constants when settings are changed */
    protected Controller controller;

    // xml references passed from the controller
    public AnchorPane main;
    public GridPane gridPane;
    public ToolBar toolBar;
    public AnchorPane slidingPane;
    public Button openCloseButton;

    // layout globals for the settings pane
    protected static final int PANE_WIDTH = 400;
    protected static final int LISTVIEW_ROW_HEIGHT = 29;
    // the duration of the animation when opening and closing the settings pane
    protected static final int TOGGLE_DURATION = 350;

    /**
     * Construct a new SlidingPane.
     * @param controller The controller which controls this.
     */
    public SlidingPane(Controller controller) {
        this.controller = controller;
    }

    /**
     * Setup components, which are hopefully not null...
     */
    public void setup() {
        AnchorPane.setTopAnchor(slidingPane, toolBar.getPrefHeight());
        preparePaneToggle();
        setupHook();
    }

    /**
     * Allow hooking into {@link #setup()}.
     */
    public void setupHook() {

    }

    /**
     * Sets up the animations for the settings pane, so we can open and close
     * the settings menu.
     */
    private void preparePaneToggle() {
        slidingPane.setPrefWidth(PANE_WIDTH);
        // set the left x coordinate of the settings pane at -PANE_WIDTH
        // on initialization, so the entire pane is outside of the window
        // for some strange reason the extra buffer of 10 is needed, otherwise the pane starts like 10 pixels in view
        slidingPane.setTranslateX(-PANE_WIDTH-10);

        // setup the animation to open the settings pane
        TranslateTransition openNav =
                new TranslateTransition(new Duration(TOGGLE_DURATION),
                        slidingPane);
        openNav.setToX(0);

        // setup the animation to close the settings pane
        TranslateTransition closeNav =
                new TranslateTransition(new Duration(TOGGLE_DURATION),
                        slidingPane);

        // EventHandler to close the settings pane when the user clicks
        // somewhere outside the settings pane
        EventHandler<MouseEvent> filter = event -> {
            // check if the region in the gridpane, outside the settings
            // pane is clicked
            if (!inHierarchy(event.getPickResult().getIntersectedNode(), slidingPane)) {
                // fire the settings button so it will close the settings
                // pane and remove this EventHandler
                openCloseButton.fire();
                event.consume();
            }

        };

        openCloseButton.setOnAction((ActionEvent evt) -> {
            if (slidingPane.getTranslateX() != 0) {

                // add the event filter to close the settings pane
                main.addEventFilter(MouseEvent.MOUSE_CLICKED, filter);

                setEnable(gridPane, false);
                openNav.play();


            } else {
                closeNav.setToX(-slidingPane.getWidth());
                closeNav.play();
                // remove the event filter to close the settings pane
                main.removeEventFilter(MouseEvent.MOUSE_CLICKED, filter);
                setEnable(gridPane, true);
            }
        });

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
    protected String getSetting(String name) {
        return Database.INSTANCE.getSetting(name);
    }

    /**
     * See {@link Database#updateSetting(String, String)}
     * @param name Same.
     * @param newValue Same.
     */
    protected void updateSetting(String name, String newValue) {
        Database.INSTANCE.updateSetting(name, newValue);
    }

    /**
     * See {@link Database#getLabels()}.
     * @return Same.
     */
    protected ArrayList<String> getLabels() {
        return Database.INSTANCE.getLabels();
    }

    /**
     * See {@link Database#updateLabel(int, String)}
     * @param id Same.
     * @param label Same.
     */
    protected void updateLabel(int id, String label) {
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
    protected void setEnable(Node node, boolean enable) {
        node.setDisable(!enable);
    }

}

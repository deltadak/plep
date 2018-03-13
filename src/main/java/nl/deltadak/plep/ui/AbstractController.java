package nl.deltadak.plep.ui;

import javafx.scene.control.ProgressIndicator;

/**
 * Abstract controller, useful for testing {@link DeleteCommand} and such things, because then you can provide a dummy
 * controller to the {@link DeleteCommand} while testing.
 */
@SuppressWarnings("ALL")
public interface AbstractController {

    /**
     * Getters for the fxml references.
     */
    ProgressIndicator getProgressIndicator();

}

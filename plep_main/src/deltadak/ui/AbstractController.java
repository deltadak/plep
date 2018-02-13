package deltadak.ui;

import deltadak.HomeworkTask;
import deltadak.commands.DeleteCommand;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;

import java.time.LocalDate;
import java.util.List;

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

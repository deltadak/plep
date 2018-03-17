package nl.deltadak.plep;

import javafx.scene.control.ProgressIndicator;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import nl.deltadak.plep.commands.DeleteCommand;
import org.junit.jupiter.api.Test;
import org.testfx.api.FxRobot;
import org.testfx.api.FxToolkit;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeoutException;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests {@link DeleteCommand}.
 */
public class DeleteCommandTest extends FxRobot {

    List<HomeworkTask> taskToDelete;
    DeleteCommand command;

    private void setup() throws TimeoutException {
        // Initialise JavaFX Toolkit, needed for things like ProgressIndicator.
        FxToolkit.registerPrimaryStage();
        FxToolkit.setupApplication(Main.class);

        LocalDate dayState = LocalDate.now();
        List<List<HomeworkTask>> list = new ArrayList<>();
        taskToDelete = new ArrayList<>();
        taskToDelete.add(new HomeworkTask());
        taskToDelete.add(new HomeworkTask());
        list.add(taskToDelete);
        // It doesn't really matter what's in here, but it needs something.
        TreeView<HomeworkTask> treeView = new TreeView<>();
        TreeItem<HomeworkTask> root = new TreeItem<>();
        treeView.setRoot(root);
        root.getChildren().add(new TreeItem<>());
        command = new DeleteCommand(new ProgressIndicator(), dayState, list, 0, treeView);
    }

    @Test
    void testIsExecuted() throws TimeoutException {
        setup();

        assertFalse(command.isExecuted(), "Command should not be executed initially");
        command.execute();
        assertTrue(command.isExecuted(), "Command should be executed after execution");
        command.undo();
        assertFalse(command.isExecuted());
    }

    @Test
    void testContent() throws TimeoutException {
        setup();

        command.execute();
        assertFalse(command.getTreeViewItems().contains(taskToDelete));
        command.undo();
        assertTrue(command.getTreeViewItems().contains(taskToDelete));
    }

}
package deltadak;

import deltadak.commands.DeleteCommand;
import deltadak.ui.AbstractController;
import javafx.scene.control.ListView;
import javafx.scene.control.TreeView;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests {@link DeleteCommand}.
 */
public class DeleteCommandTest {

    List<HomeworkTask> taskToDelete;
    DeleteCommand command;

    private void setup() {
        LocalDate dayState = LocalDate.now();
        List<List<HomeworkTask>> list = new ArrayList<>();
        taskToDelete = new ArrayList<>();
        taskToDelete.add(new HomeworkTask());
        taskToDelete.add(new HomeworkTask());
        list.add(taskToDelete);
        AbstractController dummy = new AbstractController() {
            @Override
            public void updateDatabase(LocalDate day, List<List<HomeworkTask>> homeworkTasks) {
                // do nothing
            }

            @Override
            public void cleanUp(TreeView<HomeworkTask> list) {
                // do nothing
            }

            @Override
            public void deleteExpanded(int id) {

            }

            @Override
            public void insertExpandedItem(int id, boolean expanded) {

            }
        };
        command = new DeleteCommand(dummy, dayState, list, 0, null);
    }

    @Test
    void testIsExecuted() {
        setup();

        assertFalse(command.isExecuted(), "Command should not be executed initially");
        command.execute();
        assertTrue(command.isExecuted(), "Command should be executed after execution");
        command.undo();
        assertFalse(command.isExecuted());
    }

    @Test
    void testContent() {
        setup();

        command.execute();
        assertFalse(command.getListItems().contains(taskToDelete));
        command.undo();
        assertTrue(command.getListItems().contains(taskToDelete));
    }

}
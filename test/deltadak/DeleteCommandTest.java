package deltadak;

import javafx.scene.control.ListView;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests {@link DeleteCommand}.
 */
class DeleteCommandTest {

    HomeworkTask taskToDelete;
    DeleteCommand command;

    private void setup() {
        LocalDate dayState = LocalDate.now();
        List<HomeworkTask> list = new ArrayList<>();
        taskToDelete = new HomeworkTask("text", "label", "color");
        list.add(taskToDelete);
        AbstractController dummy = new AbstractController() {
            @Override
            public void updateDatabase(LocalDate day, List<HomeworkTask> homeworkTasks) {
                // do nothing
            }

            @Override
            public void cleanUp(ListView<HomeworkTask> list) {
                // do nothing
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
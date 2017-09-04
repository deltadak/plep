package deltadak.ui;

import deltadak.HomeworkTask;
import deltadak.commands.DeleteCommand;
import javafx.scene.control.TreeView;

import java.time.LocalDate;
import java.util.List;

/**
 * Abstract controller, useful for testing {@link DeleteCommand} and such things, because then you can provide a dummy
 * controller to the {@link DeleteCommand} while testing.
 */
public interface AbstractController {

    /**
     * Updates database using the given homework tasks for a day.
     * @param day Date from which the tasks are.
     * @param homeworkTasks Tasks to be put in the database.
     */
    void updateDatabase(LocalDate day, List<List<HomeworkTask>> homeworkTasks);

    /**
     * removes empty rows, and then fills up with empty rows
     *
     * @param list to clean up
     */
    void cleanUp(TreeView<HomeworkTask> list);

}

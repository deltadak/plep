package deltadak;

import java.time.LocalDate;
import java.util.List;

/**
 * Command to delete an {@link HomeworkTask} from a ListView.
 */
public class DeleteCommand extends Command {

    /** remember state */
    Controller controller;
    LocalDate dayState;
    List<HomeworkTask> listState;
    int indexState;
    HomeworkTask deletedTask;

    public DeleteCommand(Controller controller, LocalDate day, List<HomeworkTask> list, int index) {

    }

    @Override
    protected void continueExecution() {
        // todo delete element without changing state
        controller.updateDatabase(dayState, listState);
    }

    @Override
    protected void continueUndo() {
        // todo undo
        controller.updateDatabase(dayState, listState);
    }

}

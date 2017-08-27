package deltadak.commands;

import deltadak.Database;
import deltadak.HomeworkTask;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * A command to edit the text of an homework task.
 */
public class EditTextCommand extends Command {

    /** Store state in order to undo */
    LocalDate dayState;
    List<List<HomeworkTask>> listState;
    String stuff;
    // and instance variables to store what to change...

    /**
     * Store old state to execute command.
     * @param day of the listview which is edited
     * @param list all the HomeworkTasks of the day which is edited
     */
    public EditTextCommand(LocalDate day, List<List<HomeworkTask>> list) {
        this.dayState = day;
        this.listState = list;
    }

    @Override
    protected void executionHook() {
        List<List<HomeworkTask>> newList = new ArrayList<>(listState);
//        newList.add(stuff);
        Database.INSTANCE.updateTasksDay(dayState, newList);
    }

    @Override
    protected void undoHook() {
        Database.INSTANCE.updateTasksDay(dayState, listState);

    }

}

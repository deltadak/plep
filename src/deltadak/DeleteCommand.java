package deltadak;

import javafx.scene.control.ListView;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * Command to delete an {@link HomeworkTask} from a ListView.
 */
public class DeleteCommand extends Command {

    /** remember state */

    // program to an interface so tests can provide a dummy
    // Does not call the {@link Database} instance directly in order for tests to be run on this command.
    private AbstractController controller;

    private LocalDate dayState;
    private List<HomeworkTask> listViewItems;
    private ListView<HomeworkTask> listView;
    private int indexState;
    private HomeworkTask deletedTask;

    /**
     * Constructor, gets selected index to find out what to delete.
     * @param controller the controller which needs to be updated after deletion. Does not call the {@link Database} instance directly in order for tests to be run on this command.
     * @param day from which to delete the task
     * @param listViewItems corresponding to the day, items of listview, needed as parameter for testing (which can't use a listview)
     * @param index index of task in listViewItems to delete, needed as parameter for testing (which can't use a listview)
     * @param listView to provide immediate user feedback
     */
    public DeleteCommand(AbstractController controller, LocalDate day, List<HomeworkTask> listViewItems, int index, ListView<HomeworkTask> listView) {
        this.controller = controller;
        this.dayState = day;
        this.listViewItems = new ArrayList<>(listViewItems);
        this.indexState = index;
        this.listView = listView;
    }

    @Override
    protected void executionHook() {
        if (listViewItems.isEmpty()) {
            throw new IllegalStateException("cannot delete item from empty list");
        }
        // save the deleted task so we can undo later
        deletedTask = listViewItems.get(indexState);
        listViewItems.remove(indexState);
        // use the listview to delete an item so the user gets feedback
        if (listView != null) {
            listView.getItems().remove(deletedTask);
        }
        // use the items of the listview to update the database
        controller.updateDatabase(dayState, listViewItems);
    }

    @Override
    protected void undoHook() {
        listViewItems.add(indexState, deletedTask);
        // if not testing, provide user feedback
        if (listView != null) {
            listView.getItems().add(indexState, deletedTask);
        }
        controller.updateDatabase(dayState, listViewItems);
        if (listView != null) {
            controller.cleanUp(listView);
        }
    }

    /**
     * Get the listViewItems this command remembers, mainly used for testing purposes.
     * @return listViewItems of {@link HomeworkTask}s.
     */
    public List<HomeworkTask> getListItems() {
        return listViewItems;
    }

}

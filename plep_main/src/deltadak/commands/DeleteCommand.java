package deltadak.commands;

import deltadak.ui.AbstractController;
import deltadak.HomeworkTask;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * Command to delete an {@link HomeworkTask} from a TreeView.
 */
public class DeleteCommand extends Command {

    /** remember state */
    private AbstractController controller; // program to an interface so tests can provide a dummy
    private LocalDate dayState;
    private List<List<HomeworkTask>> treeViewItems; // A list of (lists containing a parent and it's children)
    private TreeView<HomeworkTask> tree;
    private int indexState;
    private List<HomeworkTask> deletedTask; // It's a list, includes subtasks

    /**
     * Constructor, gets selected index to find out what to delete.
     * @param controller the controller which needs to be updated after deletion
     * @param day from which to delete the task
     * @param treeViewItems corresponding to the day, items of listview, needed as parameter for testing (which can't use a listview)
     * @param index index of task in treeViewItems to delete, needed as parameter for testing (which can't use a listview)
     * @param tree to provide immediate user feedback
     */
    public DeleteCommand(AbstractController controller, LocalDate day, List<List<HomeworkTask>> treeViewItems, int index, TreeView<HomeworkTask> tree) {
        this.controller = controller;
        this.dayState = day;
        this.treeViewItems = new ArrayList<>(treeViewItems);
        this.indexState = index;
        this.tree = tree;
    }

    @Override
    protected void executionHook() {
        if (treeViewItems.isEmpty()) {
            throw new IllegalStateException("cannot delete item from empty list");
        }
        // save the deleted task so we can undo later
        deletedTask = treeViewItems.get(indexState);
        treeViewItems.remove(indexState);
        // use the treeview to delete an item so the user gets feedback
        if (tree != null) {
            tree.getRoot().getChildren().remove(indexState);
        }
        // use the items of the listview to update the database
        controller.updateDatabase(dayState, treeViewItems);
    }

    @Override
    protected void undoHook() {
        treeViewItems.add(indexState, deletedTask);
        // if not testing, provide user feedback
        if (tree != null) {
            // add main task
            tree.getRoot().getChildren().add(indexState, new TreeItem<>(deletedTask.get(0)));
            // add subtasks
            // start at i=1 because first item is the parent task
            TreeItem<HomeworkTask> parent = tree.getRoot().getChildren().get(indexState);
            for (int i = 1; i < deletedTask.size(); i++) {
                parent.getChildren().add(new TreeItem<>(deletedTask.get(i)));
            }
        }
        controller.updateDatabase(dayState, treeViewItems);
        if (tree != null) {
            controller.cleanUp(tree);
        }
    }

    /**
     * Get the treeViewItems this command remembers, mainly used for testing purposes.
     * @return treeViewItems of {@link HomeworkTask}s.
     */
    public List<List<HomeworkTask>> getListItems() {
        return treeViewItems;
    }

}

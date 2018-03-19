package nl.deltadak.plep.commands;

import nl.deltadak.plep.database.DatabaseFacade;
import nl.deltadak.plep.ui.treeview.TreeViewCleaner;
import javafx.scene.control.ProgressIndicator;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import nl.deltadak.plep.Database;
import nl.deltadak.plep.HomeworkTask;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * Command to delete an {@link HomeworkTask} from a TreeView.
 */
public class DeleteCommandJava extends Command {

    /** remember state */
    protected ProgressIndicator progressIndicator;
    protected LocalDate dayState;
    protected List<List<HomeworkTask>> treeViewItems; // A list of (lists containing a parent and it's children)
    protected TreeView<HomeworkTask> tree;
    protected int indexState;
    protected List<HomeworkTask> deletedItemsList; // It's a list, includes subtasks

    /**
     * Constructor, gets selected index to find out what to delete.
     * @param progressIndicator User feedback.
     * @param day from which to delete the task
     * @param treeViewItems corresponding to the day, items of listview, needed as parameter for testing (which can't use a listview)
     * @param index index of task in treeViewItems to delete, needed as parameter for testing (which can't use a listview)
     * @param tree to provide immediate user feedback
     */
    public DeleteCommandJava(ProgressIndicator progressIndicator, LocalDate day, List<List<HomeworkTask>> treeViewItems, int index, TreeView<HomeworkTask> tree) {
        this.progressIndicator = progressIndicator;
        this.dayState = day;
        this.treeViewItems = new ArrayList<>(treeViewItems);
        this.indexState = index;
        this.tree = tree;
    }

    @Override
    protected void executionHook() {
        if (treeViewItems.isEmpty()) {
            throw new IllegalStateException("cannot delete item from empty treeview");
        }
        // save the deleted tasks so we can undo later
        deletedItemsList = treeViewItems.get(indexState);
        treeViewItems.remove(indexState);

        // use the treeview to delete an item so the user gets feedback
        if (tree != null) {

            // get the selected item BEFORE deleting the item, otherwise
            // we're selecting a different item
            TreeItem<HomeworkTask> selected = tree.getSelectionModel()
                    .getSelectedItem();

            // Delete the item from the 'expanded' table, which contains information whether the item was expanded or not.
//            controller.deleteExpanded(selected.getValue().getDatabaseID());

            tree.getRoot().getChildren().remove(indexState);
        }

        // use the items of the listview to update the database
//        controller.pushData(dayState, treeViewItems);
//        System.out.println("id: " + deletedItemsList.get(0).getDatabaseID());
        Database.INSTANCE.deleteByID(deletedItemsList.get(0).getDatabaseID());
        new TreeViewCleaner().cleanSingleTreeView(tree);
    }

    @Override
    protected void undoHook() {
        treeViewItems.add(indexState, deletedItemsList);
        // if not testing, provide user feedback
        if (tree != null) {
            // add parent task
            TreeItem<HomeworkTask> parent = new TreeItem<>(deletedItemsList.get(0));
            tree.getRoot().getChildren().add(indexState, parent);
            // add subtasks
            // start at i=1 because first item is the parent task
            for (int i = 1; i < deletedItemsList.size(); i++) {
                parent.getChildren().add(new TreeItem<>(deletedItemsList.get(i)));
            }

            new DatabaseFacade(progressIndicator).pushData(dayState, treeViewItems);

//            int parentID = parent.getValue().getDatabaseID();
//            controller.insertExpandedItem(parentID, false);

            new TreeViewCleaner().cleanSingleTreeView(tree);

        }
    }

    /**
     * Get the treeViewItems this command remembers, mainly used for testing purposes.
     * @return treeViewItems of {@link HomeworkTask}s.
     */
    public List<List<HomeworkTask>> getTreeViewItems() {
        return treeViewItems;
    }

}

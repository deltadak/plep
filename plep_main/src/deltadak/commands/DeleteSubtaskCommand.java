package deltadak.commands;

import deltadak.HomeworkTask;
import deltadak.ui.AbstractController;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;

import java.time.LocalDate;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Delete a subtask only.
 */
public class DeleteSubtaskCommand extends DeleteCommand {

    protected HomeworkTask deletedTask;
    protected int indexWithinParent; /* Index of subtask in the list of children of it's parent */
    protected int parentIndex; /* Index of parent in treeview, only counting parents */

    /**
     * Delete a subtask, in contrast to a parent task.
     * @param controller See {@link DeleteCommand}
     * @param day See {@link DeleteCommand}
     * @param treeViewItems See {@link DeleteCommand}
     * @param index Index of the subtask, the list of lists is assumed flattened.
     * @param tree See {@link DeleteCommand}
     */
    public DeleteSubtaskCommand(AbstractController controller, LocalDate day, List<List<HomeworkTask>> treeViewItems,
                                int index, TreeView<HomeworkTask> tree) {
        super(controller, day, treeViewItems, index, tree);
    }

    @Override
    protected void executionHook() {
        if (treeViewItems.isEmpty()) {
            throw new IllegalStateException("cannot delete item from empty treeview");
        }

        // Flatten the list to use the index.
        List<HomeworkTask> flattenedList = treeViewItems.stream()
                .flatMap(Collection::stream)
                .collect(Collectors.toList());

        deletedTask = flattenedList.get(indexState);

        // Remove task from saved state.
        for (int i = 0; i < treeViewItems.size(); i++) {
            List<HomeworkTask> taskList = treeViewItems.get(i);
            if (taskList.contains(deletedTask)) {
                parentIndex = i;
                // Subtract one because the parent is the first item in the list.
                indexWithinParent = taskList.indexOf(deletedTask) - 1;
                taskList.remove(deletedTask);
            }
        }

        if (tree != null) {
            TreeItem<HomeworkTask> parent = tree.getRoot().getChildren().get(parentIndex);
            parent.getChildren().remove(indexWithinParent);

            controller.updateDatabase(dayState, treeViewItems);
            controller.cleanUp(tree);
        }
    }

    @Override
    protected void undoHook() {
        // We add one to the index because the first one in the list is the parent task, and we count from there.
        treeViewItems.get(parentIndex).add(indexWithinParent+1, deletedTask);

        // Tree will be null when testing
        if (tree != null) {
            TreeItem<HomeworkTask> parent = tree.getRoot().getChildren().get(parentIndex);
            parent.getChildren().add(indexWithinParent, new TreeItem<>(deletedTask));

            controller.updateDatabase(dayState, treeViewItems);
            controller.cleanUp(tree);
        }
    }

}

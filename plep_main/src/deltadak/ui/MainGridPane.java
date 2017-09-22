package deltadak.ui;

import deltadak.Database;
import deltadak.HomeworkTask;
import deltadak.commands.DeleteCommand;
import deltadak.commands.DeleteSubtaskCommand;
import deltadak.commands.UndoFacility;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.concurrent.Task;
import javafx.geometry.Rectangle2D;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.*;
import javafx.stage.Screen;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static deltadak.ui.Controller.MAX_COLUMNS_AUTO_NAME;
import static deltadak.ui.Controller.MAX_COLUMNS_NAME;

/**
 * The GridPane which contains everything.
 */
public class MainGridPane {

    /** The one and only controller. */
    private Controller controller;

    // FXML references passed from the controller.
    private GridPane gridPane;
    private ToolBar toolBar;
    private ProgressIndicator progressIndicator;

    /** keep a reference to the undo facility */
    private UndoFacility undoFacility = new UndoFacility();

    /**
     * Constructor.
     * @param controller The main controller object.
     * @param gridPane FXML reference to the GridPane.
     * @param toolBar FXML reference to the ToolBar.
     * @param progressIndicator FXML reference.
     */
    public MainGridPane(Controller controller,
                        GridPane gridPane,
                        ToolBar toolBar,
                        ProgressIndicator progressIndicator) {
        this.controller = controller;
        this.gridPane = gridPane;
        this.toolBar = toolBar;
        this.progressIndicator = progressIndicator;
    }

    /**
     * sets up listviews for each day, initializes drag and drop, editing items
     *
     * @param focusDate date that is the top middle one (is today on default)
     * @param numberOfDays Number of days to show in the GridPane.
     */
    public void setup(LocalDate focusDate, int numberOfDays) {

        addUndoKeyListener();

        boolean isAuto = Boolean.valueOf(
                Database.INSTANCE.getSetting(MAX_COLUMNS_AUTO_NAME));
        if(isAuto) {
            controller.maxColumns = Controller.maxColumns(numberOfDays);
        } else {
            controller.maxColumns = Integer.valueOf(Database.INSTANCE.getSetting(MAX_COLUMNS_NAME));
        }

        AnchorPane.setTopAnchor(gridPane, toolBar.getPrefHeight());

        // first clear the gridpane so we don't get titles overlaying each other
        gridPane.getChildren().clear();
        for (int index = 0; index < numberOfDays; index++) {

            // add days immediately, otherwise we can't use localDate in a
            // lambda expression (as it is not final)
            LocalDate localDate = focusDate.plusDays(index - 1);

            TreeItem<HomeworkTask> rootItem = new TreeItem<>(
                    new HomeworkTask());
            rootItem.setExpanded(true);
            final TreeView<HomeworkTask> tree = new TreeView<>(rootItem);

            tree.setEditable(true);
            tree.setCellFactory(param -> {
                CustomTreeCell treeCell = new CustomTreeCell(controller, tree.getRoot());
                treeCell.setup(tree, localDate);
                return treeCell;
            });

            tree.setShowRoot(false);

            VBox vbox = setTitle(tree, localDate);
            addVBoxToGridPane(vbox, index);

            // Request content on a separate thread, and hope the content
            // will be set eventually.
            refreshDay(tree, localDate);

            // add the delete key listener
            addDeleteKeyListener(tree, localDate);

            tree.setPrefWidth(getTreeViewWidth(controller.maxColumns));
            tree.setPrefHeight(getTreeViewHeight(controller.numberOfDays, controller.maxColumns));
        }

    }

    /**
     * add title to listview
     *
     * @param tree      to use
     * @param localDate from which to make a title
     * @return VBox with listview and title
     */
    private VBox setTitle(final TreeView<HomeworkTask> tree, final LocalDate localDate) {
        // vbox will contain a title above a list of tasks
        VBox vbox = new VBox();
        Label title = new Label(localDate.getDayOfWeek() + " " + localDate);
        // the pane is used to align both properly (I think)
        Pane pane = new Pane();
        vbox.getChildren().addAll(title, pane, tree);
        VBox.setVgrow(pane, Priority.ALWAYS);
        return vbox;
    }

    /**
     * add a box containing listview and title
     *
     * @param vbox  to be added
     * @param index at the i'th place (left to right, top to bottom)
     */
    private void addVBoxToGridPane(final VBox vbox, final int index) {
        int row = index / controller.maxColumns;
        int column = index % controller.maxColumns;
        gridPane.add(vbox, column, row);
    }

    /**
     * add a Listener to a list for the delete key
     *
     * @param tree      ListView to add the Listener to
     * @param localDate so we know for what day to update the database
     */
    private void addDeleteKeyListener(final TreeView<HomeworkTask> tree, final LocalDate localDate) {
        //add option to delete a task
        tree.setOnKeyPressed(event -> {

            if (event.getCode() == KeyCode.DELETE) {
                // Check whether we want to delete a parent task or subtask.
                if (tree.getSelectionModel().getSelectedItem().getParent().equals(tree.getRoot())) {
                    // Delete a parent task.
                    deleteParentTask(tree, localDate);
                } else {
                    // We want to delete a subtask.
                    deleteSubtask(tree, localDate);
                }
            }
        });
    }

    /**
     * Delete the selected parent task, and save to database.
     * @param tree TreeView in which the task lives.
     * @param localDate Date of the TreeView.
     */
    private void deleteParentTask(TreeView<HomeworkTask> tree, LocalDate localDate) {

        undoFacility.execute(
                new DeleteCommand(
                        controller,
                        localDate,
                        CustomTreeCell.convertTreeToArrayList(tree),
                        tree.getSelectionModel().getSelectedIndex(),
                        tree
                )
        );

    }

    /**
     * Delete the selected subtask.
     * @param tree TreeView in which the task lives.
     * @param localDate Date of the TreeView.
     */
    private void deleteSubtask(TreeView<HomeworkTask> tree, LocalDate localDate) {
        TreeItem<HomeworkTask> parentItem = tree.getSelectionModel().getSelectedItem().getParent();

        undoFacility.execute(
                new DeleteSubtaskCommand(
                        controller,
                        localDate,
                        CustomTreeCell.convertTreeToArrayList(tree),
                        tree.getSelectionModel().getSelectedIndex(),
                        tree
                )
        );
    }

    private void addUndoKeyListener() {
        gridPane.setOnKeyPressed(event -> {
            if (event.isControlDown() && (event.getCode() == KeyCode.Z)) {
                undoFacility.undo();
            }
        });
    }

    /**
     * get height by total screen size
     *
     * @return intended treeview height
     */
    private int getTreeViewHeight(int numberOfDays, int maxColumns) {
        Rectangle2D primaryScreenBounds = Screen.getPrimary().getVisualBounds();
        int totalHeight = (int) primaryScreenBounds.getHeight();
        return totalHeight / (numberOfDays / maxColumns);
    }

    /**
     * get width by total screen size
     *
     * @return intended treeview width
     */
    private int getTreeViewWidth(int maxColumns) {
        Rectangle2D primaryScreenBounds = Screen.getPrimary().getVisualBounds();
        int totalWidth = (int) primaryScreenBounds.getWidth();
        return totalWidth / maxColumns;
    }


    /**
     * @return all TreeViews in the gridPane
     */
    private List<TreeView<HomeworkTask>> getAllTreeViews() {
        List<TreeView<HomeworkTask>> listViews = new ArrayList<>();
        for (Node node : gridPane.getChildren()) {
            //gridpane contains vbox contains label, pane and treeview
            if (node instanceof VBox) {
                // we try to dig up the treeview in this vbox
                for (Node subNode : ((Pane) node).getChildren()) {
                    if (subNode instanceof TreeView) {
                        listViews.add((TreeView<HomeworkTask>) subNode);
                    }
                }
            }
        }
        return listViews;
    }

    /**
     * Refreshes all listviews using data from the database.
     */
    void refreshAllDays(int numberOfDays, LocalDate focusDay) {
        // find all treeviews from the gridpane
        List<TreeView<HomeworkTask>> treeViews = getAllTreeViews();

        for (int i = 0; i < numberOfDays; i++) {
            TreeView<HomeworkTask> tree = treeViews.get(i);
            // create a list to store if the items are expanded
            List<Boolean> expanded = new ArrayList<>();

            for (int j = 0; j < tree.getRoot().getChildren().size(); j++) {
                // loop through the tree to add all the booleans
                expanded.add(tree.getRoot().getChildren().get(j).isExpanded());
            }

            // refresh the treeview from database
            LocalDate localDate = focusDay.plusDays(i - 1);
            refreshDay(tree, localDate);
        }
    }


    // todo split up
    /**
     * Requests tasks from database, and when done updates the treeview.
     *
     * @param tree      TreeView to be updated.
     * @param localDate The day for which to request tasks.
     */
    public void refreshDay(TreeView<HomeworkTask> tree, LocalDate localDate) {
        progressIndicator.setVisible(true);


        Task task = new Task() {
            @Override
            public Boolean call() throws Exception {
                // below two database accessors take noticable time

                // get tasks from the database
                List<List<HomeworkTask>> allTasks = Database.INSTANCE.getTasksDay(localDate);
                // get the homework task ids and their corresponding expanded
                // state from the database, as tuples
                List<Map.Entry<Integer, Boolean>> allExpandedTasks = Database.INSTANCE.getExpanded();
                // list with the parent tasks
                ObservableList<HomeworkTask> list =
                        convertArrayToObservableList(Controller.getParentTasks(allTasks));
                // clear all the items currently showing in the TreeView
                tree.getRoot().getChildren().clear();

                // add the items from the database to the TreeView
                for (int i = 0; i < list.size(); i++) {
                    // add the parent task to the tree
                    TreeItem<HomeworkTask> item = new TreeItem<>(list.get(i));
                    tree.getRoot().getChildren().add(item);

                    // When expanded state changes, save to database.
                    item.expandedProperty().addListener(
                            (observable, oldValue, newValue) -> {
                                Database.INSTANCE.updateExpanded(
                                        item.getValue().getDatabaseID(),
                                        newValue);
                            });

                    // get the size of the current family, or the number of
                    // subtasks + 1 (the parent)
                    int familySize = allTasks.get(i).size();
                    // add every subtask to the tree as a child of the parent task
                    // we start at j=1 because the first item is the parent task
                    for (int j = 1; j < familySize; j++) {
                        // get the subtask
                        TreeItem<HomeworkTask> childTask = new TreeItem<>(
                                allTasks.get(i).get(j));
                        // add the subtask
                        tree.getRoot().getChildren().get(i).getChildren().add
                                (childTask);
                    }

                    // Insert an empty subtask at the end to allow the user to easily add more.
                    if (familySize > 1) {
                        tree.getRoot().getChildren().get(i).getChildren().add
                                (new TreeItem<>(new HomeworkTask()));
                    }
                }

                // Expand tasks according to database
                for (Map.Entry<Integer, Boolean> expandedPair : allExpandedTasks)
                {
                    // get the id of the homework task
                    int id = expandedPair.getKey();
                    // get its expanded state (boolean)
                    boolean expanded = expandedPair.getValue();

                    if(findTreeItemById(tree, id) != null) {
                        // set the expanded state on the tree item with
                        // the id of the tuple
                        findTreeItemById(tree, id).setExpanded(expanded);
                    }

                }
                return true;
            }
        };

        task.setOnSucceeded(e -> {
            controller.cleanUp(tree);
            progressIndicator.setVisible(false);

        });

        controller.exec.execute(task);
    }


    /**
     * convert (Array)List to ObservableList
     *
     * @param list - List to be converted
     * @return ObservableList
     */
    private ObservableList<HomeworkTask> convertArrayToObservableList(
            final List<HomeworkTask> list) {
        return FXCollections.observableList(list);
    }


    /**
     * Gets a TreeItem from the database, using its id.
     *
     * @param tree The tree to search for the tree item.
     * @param id The id of the tree item.
     * @return TreeItem<HomeworkTask>
     */
    private TreeItem<HomeworkTask> findTreeItemById(
            TreeView<HomeworkTask> tree, int id) {

        List<TreeItem<HomeworkTask>> parents = tree.getRoot().getChildren();

        for (TreeItem<HomeworkTask> parent : parents) {
            if (parent.getValue().getDatabaseID() == id) {
                return parent;
            }
        }
        return null;
    }

}

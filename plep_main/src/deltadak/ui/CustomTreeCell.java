package deltadak.ui;

import deltadak.Database;
import deltadak.HomeworkTask;
import javafx.beans.InvalidationListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.cell.TextFieldTreeCell;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.*;
import javafx.scene.text.TextAlignment;

import javax.xml.crypto.Data;
import java.time.LocalDate;
import java.util.List;
import java.util.Objects;

import static java.lang.Math.min;

/**
 * Custom TextFieldTreeCell, because we can't set the converter on a regular
 * TreeCell.
 */
public class CustomTreeCell extends TextFieldTreeCell<HomeworkTask> {

    /** Temporary fix for too long labels. Should equal the size of the combobox plus the size of the checkbox plus the size of the little arrow to view subtasks. */
    private int LABEL_MAGIK = 215;
    
    private ObservableList<String> comboList;
    private TreeItem<HomeworkTask> root; // the root item of the TreeView
    private Controller controller;
    
    private HBox cellBox;
    private CheckBox checkBox;
    private Label label;
    private ComboBox<String> comboBox;
    private ContextMenu contextMenu;
    
    /**
     * Each CustomTreeCell keeps a reference to the listener of the
     * ComboBox, in order to choose whether to block it temporarily or not.
     */
    ListenerWithBlocker labelChangeListener;
    
    /**
     * Constructor for the CustomTreeCell.
     * @param controller To keep a reference to the Controller to access
     *                   methods.
     * @param root The root of the TreeView this CustomTreeCell is a part of.
     */
    CustomTreeCell(Controller controller, TreeItem<HomeworkTask> root) {
        this.controller = controller;
        this.root = root;
    
        checkBox = new CheckBox();
        
        comboList = FXCollections
                .observableArrayList(Database.INSTANCE.getLabels());
        comboList.add(0, "<no label>");
    
        comboBox = new ComboBox<>(comboList);
    }
    
    /**
     * Adds to a cell:
     *  - the converter,
     *  - listeners for a changed value,
     *  - drag and drop listeners,
     *  - what has to happen when editing,
     *  - context menu.
     * @param tree The TreeView this TreeCell is a part of.
     * @param localDate The date to which this TreeView (and thus TreeCell) belongs.
     */
    public void setup(TreeView<HomeworkTask> tree, LocalDate localDate) {
        // update text on changes
        setConverter(new TaskConverter(this));
        // update label on changes
        comboBox.valueProperty().addListener(
                (observable, oldValue, newValue) -> {

                    HomeworkTask task = this.getTreeItem().getValue();

                    if (newValue.equals("<no label>")) {
                        task.setLabel("");
                        comboBox.setValue("");
                    } else {
                        task.setLabel(newValue);
                    }
                }
                
        );

        // If an item is selected, deselect all other items.
        tree.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) -> {
            if (newValue != null) {
                // We use our custom select method.
                select(
                        tree, () -> tree.getSelectionModel().select(newValue)
                );
            }
        });
        
        setOnLabelChangeListener(tree, localDate);
        setOnDoneChangeListener(tree, localDate);
    
        setOnDragDetected();
        setOnDragOver();
        setOnDragEntered(tree);
        setOnDragExited(tree);
        setOnDragDropped(tree, localDate);
        setOnDragDone(tree, localDate);

        setupSubtasksEditing(tree, localDate);
        
        // create the context menu
        contextMenu = createContextMenu(tree, localDate);
    }

    /**
     * Initialize what happens after the user edited a subtask.
     * @param tree The TreeView in which an item was edited.
     * @param localDate The day of the TreeView.
     */
    private void setupSubtasksEditing(TreeView<HomeworkTask> tree, LocalDate localDate) {
        tree.setOnEditCommit(event -> {
            TreeItem<HomeworkTask> editingItem = getTreeView().getEditingItem();

            // if we are editing one of the subtasks
            if(!editingItem.getParent().equals(root)) {
                // if we're not adding an empty task, create another subtask
                if(!event.getNewValue().getText().equals("")){
                    createSubTask(editingItem.getParent());
                }
            } else { // if we are not editing a subtask
                // insert the task in the expanded table
                controller.insertExpandedItem(editingItem.getValue()
                        .getDatabaseID(), false);
            }

            // update the database with the current first level items
            controller.updateDatabase(localDate, controller.convertTreeToArrayList(tree));
        });
    }

    /**
     * Select a TreeItem in a TreeView, and deselect all other items.
     * @param tree Any TreeView.
     * @param selectionFunction The function to select the right item, can select using index or TreeItems. e.g. tree.getSelectionModel().select(index)
     */
    private void select(TreeView<HomeworkTask> tree, Runnable selectionFunction) {
        // We want to clear the selection on all the other listviews, otherwise weird 'half-selected' greyed out cells are left behind.

        // A TreeView is inside a VBox inside the GridPane, so node will be a VBox
        // Hence we need to circumvent a little to clear selection of all other listviews
        for(Node node : tree.getParent().getParent().getChildrenUnmodifiable()) {
            if (node instanceof VBox) {
                // We assume the title (a Label) is first, Pane is second, treeview is third
                Node treeNode = ((VBox) node).getChildren().get(2);
                // First deselect all of them...
                if ((treeNode instanceof TreeView) ) {
                    ((TreeView) treeNode).getSelectionModel().clearSelection();
                }
            }

        }

        // ... then reselect this one
        selectionFunction.run();
    }
    
    /**
     * Sets the layout of a TreeCell. Always contains a CheckBox and
     * a Label (text). Also contains a ComboBox if the Cell is the root of a
     * task.
     * @param homeworkTask The Homework task to be displayed in the Cell.
     * @param empty Whether or not the new Cell displays data.
     */
    @Override
    public void updateItem(HomeworkTask homeworkTask, boolean empty) {
        super.updateItem(homeworkTask, empty);

        if(isEmpty()) {
            setGraphic(null);
            setText(null);
        } else {
            // create the items that are on every cell
            cellBox = new HBox(10);
            
            boolean done = homeworkTask.getDone();
            checkBox.setSelected(done);
            
            label = new Label(homeworkTask.getText());

            // This won't work for long text without spaces, and more.
//            label.prefWidthProperty().bind(getTreeView().widthProperty().subtract(comboBox.widthProperty()).subtract(checkBox.widthProperty()));
            // combobox.getWidth() is equal to 0 here, we can't use that.
            // Result:
            label.setPrefWidth(getTreeView().getWidth() - LABEL_MAGIK);

            // Get style from the database and apply to the item
            String color = homeworkTask.getColor();

            // On dark backgrounds, change text color.
            setWhiteComboboxText();

            setStyle("-fx-control-inner-background: "
                        + controller.convertColorToHex(color));

            // set the style on the label
            setDoneStyle(done);
    
            // if the item is first level, it has to show a course label
            // (ComboBox), and it has to have a context menu
            if(getTreeItem().getParent().equals(root)) {
    
                // Before setting value, we need to temporarily disable the
                // listener, otherwise it fires and goes unnecessarily updating
                // the database, which takes a lot of time.
                labelChangeListener.setBlock(true);
                comboBox.setValue((homeworkTask.getLabel() != null) ? homeworkTask.getLabel() : "<null>");
                labelChangeListener.setBlock(false);
                
                // create a region to make sure that the ComboBox is aligned
                // on the right
                Region region = new Region();
                HBox.setHgrow(region, Priority.ALWAYS);
                
                cellBox.getChildren().addAll(checkBox, label, region, comboBox);

                // set the context menu
                setContextMenu(contextMenu);
                setGraphic(cellBox);
                setText(null);
            } else {
                setContextMenu(null); // disable the context menu on subtasks
                cellBox.getChildren().addAll(checkBox, label);
                setGraphic(cellBox);
                setText(null);
            }
            
        }
    }
    
    /**
     * set listener on the ComboBox to update the database when the
     * selected index changes
     *
     * @param tree TreeView which the LabelCell is in, needed for updating
     *             the database
     * @param day LocalDate which we need for updating the database
     */
    private void setOnLabelChangeListener(TreeView<HomeworkTask> tree,
                                  LocalDate day) {
        
        InvalidationListener invalidationListener = observable -> {
            controller.updateDatabase(day, controller
                    .convertTreeToArrayList(tree));
            // We do not need to cleanup here, as no tasks
            // were added or deleted.
        };
        
        // Pass the invalidationlistener on to the custom listener
        labelChangeListener = new ListenerWithBlocker(invalidationListener);
        
        // update label in database when selecting a different one
        comboBox.getSelectionModel().selectedIndexProperty()
                .addListener(labelChangeListener);
    }
    
    /**
     * Sets a change listener on the CheckBox, to update the database on changes.
     * @param tree The TreeView the current TreeCell is in. We need this to update the database.
     * @param localDate The date of the TreeView, and thus all the HomeworkTasks, in which the CheckBox is toggled.
     */
    void setOnDoneChangeListener(TreeView<HomeworkTask> tree, LocalDate localDate) {
        checkBox.selectedProperty().addListener(
                (observable, oldValue, newValue) -> {
                    
                    getTreeItem().getValue().setDone(newValue);
    
                    // set the style on the label
                    if(label != null) {
                        setDoneStyle(newValue);
                    }

                    // Deselect the item, otherwise the selector changes color and overrides the item color.
                    select(tree, () -> {});
                    
                    /* If the item of which the checkbox is toggled is
                     * a subtask, then we check if all subtasks are done.
                     * If so, we mark its parent task as done.
                     */
                    if(!getTreeItem().getParent().equals(root)) {
                        
                        // the total number of subtasks of its parent
                        int totalSubtasks = getTreeItem().getParent()
                                .getChildren().size();
                        
                        // the number of those tasks that are marked as done
                        int doneSubtasks = getDoneSubtasks(getTreeItem().getParent());
                        
                        // if all the tasks are done, we mark the parent task
                        // as done
                        // This is a bit complicated by the idea that we always provide one more empty subtask to be edited.
                        // Which means that with more than one subtask, the total of done subtasks should be one less than the total.
                        // Border case: when there is only one subtask, it needs to be checked for the parent to be checked,
                        // so the amount of done subtasks needs to be at least one.
                        if((totalSubtasks == (doneSubtasks + 1)) && (doneSubtasks > 0)) {
                            // calling ...getparent().getValue().setDone(true)
                            // is not enough to trigger the event listener of
                            // the parent item
                            HomeworkTask parentOld = getTreeItem().getParent().getValue();
                            HomeworkTask parent = new
                                    HomeworkTask(true,
                                                 parentOld.getText(),
                                                 parentOld.getLabel(),
                                                 parentOld.getColor(),
                                                 parentOld.getDatabaseID());
                            getTreeItem().getParent().setValue(parent);
                        }
                    }
                    
                    controller.updateDatabase(localDate, controller
                            .convertTreeToArrayList(tree));
                });
    }
    
    /**
     * Creates a context menu to be able to add a subtask, repeat a task, or
     * change the colour of a task.
     * @param tree The TreeView this TreeCell is a part of.
     * @param day The day to which this TreeView (and thus TreeCell) belongs.
     * @return The ContextMenu.
     */
    ContextMenu createContextMenu(final TreeView<HomeworkTask> tree,
                           final LocalDate day) {
    
        // create the context menu
        ContextMenu contextMenu = new ContextMenu();
        
        // MenuItem to add a subtask
        MenuItem addSubTaskMenuItem = new MenuItem("Add subtask");
    
        // MenuItem that holds a menu to choose for how long to repeat the task
        Menu repeatTasksMenu = makeRepeatMenu(this, day);
        
        // a separator; a horizontal line
        SeparatorMenuItem separatorMenuItem = new SeparatorMenuItem();
    
        // the different colours to be added
        MenuItem firstColor = new MenuItem("Green");
        MenuItem secondColor = new MenuItem("Blue");
        MenuItem thirdColor = new MenuItem("Red");
        MenuItem defaultColor = new MenuItem("White");
    
        // add all the items to the context menu
        contextMenu.getItems()
                .addAll(addSubTaskMenuItem, repeatTasksMenu, separatorMenuItem,
                        firstColor, secondColor, thirdColor, defaultColor);
        
        addSubTaskMenuItem.setOnAction(event -> createSubTask(getTreeItem()));
        
        // sets an action on all the colour items
        for (int i = 2; i < contextMenu.getItems().size(); i++) {
            MenuItem colorMenuItem = contextMenu.getItems().get(i);
            colorMenuItem.setOnAction(event1 -> {
                controller.setBackgroundColor(colorMenuItem.getText(), this);
                controller.updateDatabase(day, controller
                        .convertTreeToArrayList(tree));
                controller.cleanUp(tree);
    
            });
        }
        return contextMenu;
    }
    
    /**
     * Creates an empty subtask, a child, of the parentItem.
     * @param parentItem The item to create a subtask in/under.
     */
    private void createSubTask(TreeItem parentItem) {
        // add a new subtask
        TreeItem<HomeworkTask> emptyItem = new TreeItem<>(
                new HomeworkTask());
        parentItem.getChildren().add(emptyItem);
        
        // select the new subtask
        getTreeView().getSelectionModel().select(emptyItem);
        // get the index of the new subtask
        int index = getTreeView().getSelectionModel().getSelectedIndex();
        // layout the TreeView again (otherwise we can't directly
        // edit an item)
        getTreeView().layout();
        // create a new TreeItem from the selected index, we need this
        // to do this to be able to edit it (pointer to emptyItem
        // is lost?)
        TreeItem<HomeworkTask> item = getTreeView().getTreeItem(index);
        // finnaly we can edit!
        getTreeView().edit(item);
        
    }
    
    /**
     * Creates the Menu to be able to choose for how long to repeat a task.
     * See {@link #createContextMenu}.
     * @param customTreeCell The TreeCell to show the context menu on.
     * @param day The day the TreeCell is in, to be able to calculate on what
     *           other days the task will have to be placed.
     * @return A drop down Menu.
     */
    private Menu makeRepeatMenu(CustomTreeCell customTreeCell, LocalDate day) {
        Menu repeatTasksMenu = new Menu("Repeat for x weeks");
        for (int i = 1; i < 9; i++) {
            MenuItem menuItem = new MenuItem(String.valueOf(i));
            repeatTasksMenu.getItems().add(menuItem);
        }
        
        List<MenuItem> repeatMenuItems = repeatTasksMenu.getItems();
        for (MenuItem repeatMenuItem : repeatMenuItems) {
            repeatMenuItem.setOnAction(event12 -> {
                int repeatNumber = Integer.valueOf(repeatMenuItem.getText());
                HomeworkTask homeworkTaskToRepeat = customTreeCell.getItem();
                repeatTask(repeatNumber, homeworkTaskToRepeat, day);
            });
        }
        return repeatTasksMenu;
    }
    
    /**
     * Repeats a task for a number of weeks.
     * @param repeatNumber The number of weeks to repeat the task.
     * @param homeworkTask The HomeworkTask to be repeated.
     * @param day The current day, to be able to calculate on what days to
     *            add the task.
     */
    private void repeatTask(final int repeatNumber, final HomeworkTask homeworkTask, LocalDate day) {
        for (int i = 0; i < repeatNumber; i++) {
            // add one week to the day to put the task there
            LocalDate newDay = day.plusWeeks(1);
            // copy the task and its subtasks to the new day in the database
            Database.INSTANCE.copyAndInsertTask(newDay, homeworkTask);
        }
        // refresh the console to see the newly copied tasks
        controller.refreshAllDays();
    }
    
    /**
     * Sets the style of the text of a task, depending on whether the task
     * is done or not.
     *
     * @param done boolean, true if the task is done, false if not done.
     */
    private void setDoneStyle(boolean done) {
        String color = getItem().getColor();

        if(done) {
            label.getStyleClass().remove("label");

            // Remove the white text of the combobox, which we set earlier when the background color is dark.
            comboBox.getStyleClass().remove("combobox-text-white");

            // Dark colors need a light text.
            switch (color) {
                case "Red":
                case "Blue":  // and item is selected
                    label.getStyleClass().add("label-done-light");
                    comboBox.getStyleClass().add("combobox-done-light");
                    break;
                case "Green":
                    label.getStyleClass().add("label-done-dark");
                    comboBox.getStyleClass().add("combobox-done-dark");
                    break;
                default:
                    label.getStyleClass().add("label-done");
                    comboBox.getStyleClass().add("combobox-done");
                    break;
            }

        } else {
            // Remove all the classes which styled the 'done' style on the item.
            label.getStyleClass().removeAll("label-done", "label-done-light", "label-done-dark");
            label.getStyleClass().add("label");

            comboBox.getStyleClass().removeAll("combobox-done", "combobox-done-light", "combobox-done-dark");

            // Re-add white text for dark backgrounds.
            setWhiteComboboxText();

        }
    }

    /**
     * Useful for dark background colors.
     */
    private void setWhiteComboboxText() {
        String color = getItem().getColor();

        if (color.equals("Red")) {
            comboBox.getStyleClass().add("combobox-text-white");
        } else {
            comboBox.getStyleClass().remove("combobox-text-white");
        }

    }

    /**
     * Counts the number of subtasks of taskTreeItem that are marked as done.
     *
     * @param taskTreeItem The TreeItem<HomeworkTask> of which to count the
     *                     done subtasks.
     * @return int, the number of subtasks that are marked as done.
     */
    private int getDoneSubtasks(TreeItem<HomeworkTask> taskTreeItem) {
        
        List<TreeItem<HomeworkTask>> subtasks = taskTreeItem.getChildren();
        int count = 0;
        for (TreeItem<HomeworkTask> subtask : subtasks) {
            if (subtask.getValue().getDone()) {
                count++;
            }
        }
        return count;
    }
    
    
    /**
     * When the dragging is detected, we place the content of the LabelCell
     * in the DragBoard.
     */
    void setOnDragDetected() {
        setOnDragDetected(event -> {

            boolean isParentTask = getTreeItem().getParent().equals(root);

            boolean isEmpty = getTreeItem().getValue().getText().equals("");

            if (!isEmpty && isParentTask) {
                Dragboard db = startDragAndDrop(TransferMode.MOVE);
                ClipboardContent content = new ClipboardContent();
                content.put(controller.DATA_FORMAT, getTreeItem()
                        .getValue());
                db.setContent(content);
            }
            event.consume();
        });
    }
    
    /**
     * Sets on drag over.
     */
    void setOnDragOver() {
        setOnDragOver(event -> {
            if (!Objects.equals(event.getGestureSource(), this) && event
                    .getDragboard().hasContent(controller.DATA_FORMAT)) {
                event.acceptTransferModes(TransferMode.MOVE);
            }
            event.consume();
        });
    }
    
    /**
     * Sets on drag entered.
     * @param tree TreeView to style as focused.
     */
    void setOnDragEntered(TreeView<HomeworkTask> tree) {
        setOnDragEntered(event -> {
            if ((!Objects.equals(event.getGestureSource(), this)) && event
                    .getDragboard().hasContent(controller.DATA_FORMAT)) {
                tree.setStyle("-fx-background-color: -fx-accent;");
            }
            
            event.consume();
        });
    }
    
    /**
     * Sets on drag exited.
     * @param tree TreeView to style as not focused.
     */
    void setOnDragExited(TreeView<HomeworkTask> tree) {
        setOnDragExited(event -> {
            tree.setStyle("-fx-background-color: -fx-base;");
            event.consume();
    
        });
    }
    
    /**
     * updates the ListView and database when a CustomTreeCell is being dropped
     *
     * @param tree TreeView needed for updating the database
     * @param day LocalDate needed for updating the database
     */
    void setOnDragDropped(final TreeView<HomeworkTask> tree, final LocalDate day) {
        setOnDragDropped(event -> {
            Dragboard db = event.getDragboard();
            boolean success = false;
            if (db.hasContent(controller.DATA_FORMAT)) {
                HomeworkTask newHomeworkTask
                        = (HomeworkTask)db.getContent(controller.DATA_FORMAT);
                //insert new task, removing will happen in onDragDone
                int index = min(getIndex(), tree.getRoot().getChildren()
                        .size()); // item can be dropped way below
                // the existing list
                
                //we have put an empty item instead of no items
                //because otherwise there are no treeCells that can
                // receive an item
                if (tree.getRoot().getChildren().get(index).getValue().getText().equals("")) {
                    tree.getRoot().getChildren().get(index)
                            .setValue(newHomeworkTask); //replace empty item
                } else {
                    TreeItem<HomeworkTask> item = new TreeItem<>(newHomeworkTask);
                    tree.getRoot().getChildren().add(index, item);
                }
                success = true;
                // update tasks in database (old day?)
                System.out.println("test"); // prints this every time you
                // drop something
                controller.updateParentDatabase(day,
                        controller.getParentTasks(
                            controller.convertTreeToArrayList(tree)
                        )
                );

                // Clear selection on all other items immediately. This will result in a smooth reaction, whereas otherwise it takes a bit of noticable time before selection of the just-dragged item (on its previous location) is cleared.
                select(tree,
                        () -> {}
                );
            }
            
            
            event.setDropCompleted(success);
            event.consume();
            // clean up immediately for a smooth reaction
            controller.cleanUp(tree);
            
            // works to let the subtasks show up after the drag, except when dragging a task with subtasks in the same list...
            controller.refreshAllDays();
        });
    }
    
    /**
     * removing the original copy
     *
     * @param tree TreeView needed for updating the database
     * @param day LocalDate needed for updating the database
     */
    void setOnDragDone(final TreeView<HomeworkTask> tree, final LocalDate day) {
        setOnDragDone(event -> {
            //ensures the original element is only removed on a
            // valid copy transfer (no dropping outside listviews)
            if (event.getTransferMode() == TransferMode.MOVE) {
                Dragboard db = event.getDragboard();
                HomeworkTask newHomeworkTask
                        = (HomeworkTask)db.getContent(controller.DATA_FORMAT);
                HomeworkTask emptyHomeworkTask = new HomeworkTask();
                //remove original item
                //item can have been moved up (so index becomes one
                // too much)
                // or such that the index didn't change, like to
                // another day
                
                // If item was moved to an other day, or down in same list
                TreeItem<HomeworkTask> currentItem = tree.getRoot().getChildren().get(getIndex());
                String currentText = currentItem.getValue().getText();
                // if text at current location is equal to
                String newText = newHomeworkTask.getText();

                if (currentText.equals(newText)) {
                    tree.getRoot().getChildren().get(getIndex())
                            .setValue(emptyHomeworkTask);
                    setGraphic(null);
                    
                    // deleting blank row from database which updating creates
                } else { // item was moved up in same tree
                    // we never get here...
                    int index = getIndex() + 1;
                    tree.getRoot().getChildren().get(index)
                            .setValue(emptyHomeworkTask);
                }

                // update in database (new day?)
                controller.updateParentDatabase(day,
                        controller.getParentTasks(
                            controller.convertTreeToArrayList(tree)
                        )
                );

            }
            event.consume();
            // clean up immediately for a smooth reaction
            controller.cleanUp(tree);
    
        });
    }
}

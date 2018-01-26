package deltadak.ui.taskcell;

import deltadak.Database;
import deltadak.HomeworkTask;
import deltadak.database.DatabaseFacade;
import deltadak.ui.Controller;
import deltadak.ui.taskcell.courselabel.OnChangeUpdater;
import deltadak.ui.taskcell.selection.SelectionCleaner;
import deltadak.ui.taskcell.selection.Selector;
import deltadak.ui.taskcell.subtasks.SubtasksCreator;
import deltadak.ui.taskcell.subtasks.SubtasksEditor;
import deltadak.ui.util.TreeToListConverter;
import javafx.beans.InvalidationListener;
import javafx.beans.value.ChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Pos;
import javafx.scene.control.*;
import javafx.scene.control.cell.TextFieldTreeCell;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.*;
import javafx.scene.text.TextAlignment;
import kotlin.Unit;

import java.time.LocalDate;
import java.util.List;
import java.util.Objects;

import static java.lang.Math.min;

/**
 * Custom TextFieldTreeCell, because we can't set the converter on a regular
 * TreeCell.
 */
public class TaskCell extends TextFieldTreeCell<HomeworkTask> {

    /** Temporary fix for too long labels. Should equal the size of the courselabel plus the size of the checkbox plus the size of the little arrow to view subtasks. */
    private int LABEL_MAGIK = 215;
    
    private ObservableList<String> comboList;
    private TreeItem<HomeworkTask> root; // the root item of the TreeView
    private Controller controller;
    
    private HBox cellBox;
    private CheckBox checkBox;
    private Label label;
    private ComboBox<String> comboBox;
    private ContextMenu contextMenu;
    
    // Use a number of spaces so when we color the label of a context menu
    // item, we get a decent looking colored area.
    private static final String LABEL_COLOR_CONTEXT_MENU_ITEMS =
            "                                ";
    
    /**
     * Each TaskCell keeps a reference to the listener of the
     * ComboBox, in order to choose whether to block it temporarily or not.
     */
    InvalidationListenerWithBlocker labelChangeListener;
    ChangeListenerWithBlocker doneChangeListener;
    
    /**
     * Constructor for the TaskCell.
     * @param controller To keep a reference to the Controller to access
     *                   methods.
     * @param root The root of the TreeView this TaskCell is a part of.
     */
    public TaskCell(Controller controller, TreeItem<HomeworkTask> root) {
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

        // update course label (a combobox) on changes
        new OnChangeUpdater(comboBox, this).addChangeListener();

        // If an item is selected, deselect all other items.
        new SelectionCleaner(tree).addSelectionListener();
        
        setOnLabelChangeListener(tree, localDate);
        setOnDoneChangeListener(tree, localDate);
    
        setOnDragDetected();
        setOnDragOver();
        setOnDragEntered(tree);
        setOnDragExited(tree);
        setOnDragDropped(tree, localDate);
        setOnDragDone(tree, localDate);

        new SubtasksEditor(controller, tree, localDate).setup();
        
        // create the context menu
        contextMenu = createContextMenu(tree, localDate);
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
            cellBox.setAlignment(Pos.CENTER_LEFT);
            
            // block the listener on the checkbox when we manually toggle it
            // so it corresponds to the value in the database
            doneChangeListener.setBlock(true);
                boolean done = homeworkTask.getDone();
                checkBox.setSelected(done); // manually toggle (if necessary)
            doneChangeListener.setBlock(false); // unblock the listener
            
            label = new Label(homeworkTask.getText());

            // This won't work for long text without spaces, and more.
//            label.prefWidthProperty().bind(getTreeView().widthProperty().subtract(comboBox.widthProperty()).subtract(checkBox.widthProperty()));
            // courselabel.getWidth() is equal to 0 here, we can't use that.
            // Result:
//            label.setPrefWidth(getTreeView().getWidth() - LABEL_MAGIK);
    
            // This works? :
            label.prefWidthProperty().bind(getTreeView().widthProperty()
                                                   .subtract(LABEL_MAGIK));
            label.setWrapText(true);
            label.setTextAlignment(TextAlignment.JUSTIFY);
    
            // Get style from the database and apply to the item
            int colorID = homeworkTask.getColorID();
            String color = controller.getColorFromDatabase(colorID);

            setStyle("-fx-control-inner-background: #" + color);
    
            // set the style on the label
            setDoneStyle(done);
    
            // if the item is first level, it has to show a course label
            // (ComboBox), and it has to have a context menu
            // Note: replacing equals() with a 'safe' equals() resolved NPE.
            if(Objects.equals(getTreeItem().getParent(), root)) {
    
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
            new DatabaseFacade(controller).updateDatabase(day, new TreeToListConverter().convertTreeToList(tree));
            // We do not need to cleanup here, as no tasks
            // were added or deleted.
        };
        
        // Pass the invalidationlistener on to the custom listener
        labelChangeListener = new InvalidationListenerWithBlocker(invalidationListener);
        
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
        
        ChangeListener<Boolean> changeListener =
                (observable, oldValue, newValue) -> {
                    getTreeItem().getValue().setDone(newValue);
    
                    // set the style on the label
                    if(label != null) {
                        setDoneStyle(newValue);
                    }
    
                    // Deselect the item, otherwise the selector changes color and overrides the item color.
                    new Selector(tree).select(() -> Unit.INSTANCE);
                    
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
                                                 parentOld.getColorID(),
                                                 parentOld.getExpanded(),
                                                 parentOld.getDatabaseID());
                            getTreeItem().getParent().setValue(parent);
                        }
                    }
    
                    new DatabaseFacade(controller).updateDatabase(localDate, new TreeToListConverter().convertTreeToList(tree));
                            
                };
        
        doneChangeListener = new ChangeListenerWithBlocker<Boolean>(changeListener);
        
        checkBox.selectedProperty().addListener(doneChangeListener);
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
        MenuItem firstColor = new MenuItem(LABEL_COLOR_CONTEXT_MENU_ITEMS);
        MenuItem secondColor = new MenuItem(LABEL_COLOR_CONTEXT_MENU_ITEMS);
        MenuItem thirdColor = new MenuItem(LABEL_COLOR_CONTEXT_MENU_ITEMS);
        MenuItem fourthColor = new MenuItem(LABEL_COLOR_CONTEXT_MENU_ITEMS);
        MenuItem defaultColor = new MenuItem(LABEL_COLOR_CONTEXT_MENU_ITEMS);
    
        String[] colorsFromDatabase = Database.INSTANCE.getColorsFromDatabase();
        firstColor.setStyle("-fx-background-color: #" + colorsFromDatabase[0]);
        secondColor.setStyle("-fx-background-color: #" + colorsFromDatabase[1]);
        thirdColor.setStyle("-fx-background-color: #" + colorsFromDatabase[2]);
        fourthColor.setStyle("-fx-background-color: #" + colorsFromDatabase[3]);
        defaultColor.setStyle("-fx-background-color: #" + colorsFromDatabase[4]);
    
    
        // add all the items to the context menu
        contextMenu.getItems()
                .addAll(addSubTaskMenuItem, repeatTasksMenu, separatorMenuItem,
                        firstColor, secondColor, thirdColor, fourthColor, defaultColor);
        
        addSubTaskMenuItem.setOnAction(event -> new SubtasksCreator(tree).create(getTreeItem()));
        
        // sets an action on all the colour items
        for (int i = 3; i < contextMenu.getItems().size(); i++) {
            MenuItem colorMenuItem = contextMenu.getItems().get(i);
            int colorID = i-3;
            String colorString = colorsFromDatabase[colorID];
            
            colorMenuItem.setOnAction(event1 -> {
//                setStyle("-fx-background: #98ab7c");
                System.out.println("item " + colorString + " clicked");
                controller.setBackgroundColor(colorID, this);
                getTreeItem().getValue().setColorID(colorID);
                new DatabaseFacade(controller).updateDatabase(day, new TreeToListConverter().convertTreeToList(tree));
                controller.cleanUp(tree);
    
            });
        }
        return contextMenu;
    }
    
    /**
     * Creates the Menu to be able to choose for how long to repeat a task.
     * See {@link #createContextMenu}.
     * @param taskCell The TreeCell to show the context menu on.
     * @param day The day the TreeCell is in, to be able to calculate on what
     *           other days the task will have to be placed.
     * @return A drop down Menu.
     */
    private Menu makeRepeatMenu(TaskCell taskCell, LocalDate day) {
        Menu repeatTasksMenu = new Menu("Repeat for x weeks");
        for (int i = 1; i < 9; i++) {
            MenuItem menuItem = new MenuItem(String.valueOf(i));
            repeatTasksMenu.getItems().add(menuItem);
        }
        
        List<MenuItem> repeatMenuItems = repeatTasksMenu.getItems();
        for (MenuItem repeatMenuItem : repeatMenuItems) {
            repeatMenuItem.setOnAction(event12 -> {
                int repeatNumber = Integer.valueOf(repeatMenuItem.getText());
                HomeworkTask homeworkTaskToRepeat = taskCell.getItem();
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

        if(done) {
            label.getStyleClass().remove("label");
            label.getStyleClass().add("label-done");
            
            comboBox.getStyleClass().remove("combo-box");
            comboBox.getStyleClass().add("courselabel-done");

        } else {
            // Remove all the classes which styled the 'done' style on the item.
            label.getStyleClass().removeAll("label-done");
            label.getStyleClass().add("label");

            comboBox.getStyleClass().removeAll("courselabel-done");
            comboBox.getStyleClass().add("combo-box");

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
     * updates the ListView and database when a TaskCell is being dropped
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
                controller.updateParentDatabase(day,
                        controller.getParentTasks(
                                new TreeToListConverter().convertTreeToList(tree)
                        )
                );

                // Clear selection on all other items immediately. This will result in a smooth reaction, whereas otherwise it takes a bit of noticable time before selection of the just-dragged item (on its previous location) is cleared.
                new Selector(tree).select(() -> Unit.INSTANCE);

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
                    // we get here when dragging a task from below an
                    // expanded task
                    int index = getIndex() + 1;
                    tree.getTreeItem(index).setValue(emptyHomeworkTask);
                }

                // update in database (new day?)
                controller.updateParentDatabase(day,
                        controller.getParentTasks(
                                new TreeToListConverter().convertTreeToList(tree)
                        )
                );

            }
            event.consume();
            // clean up immediately for a smooth reaction
            controller.cleanUp(tree);
    
        });
    }
}

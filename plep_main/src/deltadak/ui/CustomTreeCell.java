package deltadak.ui;

import deltadak.Database;
import deltadak.HomeworkTask;
import javafx.beans.InvalidationListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.control.cell.TextFieldTreeCell;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;

import java.time.LocalDate;
import java.util.List;
import java.util.Objects;

import static java.lang.Math.min;

/**
 * Custom TextFieldTreeCell, because we can't set the converter on a regular
 * TreeCell.
 */
public class CustomTreeCell extends TextFieldTreeCell<HomeworkTask> {
    
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
     * @param localDate The date to which this TreeView (and thus TreeCell)
     *                  belong.
     */
    public void setup(TreeView<HomeworkTask> tree, LocalDate localDate) {
        setConverter(new TaskConverter(this));
        
        comboBox.valueProperty().addListener(
                (observable, oldValue, newValue) -> this.getTreeItem()
                    .getValue().setLabel(newValue)
                
        );
        
        setOnLabelChangeListener(tree, localDate);
        setOnDoneChangeListener(tree, localDate);
        
        setOnDragDetected();
        setOnDragOver();
        setOnDragEntered();
        setOnDragExited();
        setOnDragDropped(tree, localDate);
        setOnDragDone(tree, localDate);
        
        tree.setOnEditCommit(event -> {
            TreeItem<HomeworkTask> editingItem = getTreeView().getEditingItem();
            // if we are editing one of the subtasks
            if(!editingItem.getParent().equals(root)) {
                // if we're not adding an empty task, create another subtask
                if(!event.getNewValue().getText().equals("")){
                    createSubTask(editingItem.getParent());
                }
            }
            
            // update the database with the current first level items
            controller.updateDatabase(localDate, controller.convertTreeToArrayList(tree));
        });
        
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
            
            boolean done = homeworkTask.getDone();
            checkBox.setSelected(done);
            
            label = new Label(homeworkTask.getText());
            setStyle("-fx-control-inner-background: "
                     + controller.convertColorToHex(homeworkTask.getColor()));
            
            // if the item is first level, it has to show a course label
            // (ComboBox), and it has to have a context menu
            if(getTreeItem().getParent().equals(root)) {
    
                // Before setting value, we need to temporarily disable the
                // listener, otherwise it fires and goes unnecessarily updating
                // the database, which takes a lot of time.
                labelChangeListener.setBlock(true);
                comboBox.setValue((homeworkTask.getLabel() != null) ?
                                  homeworkTask.getLabel() : "<null>");
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
     * Sets a change listener on the CheckBox, to update the database on
     * changes.
     * @param tree The TreeView the current TreeCell is in. We need this to
     *            update the database.
     * @param localDate The date of the TreeView, and thus all the
     *                  HomeworkTasks, in which the CheckBox is toggled.
     */
    void setOnDoneChangeListener(TreeView<HomeworkTask> tree, LocalDate localDate) {
        checkBox.selectedProperty().addListener(
                (observable, oldValue, newValue) -> {
                    getTreeItem().getValue().setDone(newValue);
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
                System.out.println(colorMenuItem.getText() + " clicked");
                controller.setBackgroundColor(colorMenuItem, this);
                controller.updateDatabase(day, controller
                        .convertTreeToArrayList(tree));
                controller.cleanUp(tree);
    
            });
        }
        return contextMenu;
    }
    
    /**
     * Creates a subtask, or a child, of the parentItem.
     * @param parentItem The item to create a subtask in/under.
     */
    private void createSubTask(TreeItem parentItem) {
        // add a new subtask
        TreeItem<HomeworkTask> emptyItem = new TreeItem<>(
                new HomeworkTask(false, "", "", "White", -1));
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
                System.out.println(repeatNumber + " clicked");
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
            // TODO
//            day = day.plusWeeks(1);
//            List<HomeworkTask> homeworkTasks = controller.getDatabaseSynced
//                    (day);
//            homeworkTasks.add(homeworkTask);
//            controller.updateDatabase(day, homeworkTasks);
        }
        controller.refreshAllDays();
    }
    
    /**
     * When the dragging is detected, we place the content of the LabelCell
     * in the DragBoard.
     */
    void setOnDragDetected() {
        setOnDragDetected(event -> {
            if (!getTreeItem().getValue().getText().equals("")) {
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
     */
    void setOnDragEntered() {
        setOnDragEntered(event -> {
            if ((!Objects.equals(event.getGestureSource(), this)) && event
                    .getDragboard().hasContent(controller.DATA_FORMAT)) {
                //                System.out.println("TODO: change color of listview"); //todo
            }
            
            event.consume();
        });
    }
    
    /**
     * Sets on drag exited.
     */
    void setOnDragExited() {
        setOnDragExited(event -> {
            //            System.out.println("TODO reset color of listview"); //todo
    
            event.consume();
    
        });
    }
    
    /**
     * updates the ListView and database when a CustomTreeCell is being dropped
     *
     * @param tree TreeView needed for updating the database
     * @param day LocalDate needed for updating the database
     */
    void setOnDragDropped(final TreeView<HomeworkTask> tree,
                          final LocalDate day) {
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
                    tree.getRoot().getChildren().add(item);
                }
                success = true;
                // update tasks in database
                controller.updateParentDatabase(day,
                        controller.getParentTasks(
                            controller.convertTreeToArrayList(tree)
                        )
                );
            }
            
            
            event.setDropCompleted(success);
            event.consume();
            // clean up immediately for a smooth reaction
            controller.cleanUp(tree);
            
            // works to le the subtasks show up after the drag, except when dragging a task with subtasks in the same list...
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
                HomeworkTask emptyHomeworkTask = new HomeworkTask(
                        false, "", "", "White", -1);
                //remove original item
                //item can have been moved up (so index becomes one
                // too much)
                // or such that the index didn't change, like to
                // another day
                
                // If item was moved to an other day, or down in same list
                if (tree.getRoot().getChildren().get(getIndex())
                            .getValue().getText()
                                .equals(newHomeworkTask.getText())) {
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
                
                // update in database
                controller.updateParentDatabase(day,
                        controller.getParentTasks(
                            controller.convertTreeToArrayList(tree)
                        )
                );
                
                // prevent an empty list from refusing to receive
                // items, as it wouldn't contain any treecell
                if (tree.getRoot().getChildren().size() < 1) {
                    TreeItem<HomeworkTask> item =
                            new TreeItem<>(emptyHomeworkTask);
                    tree.getRoot().getChildren().add(item);
                }
            }
            event.consume();
            // clean up immediately for a smooth reaction
            controller.cleanUp(tree);
    
        });
    }
}

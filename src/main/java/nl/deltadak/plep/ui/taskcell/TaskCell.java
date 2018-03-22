package nl.deltadak.plep.ui.taskcell;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Pos;
import javafx.scene.control.*;
import javafx.scene.control.cell.TextFieldTreeCell;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.text.TextAlignment;
import nl.deltadak.plep.Database;
import nl.deltadak.plep.HomeworkTask;
import nl.deltadak.plep.database.ContentProvider;
import nl.deltadak.plep.database.DatabaseFacade;
import nl.deltadak.plep.ui.Controller;
import nl.deltadak.plep.ui.draganddrop.DragDetection;
import nl.deltadak.plep.ui.draganddrop.DragOver;
import nl.deltadak.plep.ui.taskcell.blockerlisteners.ChangeListenerWithBlocker;
import nl.deltadak.plep.ui.taskcell.blockerlisteners.InvalidationListenerWithBlocker;
import nl.deltadak.plep.ui.taskcell.checkbox.CheckBoxUpdater;
import nl.deltadak.plep.ui.taskcell.contextmenu.ContextMenuCreator;
import nl.deltadak.plep.ui.taskcell.courselabel.OnCourseLabelChangeUpdater;
import nl.deltadak.plep.ui.taskcell.selection.SelectionCleaner;
import nl.deltadak.plep.ui.taskcell.selection.Selector;
import nl.deltadak.plep.ui.taskcell.subtasks.SubtasksEditor;
import nl.deltadak.plep.ui.taskcell.textlabel.TextLabelStyle;
import nl.deltadak.plep.ui.taskcell.treecell.TaskConverter;
import nl.deltadak.plep.ui.treeview.TreeViewCleaner;
import nl.deltadak.plep.ui.util.converters.ConvertersKt;

import java.time.LocalDate;
import java.util.Objects;

import static java.lang.Math.min;
import static nl.deltadak.plep.ui.draganddrop.UtilKt.DATA_FORMAT;
import static nl.deltadak.plep.ui.util.converters.ConvertersKt.getParentTasks;

/**
 * Custom TextFieldTreeCell, because we can't set the converter on a regular
 * TreeCell.
 */
public class TaskCell extends TextFieldTreeCell<HomeworkTask> {

    /** Temporary fix for too long labels. Should equal the size of the courselabel plus the size of the checkbox plus the size of the little arrow to view subtasks. */
    private int LABEL_MAGIK = 215;

    /** This is the TreeView this TaskCel is in. */
    public TreeView<HomeworkTask> tree;
    
    private ObservableList<String> comboList;
    private TreeItem<HomeworkTask> root; // the root item of the TreeView
    private Controller controller;
    
    private HBox cellBox;
    private ContextMenu contextMenu;
    
    /** The CheckBox of this TaskCell. */
    public CheckBox checkBox;
    /** The Label of this TaskCell. */
    public Label label;
    /** The ComboBox of this TaskCell. */
    public ComboBox<String> comboBox;
    
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
     * @param progressIndicator User feedback.
     * @param gridPane Main UI element.
     * @param focusDay Focusday of UI.
     */
    public void setup(TreeView<HomeworkTask> tree, LocalDate localDate, ProgressIndicator progressIndicator, GridPane gridPane, LocalDate focusDay) {
        this.tree = tree;

        // update text on changes
        setConverter(new TaskConverter(this));

        // update course label (a combobox) on changes
        OnCourseLabelChangeUpdater onCourseLabelChangeUpdater
                = new OnCourseLabelChangeUpdater(controller.getProgressIndicator(), comboBox);
        onCourseLabelChangeUpdater.addValueChangeListener(this);
        labelChangeListener = onCourseLabelChangeUpdater
                .addDatabaseUpdaterChangeListener(tree, localDate);

        // If an item is selected, deselect all other items.
        new SelectionCleaner(tree).addSelectionListener();
        
        doneChangeListener = new CheckBoxUpdater(controller.getProgressIndicator(), checkBox).setChangeListener(tree, this, localDate);
    
        new DragDetection(this, root);
        new DragOver(this);
        setOnDragEntered(tree);
        setOnDragExited(tree);
        setOnDragDropped(tree, localDate, progressIndicator, gridPane, focusDay);
        setOnDragDone(tree, localDate, progressIndicator);

        new SubtasksEditor(controller.getProgressIndicator(), tree, localDate).setup();
        
        // create the context menu
        contextMenu = new ContextMenuCreator(controller, gridPane, focusDay, controller.getProgressIndicator(), this, localDate).create();
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
            String color = Database.INSTANCE.getColorFromDatabase(colorID);

            setStyle("-fx-control-inner-background: #" + color);
    
            // set the style on the label
            new TextLabelStyle().setDoneStyle(done, label, comboBox);
    
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
     * Sets on drag entered.
     * @param tree TreeView to style as focused.
     */
    void setOnDragEntered(TreeView<HomeworkTask> tree) {
        setOnDragEntered(event -> {
            if ((!Objects.equals(event.getGestureSource(), this)) && event
                    .getDragboard().hasContent(DATA_FORMAT)) {
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
     * @param progressIndicator User feedback.
     */
    void setOnDragDropped(final TreeView<HomeworkTask> tree, final LocalDate day, ProgressIndicator progressIndicator, GridPane gridPane, LocalDate focusDay) {
        setOnDragDropped(event -> {
            Dragboard db = event.getDragboard();
            boolean success = false;
            if (db.hasContent(DATA_FORMAT)) {
                HomeworkTask newHomeworkTask
                        = (HomeworkTask)db.getContent(DATA_FORMAT);
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
                // we only have to update the parents, because the subtasks only depend on their parents, and are independent of the day and the order in the day.
                new DatabaseFacade(progressIndicator).pushParentData(day,
                        getParentTasks(
                                ConvertersKt.toHomeworkTaskList(tree)
                        )
                );

                // Clear selection on all other items immediately. This will result in a smooth reaction, whereas otherwise it takes a bit of noticable time before selection of the just-dragged item (on its previous location) is cleared.
                new Selector(tree).deselectAll();

            }
            
            
            event.setDropCompleted(success);
            event.consume();
            // clean up immediately for a smooth reaction
            new TreeViewCleaner().cleanSingleTreeView(tree);

            // works to let the subtasks show up after the drag, except when dragging a task with subtasks in the same list...
            new ContentProvider().setForAllDays(gridPane, focusDay, progressIndicator); // todo this is overkill refresh
        });
    }
    
    /**
     * removing the original copy
     *
     * @param tree TreeView needed for updating the database
     * @param day LocalDate needed for updating the database
     */
    void setOnDragDone(final TreeView<HomeworkTask> tree, final LocalDate day, ProgressIndicator progressIndicator) {
        setOnDragDone(event -> {
            //ensures the original element is only removed on a
            // valid copy transfer (no dropping outside listviews)
            if (event.getTransferMode() == TransferMode.MOVE) {
                Dragboard db = event.getDragboard();
                HomeworkTask newHomeworkTask
                        = (HomeworkTask)db.getContent(DATA_FORMAT);
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
                // we only have to update the parents, because the subtasks only depend on their parents, and are independent of the day and the order in the day.
                new DatabaseFacade(progressIndicator).pushParentData(day,
                        getParentTasks(
                                ConvertersKt.toHomeworkTaskList(tree)
                        )
                );

            }
            event.consume();
            // clean up immediately for a smooth reaction
            new TreeViewCleaner().cleanSingleTreeView(tree);

        });
    }
}

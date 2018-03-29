package nl.deltadak.plep.ui.taskcell;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.*;
import javafx.scene.control.cell.TextFieldTreeCell;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import nl.deltadak.plep.Database;
import nl.deltadak.plep.HomeworkTask;
import nl.deltadak.plep.ui.Controller;
import nl.deltadak.plep.ui.draganddrop.*;
import nl.deltadak.plep.ui.taskcell.components.checkbox.CheckBoxUpdater;
import nl.deltadak.plep.ui.taskcell.components.courselabel.OnCourseLabelChangeUpdater;
import nl.deltadak.plep.ui.taskcell.contextmenu.ContextMenuCreator;
import nl.deltadak.plep.ui.taskcell.selection.SelectionCleaner;
import nl.deltadak.plep.ui.taskcell.subtasks.SubtasksEditor;
import nl.deltadak.plep.ui.taskcell.treecell.TaskConverter;
import nl.deltadak.plep.ui.taskcell.util.blockerlisteners.ChangeListenerWithBlocker;
import nl.deltadak.plep.ui.taskcell.util.blockerlisteners.InvalidationListenerWithBlocker;

import java.time.LocalDate;

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
    ChangeListenerWithBlocker<Boolean> doneChangeListener;
    
    /**
     * Constructor for the TaskCell.
     * @param controller To keep a reference to the Controller to access
     *                   methods.
     * @param root The root of the TreeView this TaskCell is a part of.
     */
    public TaskCell(Controller controller, TreeItem<HomeworkTask> root) {
        this.controller = controller;
        this.root = root;

        // Initialize components with something default.
    
        checkBox = new CheckBox();
        
        comboList = FXCollections
                .observableArrayList(Database.INSTANCE.getLabels());
        comboList.add(0, "<no label>");
    
        comboBox = new ComboBox<>(comboList);
        label = new Label("");
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
    
        new DragDetect(this, root);
        new DragOver(this);
        new DragEnter(this, tree);
        new DragExit(this, tree);
        new DragDrop(this, tree, localDate, progressIndicator);
        new DragDone(this, tree, localDate, progressIndicator);

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

        new TaskLayout(this, doneChangeListener, checkBox, label, labelChangeListener, contextMenu, root, comboBox).update(homeworkTask);
    }

}

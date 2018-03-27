package nl.deltadak.plep.ui.taskcell;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Pos;
import javafx.scene.control.*;
import javafx.scene.control.cell.TextFieldTreeCell;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.text.TextAlignment;
import nl.deltadak.plep.Database;
import nl.deltadak.plep.HomeworkTask;
import nl.deltadak.plep.ui.Controller;
import nl.deltadak.plep.ui.draganddrop.*;
import nl.deltadak.plep.ui.taskcell.util.blockerlisteners.ChangeListenerWithBlocker;
import nl.deltadak.plep.ui.taskcell.util.blockerlisteners.InvalidationListenerWithBlocker;
import nl.deltadak.plep.ui.taskcell.components.checkbox.CheckBoxUpdater;
import nl.deltadak.plep.ui.taskcell.contextmenu.ContextMenuCreator;
import nl.deltadak.plep.ui.taskcell.components.courselabel.OnCourseLabelChangeUpdater;
import nl.deltadak.plep.ui.taskcell.selection.SelectionCleaner;
import nl.deltadak.plep.ui.taskcell.subtasks.SubtasksEditor;
import nl.deltadak.plep.ui.taskcell.components.textlabel.TextLabelStyle;
import nl.deltadak.plep.ui.taskcell.treecell.TaskConverter;

import java.time.LocalDate;
import java.util.Objects;

import static java.lang.Math.min;

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

}

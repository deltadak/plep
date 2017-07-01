package deltadak.ui;

import deltadak.Database;
import deltadak.HomeworkTask;
import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.cell.TextFieldListCell;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;

import java.time.LocalDate;
import java.util.Objects;

import static java.lang.Integer.min;

/**
 * custom ListCell
 */
public class LabelCell extends TextFieldListCell<HomeworkTask> {

    private Controller controller;
    HBox hbox = new HBox();
    Label text = new Label("");
    Pane pane = new Pane();
    ObservableList<String> comboList;
    ComboBox<String> comboBox;
    
    /**
     * Each LabelCell keeps a reference to the listener of the
     * ComboBox, in order to choose whether to block it temporarily or not.
     */
    ListenerWithBlocker labelChangeListener;
    
    // used to set the width of the text as
    // size of the listview - size of the combobox
    private static final double COMBOBOX_DEFAULT_WIDTH = 130;
    
    /**
     * Constructor.
     * @param controller a private controller object to access some methods.
     */
    LabelCell(Controller controller) {
        super();
        this.controller = controller;
        comboList = FXCollections
                .observableArrayList(Database.INSTANCE.getLabels());
        comboBox = new ComboBox<>(comboList);
        hbox.getChildren().addAll(text, pane, comboBox);
        HBox.setHgrow(pane, Priority.ALWAYS);
        
        
    }
    
    /**
     * sets up a labelcell, e.g. adding drag and drop listeners
     * @param list listview in which the labelcell resides
     * @param day localdate which belongs to the listview
     */
    public void setup(ListView<HomeworkTask> list, LocalDate day) {
        //update text on changes
        setConverter(new TaskConverter(this));
        
        // update label on changes
        comboBox.valueProperty().addListener(
                (observable, oldValue, newValue) -> this.getItem()
                        .setLabel(newValue));
    
        setOnLabelChangeListener(list, day);
    
        setOnDragDetected();
        setOnDragOver();
        setOnDragEntered();
        setOnDragExited();
        setOnDragDropped(list, day);
        setOnDragDone(list, day);
    
        setRightMouseClickListener(list, day);
    }
    
    /**
     * called when starting edit with (null, true)
     * and when finished edit with (homeworkTask, false)
     *
     * @param homeworkTask
     *         to be updated
     * @param empty
     *         whether to set empty?
     */
    @Override
    public void updateItem(final HomeworkTask homeworkTask, final boolean empty) {
        super.updateItem(homeworkTask, empty);
        setText(null);
        if (empty) {
            setGraphic(null);
        } else {
            text.prefWidthProperty().bind(
                    getListView().widthProperty()
                            .subtract(COMBOBOX_DEFAULT_WIDTH));
            text.setWrapText(true);
            text.setText(
                    (homeworkTask.getText() != null) ? homeworkTask.getText() : "<null>");
            
            // Before setting value, we need to temporarily disable the
            // listener, otherwise it fires and goes unnecessarily updating
            // the database, which takes a lot of time.
            labelChangeListener.setBlock(true);
            comboBox.setValue(
                    (homeworkTask.getLabel() != null) ? homeworkTask
                            .getLabel() : "<null>");
            labelChangeListener.setBlock(false);
            
            setGraphic(hbox);
            setStyle("-fx-control-inner-background: "
                             + controller.convertColorToHex(homeworkTask.getColor()));
            
        }
    }
    
    /**
     * set listener on the ComboBox to update the database when the
     * selected index changes
     *
     * @param list ListView which the LabelCell is in, needed for updating
     *             the database
     * @param day LocalDate which we need for updating the database
     */
    void setOnLabelChangeListener(ListView<HomeworkTask> list,
                                          LocalDate day) {
        
        InvalidationListener invalidationListener = new InvalidationListener() {
            @Override
            public void invalidated(Observable observable) {
                controller.updateDatabase(day, controller
                        .convertObservableListToArrayList(
                                list.getItems()));
                // We do not need to cleanup here, as no tasks
                // were added or deleted.
            }
        };
        
        // Pass the invalidationlistener on to the custom listener
        labelChangeListener = new ListenerWithBlocker(invalidationListener);
    
        // update label in database when selecting a different one
        comboBox.getSelectionModel().selectedIndexProperty()
                .addListener(labelChangeListener);
    }
    
    /**
     * set ClickListener on a LabelCell, to be able to display the
     * ContextMenu on a RightClick
     *
     * @param list ListView is needed for updating
     * @param day LocalDate is needed for updating
     */
    void setRightMouseClickListener(final ListView<HomeworkTask> list,
                                            final LocalDate day) {
    
        addEventHandler(MouseEvent.MOUSE_CLICKED, event -> {
            if (event.getButton() == MouseButton.SECONDARY) {
                controller.createContextMenu(event, this, list, day);
            }
        });
    }
    
    /**
     * When the dragging is detected, we place the content of the LabelCell
     * in the DragBoard.
     */
    void setOnDragDetected() {
        setOnDragDetected((MouseEvent event) -> {
            if (!getItem().getText().equals("")) {
                Dragboard db = startDragAndDrop(TransferMode.MOVE);
                ClipboardContent content = new ClipboardContent();
                content.put(controller.DATA_FORMAT, getItem());
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
     * updates the ListView and database when a LabelCell is being dropped
     *
     * @param list ListView needed for updating the database
     * @param day LocalDate needed for updating the database
     */
    void setOnDragDropped(final ListView<HomeworkTask> list,
                                  final LocalDate day) {
        setOnDragDropped(event -> {
            Dragboard db = event.getDragboard();
            boolean success = false;
            if (db.hasContent(controller.DATA_FORMAT)) {
                HomeworkTask newHomeworkTask
                        = (HomeworkTask)db.getContent(controller.DATA_FORMAT);
                //insert new task, removing will happen in onDragDone
                int index = min(getIndex(), list.getItems()
                        .size()); // item can be dropped way below
                // the existing list
                //we have put an empty item instead of no items
                //because otherwise there are no listCells that can
                // receive an item
                if (list.getItems().get(index).getText().equals("")) {
                    list.getItems().set(index, newHomeworkTask); //replace empty item
                } else {
                    list.getItems().add(index, newHomeworkTask);
                }
                success = true;
                // update tasks in database
                controller.updateDatabase(
                        day, controller.convertObservableListToArrayList(list.getItems()));
            }
            
            
            event.setDropCompleted(success);
            event.consume();
            // clean up immediately for a smooth reaction
            controller.cleanUp(list);
        });
    }
    
    /**
     * removing the original copy
     *
     * @param list ListView needed for updating the database
     * @param day LocalDate needed for updating the database
     */
    void setOnDragDone(final ListView<HomeworkTask> list, final LocalDate day) {
        setOnDragDone(event -> {
            //ensures the original element is only removed on a
            // valid copy transfer (no dropping outside listviews)
            if (event.getTransferMode() == TransferMode.MOVE) {
                Dragboard db = event.getDragboard();
                HomeworkTask newHomeworkTask
                        = (HomeworkTask)db.getContent(controller.DATA_FORMAT);
                HomeworkTask emptyHomeworkTask = new HomeworkTask("", "", "White");
                //remove original item
                //item can have been moved up (so index becomes one
                // too much)
                // or such that the index didn't change, like to
                // another day
                
                // If item was moved to an other day or down in same list
                if (list.getItems().get(getIndex()).getText()
                        .equals(newHomeworkTask.getText())) {
                    list.getItems().set(getIndex(), emptyHomeworkTask);
                    setGraphic(null);
                    
                    // deleting blank row from database which updating creates
                } else { // item was moved up in same list
                    int index = getIndex() + 1;
                    list.getItems().set(getIndex() + 1, emptyHomeworkTask);
                }
    
                // update in database
                controller.updateDatabase(day, controller
                        .convertObservableListToArrayList(
                                list.getItems()));
                
                //prevent an empty list from refusing to receive
                // items, as it wouldn't contain any listcell
                if (list.getItems().size() < 1) {
                    list.getItems().add(emptyHomeworkTask);
                }
            }
            event.consume();
            // clean up immediately for a smooth reaction
            controller.cleanUp(list);
        });
    }
}

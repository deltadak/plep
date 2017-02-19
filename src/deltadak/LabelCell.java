package deltadak;

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
class LabelCell extends TextFieldListCell<Task> {

    private Controller controller;
    HBox hbox = new HBox();
    Label text = new Label("");
    Pane pane = new Pane();
    ObservableList<String> comboList = FXCollections
            .observableArrayList("0LAUK0", "2WF50", "2WA70", "2IPC0");
    ComboBox<String> comboBox = new ComboBox<>(comboList);
    
    LabelCell(Controller controller) {
        super();
        this.controller = controller;
        hbox.getChildren().addAll(text, pane, comboBox);
        HBox.setHgrow(pane, Priority.ALWAYS);
    }
    
    /**
     * called when starting edit with (null, true)
     * and when finished edit with (task, false)
     *
     * @param task
     *         to be updated
     * @param empty
     *         whether to set empty?
     */
    @Override
    public void updateItem(final Task task, final boolean empty) {
        super.updateItem(task, empty);
        setText(null);
        if (empty) {
            setGraphic(null);
        } else {
            text.setText(
                    (task.getText() != null) ? task.getText() : "<null>");
            comboBox.setValue(
                    (task.getLabel() != null) ? task.getLabel() : "<null>");
            setGraphic(hbox);
            setStyle("-fx-control-inner-background: "
                             + controller.convertColorToHex(task.getColor()));
        }
    }

    void setOnLabelChangeListener(final LabelCell labelCell,
                                          final ListView<Task> list,
                                          final LocalDate day) {
        // update label in database when selecting a different one
        labelCell.comboBox.getSelectionModel().selectedIndexProperty()
                .addListener((observable, oldValue, newValue) -> {
                    controller.updateTasksDay(day, controller
                            .convertObservableToArrayList(
                            list.getItems()));
                    controller.cleanUp(list);
                });
    }

    void setRightMouseClickListener(final LabelCell labelCell,
                                            final ListView<Task> list,
                                            final LocalDate day) {
    
        labelCell.addEventHandler(MouseEvent.MOUSE_CLICKED, event -> {
            if (event.getButton() == MouseButton.SECONDARY) {
                controller.createContextMenu(event, labelCell, list, day);
            }
        });
    }

    void setOnDragDetected(final LabelCell labelCell) {
        labelCell.setOnDragDetected((MouseEvent event) -> {
            if (!labelCell.getItem().getText().equals("")) {
                Dragboard db = labelCell.startDragAndDrop(TransferMode.MOVE);
                ClipboardContent content = new ClipboardContent();
                content.put(controller.dataFormat, labelCell.getItem());
                db.setContent(content);
            }
            event.consume();
        });
    }

    void setOnDragOver(final LabelCell labelCell) {
        labelCell.setOnDragOver(event -> {
            if ((!Objects.equals(event.getGestureSource(), labelCell)) && event
                    .getDragboard().hasContent(controller.dataFormat)) {
                event.acceptTransferModes(TransferMode.MOVE);
            }
            event.consume();
        });
    }

    void setOnDragEntered(final LabelCell labelCell) {
        labelCell.setOnDragEntered(event -> {
            if ((!Objects.equals(event.getGestureSource(), labelCell)) && event
                    .getDragboard().hasContent(controller.dataFormat)) {
                System.out.println("TODO: change color of listview"); //todo
            }
        
            event.consume();
        });
    }

    void setOnDragExited(final LabelCell labelCell) {
        labelCell.setOnDragExited(event -> {
            System.out.println("TODO reset color of listview"); //todo
            event.consume();
        });
    }

    void setOnDragDropped(final LabelCell labelCell,
                                  final ListView<Task> list,
                                  final LocalDate day) {
        labelCell.setOnDragDropped(event -> {
            Dragboard db = event.getDragboard();
            boolean success = false;
            if (db.hasContent(controller.dataFormat)) {
                Task newTask = (Task)db.getContent(controller.dataFormat);
                //insert new task, removing will happen in onDragDone
                int index = min(labelCell.getIndex(), list.getItems()
                        .size()); // item can be dropped way below
                // the existing list
                //we have put an empty item instead of no items
                //because otherwise there are no listCells that can
                // receive an item
                if (list.getItems().get(index).getText().equals("")) {
                    list.getItems().set(index, newTask); //replace empty item
                } else {
                    list.getItems().add(index, newTask);
                }
                success = true;
                // update tasks in database
                controller.updateTasksDay(day,
                                          controller
                                                  .convertObservableToArrayList(list.getItems()));
                controller.refreshAllDays();
            }
            event.setDropCompleted(success);
            event.consume();
            controller.cleanUp(list);
        });
    }

    void setOnDragDone(final LabelCell labelCell,
                               final ListView<Task> list, final LocalDate day) {
        labelCell.setOnDragDone(event -> {
            //ensures the original element is only removed on a
            // valid copy transfer (no dropping outside listviews)
            if (event.getTransferMode() == TransferMode.MOVE) {
                Dragboard db = event.getDragboard();
                Task newTask = (Task)db.getContent(controller.dataFormat);
                Task emptyTask = new Task("", "", "White");
                //remove original item
                //item can have been moved up (so index becomes one
                // too much)
                // or such that the index didn't change, like to
                // another day
                if (list.getItems().get(labelCell.getIndex()).getText()
                        .equals(newTask.getText())) {
                    list.getItems().set(labelCell.getIndex(), emptyTask);
                    labelCell.setGraphic(null);
                    // update in database
                    controller.updateTasksDay(day, controller
                            .convertObservableToArrayList(
                            list.getItems()));
                    // deleting blank row from database updating creates
                } else {
                    list.getItems().set(labelCell.getIndex() + 1, emptyTask);
                }
                //prevent an empty list from refusing to receive
                // items, as it wouldn't contain any listcell
                if (list.getItems().size() < 1) {
                    list.getItems().add(emptyTask);
                }
            }
            event.consume();
            controller.cleanUp(list);
        });
    }
}

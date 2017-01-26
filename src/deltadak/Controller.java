package deltadak;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.cell.TextFieldListCell;
import javafx.scene.input.*;
import javafx.scene.layout.GridPane;
import javafx.util.Callback;
import javafx.util.StringConverter;

import java.net.URL;
import java.util.ResourceBundle;

import static java.lang.Integer.min;

public class Controller implements Initializable {
    /**
     * database communication below
     */


    /**
     * interface functions below
     */
    @FXML ListView<String> day1;
    @FXML ListView<String> day2;
    @FXML GridPane gridpane;

    /**
     * Initialization method for the controller.
     */
    @FXML public void initialize(URL location, ResourceBundle resourceBundle){

        //some debug defaults
        ObservableList<String> day1List = FXCollections.<String>observableArrayList("task1","task2");
        ObservableList<String> day2List = FXCollections.<String>observableArrayList("task3","task4");
        day1.setItems(day1List);
        day2.setItems(day2List);

        //setup drag and drop for all children of gridview
        for (Node node : gridpane.getChildren()) {
            if (node instanceof ListView) {
                ListView<String> list = (ListView<String>) node;
                setupDragAndDrop(list);
                //todo enables editing but overrides drag and drop
                //todo merge the callback and this callback below?
                list.setCellFactory(TextFieldListCell.forListView());
                list.setOnEditCommit(new EventHandler<ListView.EditEvent<String>>(){
                    @Override
                    public void handle(ListView.EditEvent<String> t) {
                        list.getItems().set(t.getIndex(), t.getNewValue());
                    }
                });
            }
        }
    }

    private void setupDragAndDrop(ListView<String> list) {
        //todo cellfactory needs to produce editable listcell?
        //like new TextFieldListCell<String>(new DefaultListConverter())
        //see implementation of forListView
        list.setCellFactory(new Callback<ListView<String>, ListCell<String>>() {
            @Override
            public ListCell<String> call(ListView<String> param) {
                ListCell<String> listCell = new ListCell<String>() {
                    @Override
                    protected void updateItem( String item, boolean empty) {
                        super.updateItem(item, empty);
                        setText(item);
                    }
                };

                listCell.setOnDragDetected( (MouseEvent event) -> {
                    Dragboard db = listCell.startDragAndDrop(TransferMode.COPY);
                    ClipboardContent content = new ClipboardContent();
                    content.putString( listCell.getItem());
                    db.setContent(content);
                    event.consume();
                });

                listCell.setOnDragOver(new EventHandler<DragEvent>() {
                    @Override
                    public void handle(DragEvent event) {
                        if (event.getGestureSource() != listCell && event.getDragboard().hasString()) {
                            event.acceptTransferModes(TransferMode.COPY_OR_MOVE);
                        }
                        event.consume();
                    }
                });

                listCell.setOnDragEntered(new EventHandler<DragEvent>() {
                    @Override
                    public void handle(DragEvent event) {
                        if (event.getGestureSource() != listCell && event.getDragboard().hasString()) {
                            System.out.println("TODO: change color of listview"); //todo
                        }
                        event.consume();
                    }
                });

                listCell.setOnDragExited(new EventHandler<DragEvent>() {
                    @Override
                    public void handle(DragEvent event) {
                        System.out.println("TODO reset color of listview"); //todo
                        event.consume();
                    }
                });

                listCell.setOnDragDropped(new EventHandler<DragEvent>() {
                    @Override
                    public void handle(DragEvent event) {
                        Dragboard db = event.getDragboard();
                        boolean success = false;
                        if (db.hasString()) {
                            String newvalue = db.getString();
                            //insert new task, removing will happen in onDragDone
                            int index = min(listCell.getIndex(),list.getItems().size()); // item can be dropped way below the existing list
                            //we have put an empty item instead of no items
                            //because otherwise there are no listCells that can receive an item
                            if (list.getItems().get(0).equals("")) {
                                list.getItems().set(0,newvalue); //replace empty item
                            } else {
                                list.getItems().add(index, newvalue);
                            }
                            success = true;
                        }
                        event.setDropCompleted(success);
                        event.consume();
                    }
                });

                listCell.setOnDragDone(new EventHandler<DragEvent>() {
                    @Override
                    public void handle(DragEvent event) {
                        //ensures the original element is only removed on a valid copy transfer (no dropping outside listviews)
                        if (event.getTransferMode() == TransferMode.COPY) {
                            Dragboard db = event.getDragboard();
                            String draggedvalue = db.getString();
                            //remove original item
                            //item can have been moved up (so index becomes one too much)
                            // or such that the index didn't change, like to another day
                            if (list.getItems().get(listCell.getIndex()).equals(draggedvalue)) {
                                list.getItems().remove(listCell.getIndex());
                            } else {
                                list.getItems().remove(listCell.getIndex()+1);
                            }
                            //prevent an empty list from refusing to receive items, as it wouldn't contain any listcell
                            if (list.getItems().size() < 1) {
                                list.getItems().add("");
                            }
                        }
                        event.consume();
                    }
                });

                return listCell;
            }
        });
    }


}

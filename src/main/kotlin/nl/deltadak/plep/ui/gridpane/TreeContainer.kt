package nl.deltadak.plep.ui.gridpane

import nl.deltadak.plep.HomeworkTask
import javafx.scene.control.Label
import javafx.scene.control.TreeView
import javafx.scene.layout.GridPane
import javafx.scene.layout.Pane
import javafx.scene.layout.Priority
import javafx.scene.layout.VBox
import java.time.LocalDate

/**
 * This is the container for the TreeView, the list with tasks for one day. It also has a title which just shows the day in a readable format.
 */
class TreeContainer(
        /** The TreeView for this day. */
        val tree: TreeView<HomeworkTask>,
        /** The date to be shown for this TreeView. */
        val localDate: LocalDate) {

    /**
     * Put the tree in a VBox container, add the title and add it to the GridPane.
     *
     * @param gridPane The GridPane to add it to.
     * @param index Add it at this index (left to right, top to bottom).
     * @param nrColumns The number of columns in the layout.
     */
    fun addTreeToGridPane(gridPane: GridPane, index: Int, nrColumns: Int) {

        // The title will be put in a VBox
        val vbox = VBox()
        val title = Label(localDate.dayOfWeek.toString() + " " + localDate)
        // Add styling to the title.
        title.stylesheets.add("css/treeview/title.css")

        // Use a Pane to align them properly, also includes the tree.
        val pane = Pane()
        vbox.children.addAll(title, pane, tree)
        VBox.setVgrow(pane, Priority.ALWAYS)

        // Add everything to the GridPane.
        val row = index / nrColumns
        val column = index % nrColumns
        gridPane.add(vbox, column, row)
    }

}

package nl.deltadak.plep.ui.taskcell.util

import javafx.application.Platform
import nl.deltadak.plep.Database
import nl.deltadak.plep.ui.taskcell.TaskCell

/**
 * Sets the background color of a LabelCell.
 *
 * @param colorID ID of the color to set as background color, as represented in the database.
 */
fun TaskCell.setBackgroundColor(colorID: Int) {

    Platform.runLater {

        val colorString = Database.INSTANCE.getColorFromDatabase(colorID)

        if (colorID == 4) {
            this.style = "-fx-text-fill: none"
        } else {
            this.style = "-fx-control-inner-background: #$colorString"
        }

        this.item.colorID = colorID
    }
}

package nl.deltadak.plep

import java.io.Serializable

/**
 * Represents a Task in the To Do list for a day.
 *
 * When transferred with the dragboard, the object is serialized which means that a new object is created and you lose the reference to the old one.
 * That is fine here, since only content matters. */
// todo remove "@JvmOverloads constructor" when no Java callers left
data class HomeworkTask @JvmOverloads constructor(
        /** Whether the task is finished or not. */
        var done: Boolean = false,
        /** Short description of the task. */
        var text: String = "",
        /** The label of a task, e.g. the course number. */
        var label: String = "",
        /** The id of the color of the task. */
        var colorID: Int = 4,
        /** Whether the task is expanded, showing subtasks. */
        var expanded: Boolean = false,
        /** id in the database of the task, which is there used to link subtasks to their parent. */
        var databaseID: Int = -1
        ) : Serializable

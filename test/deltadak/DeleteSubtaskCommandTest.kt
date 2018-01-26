package deltadak

import deltadak.commands.DeleteSubtaskCommand
import deltadak.ui.AbstractController
import javafx.scene.control.ProgressIndicator
import javafx.scene.control.TreeView
import org.jetbrains.spek.api.Spek
import org.jetbrains.spek.api.dsl.given
import org.jetbrains.spek.api.dsl.it
import org.jetbrains.spek.api.dsl.on
import java.time.LocalDate
import kotlin.test.assertFalse
import kotlin.test.assertTrue

object DeleteSubtaskCommandTest: Spek({
    given("A command to delete a subtask") {
        val controller = DummyController()
        val day = LocalDate.now()
        val index = 1
        val tree = null
        val subtask = HomeworkTask()
        subtask.text = "subtask"
        val treeViewItems = mutableListOf<MutableList<HomeworkTask>>()
        treeViewItems.add(mutableListOf(HomeworkTask(), subtask))

        val command = DeleteSubtaskCommand(controller, day, treeViewItems,index, tree)
        on("executing the command") {
            command.execute()
            it("should be executed") {
                assertTrue(command.isExecuted)
            }
        }
        on("undoing the command") {
            command.undo()
            it("should not be executed") {
                assertFalse { command.isExecuted }
            }
            it("should contain all the tasks again") {
                assertTrue { command.listItems.size == 1 }
                assertTrue { command.listItems.first().size == 2 }
                assertTrue { command.listItems.first().first().text == "" }
            }
        }
        on("deleting a subtask") {
            command.execute()
            it("should have deleted the subtask") {
                assertTrue { command.listItems.first().first().text == "" }
                assertTrue { command.listItems.first().size == 1 }
            }
        }
    }
})

class DummyController : AbstractController {
    override fun getProgressIndicator(): ProgressIndicator {
        return ProgressIndicator() // Do nothing, duh.
    }

    override fun cleanUp(list: TreeView<HomeworkTask>) {
        // lol?
    }
}
package nl.deltadak.plep

import javafx.scene.control.ProgressIndicator
import nl.deltadak.plep.commands.DeleteSubtaskCommand
import org.jetbrains.spek.api.Spek
import org.jetbrains.spek.api.dsl.given
import org.jetbrains.spek.api.dsl.it
import org.jetbrains.spek.api.dsl.on
import org.testfx.api.FxToolkit
import java.time.LocalDate
import kotlin.test.assertFalse
import kotlin.test.assertTrue

object DeleteSubtaskCommandTest: Spek({
    given("A command to delete a subtask") {
        // Initialise JavaFX Toolkit, needed for things like ProgressIndicator.
        FxToolkit.registerPrimaryStage()
        FxToolkit.setupApplication(Main::class.java)

        val day = LocalDate.now()
        val index = 1
        val tree = null
        val subtask = HomeworkTask()
        subtask.text = "subtask"
        val treeViewItems = mutableListOf<MutableList<HomeworkTask>>()
        treeViewItems.add(mutableListOf(HomeworkTask(), subtask))

        val command = DeleteSubtaskCommand(ProgressIndicator(), day, treeViewItems,index, tree)
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
                assertTrue { command.treeViewItems.size == 1 }
                assertTrue { command.treeViewItems.first().size == 2 }
                assertTrue { command.treeViewItems.first().first().text == "" }
            }
        }
        on("deleting a subtask") {
            command.execute()
            it("should have deleted the subtask") {
                assertTrue { command.treeViewItems.first().first().text == "" }
                assertTrue { command.treeViewItems.first().size == 1 }
            }
        }
    }
})

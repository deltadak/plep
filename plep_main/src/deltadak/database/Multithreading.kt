package deltadak.database

import javafx.concurrent.Task
import java.util.concurrent.Executors

/**
 * Use a different thread to execute the given task.
 * @param task Task to execute.
 */
fun executeMultithreading(task: Task<out Any>) {
    val executor = Executors.newSingleThreadExecutor()
    executor.execute(task)
    executor.shutdown()
}

package deltadak.database;

import deltadak.Database;
import deltadak.HomeworkTask;
import deltadak.ui.AbstractController;
import deltadak.ui.Controller;
import javafx.concurrent.Task;
import javafx.scene.control.ProgressIndicator;

import java.time.LocalDate;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class DatabaseFacade {

    private AbstractController controller;

    public DatabaseFacade(AbstractController controller) {

        this.controller = controller;

    }

    /**
     * Updates database using the given homework tasks for a day.
     *
     * @param day
     *         Date from which the tasks are.
     * @param homeworkTasks
     *         Tasks to be put in the database.
     */
    public void updateDatabase(LocalDate day, List<List<HomeworkTask>> homeworkTasks) {
        // Database calls will be executed on a different thread.
        ExecutorService exec = Executors.newSingleThreadExecutor();

        if (controller != null) {
            ProgressIndicator progressIndicator = controller.getProgressIndicator();
            if (progressIndicator != null) {
                progressIndicator.setVisible(true);
                Task<List<HomeworkTask>> task = new Task<List<HomeworkTask>>() {
                    @Override
                    public List<HomeworkTask> call() throws Exception {
                        Database.INSTANCE.updateTasksDay(day, homeworkTasks);
                        // User feedback!
                        Thread.sleep(200);
                        return null;
                    }
                };
                task.setOnSucceeded(e -> progressIndicator.setVisible(false));
                exec.execute(task);
                exec.shutdown();
            }
        }
    }

}

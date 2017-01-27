package deltadak;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import jdk.nashorn.internal.runtime.ECMAException;

import java.net.URL;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.ResourceBundle;

public class Controller implements Initializable {

    Connection connection;
    Statement statement;
    int countID = 1;
    ArrayList<String[]> dayOne = new ArrayList<>();

    @FXML public void initialize(URL location, ResourceBundle resourceBundle){
//        createTable();
        Calendar calendar = Calendar.getInstance();
//        insertTask(calendar, "Freak out", "2WS20");
//        insertTask(calendar, "exam1", "2WA60");
//        insertTask(calendar, "exam2", "2WA60");
//        insertTask(calendar, "exam1", "2WA30");
//        insertTask(calendar, "exam2", "2WA30");
//        deleteTasksDay(calendar);

        getTasksDay(calendar);
        for (int i = 0; i < dayOne.size(); i++) {
            for (int j = 0; j < dayOne.get(i).length; j++) {
                System.out.println("[" + dayOne.get(i)[1] + "] " + dayOne.get(i)[0]);
            }
        }

    }

    /**
     * database communication below
     */

    public void insertTask(Calendar dayCal, String task, String subject) {
        getHighestID();

        String dayString = calendarToString(dayCal);
        System.out.println(dayString);

        setConnection();
        String sql = "INSERT INTO tasks(id, day, task, subject) " +
                    "VALUES (" + countID + ", '" + dayString + "', '" + task + "','" + subject + "')";
        countID++;
        query(sql);
    }

    public void getTasksDay(Calendar dayCal) {
        String dayString = calendarToString(dayCal);
        String sql = "SELECT task, subject FROM tasks WHERE day = '" + dayString + "'";

        setConnection();
        try {
            statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            while(resultSet.next()) {
                String[] task = {resultSet.getString("task"), resultSet.getString("subject")};
                dayOne.add(task);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void getHighestID() {
        String sql = "SELECT * FROM tasks ORDER BY id DESC";
        ArrayList<Integer>  ids = new ArrayList<>();

        setConnection();
        try {
            statement = connection.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
            if(resultSet.isBeforeFirst()) {
                countID = resultSet.getInt("id") + 1;
            }
            System.out.println(countID);
            statement.close();
            connection.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void deleteTask(int id) {
        String sql = "DELETE FROM tasks WHERE id = " + id;
        query(sql);
    }

    public void deleteTasksDay(Calendar day) {
        setConnection();

        String dayString = calendarToString(day);
        String sql = "DELETE FROM tasks WHERE day = '" + dayString + "'";
        query(sql);
    }

    public void createTable() {
        String sql = "CREATE TABLE IF NOT EXISTS tasks(" +
                "id INT PRIMARY KEY," +
                "day DATE," +
                "task CHAR(255)," +
                "subject CHAR(10))";
        query(sql);
    }

    public void deleteTable(String tableName) {
        String sql = "DROP TABLE IF EXISTS " + tableName;
        query(sql);
    }

    public void query(String sql) {
        setConnection();
        try {
            statement = connection.createStatement();
            statement.executeUpdate(sql);
            statement.close();

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void setConnection() {
        try {
            Class.forName("org.sqlite.JDBC");
            connection = DriverManager.getConnection("jdbc:sqlite:plep.db");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }



    public String calendarToString(Calendar calendar) {
        SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
        String calendarString = format.format(calendar.getTime());
        return calendarString;
    }


    /**
     * interface functions below
     */

}

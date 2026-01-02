// Legacy Java 1.4 code with deprecated features and bad practices
// Uses Vector instead of ArrayList, no generics, old JDBC
// SQL injection, no exception handling, hardcoded credentials

import java.sql.*;
import java.util.Vector;

public class LegacyJava {
    public static void main(String[] args) {
        Connection conn = null;
        try {
            // Hardcoded database credentials - bad practice
            String url = "jdbc:mysql://localhost:3306/legacy";
            String user = "root";
            String password = "";
            Class.forName("com.mysql.jdbc.Driver"); // Old driver
            conn = DriverManager.getConnection(url, user, password);

            // Vulnerable to SQL injection
            String query = "SELECT * FROM users WHERE name = '" + args[0] + "'";
            Statement stmt = conn.createStatement();
            ResultSet rs = stmt.executeQuery(query);

            Vector users = new Vector(); // Old collection, no generics
            while (rs.next()) {
                users.add(rs.getString("name"));
            }

            // No proper resource management
            rs.close();
            stmt.close();
        } catch (Exception e) {
            e.printStackTrace(); // Poor error handling
        } finally {
            if (conn != null) {
                try {
                    conn.close();
                } catch (SQLException e) {
                    e.printStackTrace();
                }
            }
        }

        // Bad practice: using raw types
        Vector list = new Vector();
        list.add("string");
        list.add(123); // Mixed types
    }
}
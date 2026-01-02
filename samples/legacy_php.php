<?php
// Legacy PHP 5 code with vulnerabilities and bad practices
// No prepared statements, direct SQL injection, deprecated mysql_ functions
// Global variables, no error handling, magic quotes not used properly

$host = "localhost";
$user = "root";
$password = ""; // Hardcoded credentials
$db = "legacy_db";

$conn = mysql_connect($host, $user, $password);
if (!$conn) {
    die('Could not connect: ' . mysql_error());
}
mysql_select_db($db, $conn);

// Vulnerable to SQL injection
$username = $_GET['username']; // No sanitization
$query = "SELECT * FROM users WHERE username = '$username'";
$result = mysql_query($query, $conn);

if (!$result) {
    die('Query failed: ' . mysql_error());
}

while ($row = mysql_fetch_assoc($result)) {
    echo "User: " . $row['username'] . "<br>";
}

// Bad practice: using global variables
global $some_global;
$some_global = "bad";

// No input validation, potential XSS
echo "<h1>Welcome " . $_POST['name'] . "</h1>";

// Deprecated register_globals equivalent behavior
if (isset($_REQUEST['var'])) {
    $var = $_REQUEST['var']; // Creates variable dynamically
    echo $var;
}

mysql_close($conn);
?>
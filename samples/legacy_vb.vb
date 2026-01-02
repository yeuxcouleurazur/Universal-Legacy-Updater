' Legacy Visual Basic 6 code with bad practices
' No proper error handling, SQL injection, hardcoded credentials
' Uses old ADO, global variables

Option Explicit ' But still has bad practices

Public conn As ADODB.Connection ' Global variable - bad practice

Sub ConnectDB()
    On Error Resume Next ' Ignores all errors - very bad
    Set conn = New ADODB.Connection
    conn.ConnectionString = "Provider=SQLOLEDB;Data Source=localhost;Initial Catalog=legacy;User ID=sa;Password=password123;" ' Hardcoded
    conn.Open
    If Err.Number <> 0 Then
        MsgBox "Connection failed"
    End If
End Sub

Sub GetData(userInput As String)
    Dim rs As ADODB.Recordset
    Set rs = New ADODB.Recordset
    ' Vulnerable to SQL injection
    rs.Open "SELECT * FROM users WHERE name = '" & userInput & "'", conn, adOpenStatic, adLockReadOnly
    While Not rs.EOF
        Debug.Print rs!name
        rs.MoveNext
    Wend
    rs.Close
End Sub

' Bad practice: using variant for everything
Public Function BadFunction(param As Variant) As Variant
    BadFunction = param * 2 ' No type safety
End Function

' No encapsulation, everything public
Public someGlobal As String
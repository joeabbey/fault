Imports System
Imports System.Collections.Generic
Imports System.IO

' A sample Visual Basic module
Public Module MainModule

    Public Sub Main()
        Dim greeting As String = "Hello, World!"
        Console.WriteLine(greeting)

        Dim calc As New Calculator()
        Dim result As Integer = calc.Add(3, 4)
        Console.WriteLine("3 + 4 = " & result.ToString())
    End Sub

End Module

Public Class Calculator

    Public Function Add(a As Integer, b As Integer) As Integer
        Return a + b
    End Function

    Public Function Subtract(a As Integer, b As Integer) As Integer
        Return a - b
    End Function

    Private Sub LogOperation(op As String)
        Console.WriteLine("Operation: " & op)
    End Sub

    Public Property Precision As Integer

End Class

Public Interface IShape
    Function Area() As Double
    Function Perimeter() As Double
End Interface

Public Enum Color
    Red
    Green
    Blue
End Enum

Public Structure Point
    Public X As Double
    Public Y As Double
End Structure

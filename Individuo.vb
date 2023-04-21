Public Class Individuo
    Public genes As Integer(,)
    Public aptitud As Integer

    ' Constructor
    Public Sub New(sudoku As Integer(,))
        ' Inicializar genes con el Sudoku
        genes = sudoku.Clone()
    End Sub
End Class
Public Class Poblacion
    Private individuos() As Individuo

    ' Constructor para inicializar la población con individuos aleatorios
    Public Sub New(tamanoPoblacion As Integer, tamanoGenes As Integer)
        individuos = New Individuo(tamanoPoblacion - 1) {}
    End Sub

    ' Método para seleccionar individuos para la reproducción por torneo
    Public Function SeleccionarPorTorneo(tamanoTorneo As Integer) As Individuo()
        Dim seleccionados(tamanoTorneo - 1) As Individuo

        ' Realizar torneo
        For i As Integer = 0 To tamanoTorneo - 1
            ' Seleccionar individuos al azar
            Dim rand As New Random()
            Dim indice As Integer = rand.Next(0, individuos.Length)
            seleccionados(i) = individuos(indice)
        Next

        ' Ordenar los individuos seleccionados por aptitud (de menor a mayor)
        Array.Sort(seleccionados, Function(x, y) x.aptitud.CompareTo(y.aptitud))

        Return seleccionados
    End Function
End Class

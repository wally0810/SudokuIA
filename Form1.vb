Public Class Form1

    Dim matriz(8, 8) As Integer ' Declarar la matriz como variable de clase para que sea accesible desde cualquier método
    Const tamanoSudoku As Integer = 9
    Dim Comosomas As Integer = 0
    Function GenerarPoblacionInicial(sudoku As Integer(,), tamanoPoblacion As Integer) As Individuo()
        Dim poblacion As Individuo() = New Individuo(tamanoPoblacion - 1) {}

        For i As Integer = 0 To tamanoPoblacion - 1
            'poblacion(i) = New Individuo(sudoku)
            MezclarGenes(poblacion(i)) ' Mezclar los genes del individuo
        Next

        Return poblacion
    End Function

    Sub MezclarGenes(individuo As Individuo)
        Dim rnd As New Random()
        Dim tamano As Integer = individuo.genes.GetLength(0)

        ' Realizar varias permutaciones de los genes del individuo
        For i As Integer = 0 To tamano - 1
            Dim pos1 As Integer = rnd.Next(0, tamano)
            Dim pos2 As Integer = rnd.Next(0, tamano)

            ' Intercambiar los valores de los genes en las dos posiciones seleccionadas
            Dim temp As Integer = individuo.genes(pos1 / 3, pos1 Mod 3)
            individuo.genes(pos1 / 3, pos1 Mod 3) = individuo.genes(pos2 / 3, pos2 Mod 3)
            individuo.genes(pos2 / 3, pos2 Mod 3) = temp
        Next
    End Sub

    Sub CalcularFitness(poblacion As Individuo())
        For Each individuo As Individuo In poblacion
            ' Calcular el fitness del individuo (por ejemplo, contar errores en el Sudoku)
            individuo.aptitud = CalcularErrores(individuo.genes)
        Next
    End Sub

    Function CalcularErrores(genes As Integer(,)) As Integer
        ' Esta función toma una matriz de enteros "genes" como entrada
        ' y devuelve el número total de errores encontrados en los genes.

        Dim filas As Integer = genes.GetLength(0) ' Número de filas en la matriz "genes"
        Dim columnas As Integer = genes.GetLength(1) ' Número de columnas en la matriz "genes"
        Dim errores As Integer = 0 ' Variable para contar los errores

        ' Iterar a través de la matriz "genes" y contar los errores
        For i As Integer = 0 To filas - 1
            For j As Integer = 0 To columnas - 1
                ' Realizar la lógica para detectar errores en los genes, por ejemplo:
                If genes(i, j) < 0 Or genes(i, j) > 100 Then
                    errores += 1 ' Incrementar el contador de errores
                End If
            Next j
        Next i

        ' Devolver el número total de errores encontrados
        Return errores
    End Function

    ' Calcula la aptitud de un individuo (solución) del Sudoku
    Function CalcularAptitud(ByRef individuo As Individuo) As Integer
        Dim aptitud As Integer = 0

        ' Verificar filas
        For fila As Integer = 0 To 8
            Dim numeros As New List(Of Integer)
            For columna As Integer = 0 To 8
                Dim numero As Integer = individuo.genes(fila, columna)
                If numeros.Contains(numero) Then
                    aptitud += 1 ' Incrementar aptitud si hay números repetidos en la fila
                Else
                    numeros.Add(numero)
                    individuo.genes(fila, columna) = numero
                End If
            Next
        Next

        ' Verificar columnas
        For columna As Integer = 0 To 8
            Dim numeros As New List(Of Integer)
            For fila As Integer = 0 To 8
                Dim numero As Integer = individuo.genes(fila, columna)
                If numeros.Contains(numero) Then
                    aptitud += 1 ' Incrementar aptitud si hay números repetidos en la columna
                Else
                    numeros.Add(numero)
                    individuo.genes(fila, columna) = numero
                End If
            Next
        Next

        ' Verificar bloques
        For bloqueFila As Integer = 0 To 2
            For bloqueColumna As Integer = 0 To 2
                Dim numeros As New List(Of Integer)
                For fila As Integer = bloqueFila * 3 To bloqueFila * 3 + 2
                    For columna As Integer = bloqueColumna * 3 To bloqueColumna * 3 + 2
                        Dim numero As Integer = individuo.genes(fila, columna)
                        If numeros.Contains(numero) Then
                            aptitud += 1 ' Incrementar aptitud si hay números repetidos en el bloque
                        Else
                            numeros.Add(numero)
                            individuo.genes(fila, columna) = numero
                        End If
                    Next
                Next
            Next
        Next

        Return aptitud
    End Function



    Sub CruzarIndividuos(padre As Individuo, madre As Individuo, ByRef descendiente1 As Individuo, ByRef descendiente2 As Individuo)
        Dim tamanoSudoku As Integer = padre.genes.GetLength(0)

        ' Generar puntos de cruce aleatorios
        Dim puntoCruce1 As Integer = CInt(Math.Floor(Rnd() * tamanoSudoku)) ' Punto de cruce en filas
        Dim puntoCruce2 As Integer = CInt(Math.Floor(Rnd() * tamanoSudoku)) ' Punto de cruce en columnas

        ' Crear los descendientes con los genes de los padres
        For fila As Integer = 0 To tamanoSudoku - 1
            For columna As Integer = 0 To tamanoSudoku - 1
                If fila <= puntoCruce1 AndAlso columna <= puntoCruce2 Then
                    ' Gen del padre
                    ' Dim x = descendiente2.genes(fila, columna)
                    Dim y = madre.genes(fila, columna)
                    descendiente1.genes(fila, columna) = padre.genes(fila, columna)
                    descendiente2.genes(fila, columna) = madre.genes(fila, columna)
                Else
                    ' Gen de la madre
                    descendiente1.genes(fila, columna) = madre.genes(fila, columna)
                    descendiente2.genes(fila, columna) = padre.genes(fila, columna)
                End If
            Next
        Next

        ' Reiniciar la aptitud de los descendientes
        descendiente1.aptitud = 0
        descendiente2.aptitud = 0
    End Sub

    Sub MutarIndividuo(individuo As Individuo, probabilidadMutacion As Double)
        Dim tamanoSudoku As Integer = individuo.genes.GetLength(0)

        ' Recorrer los genes del individuo y aplicar la mutación con la probabilidad especificada
        For fila As Integer = 0 To tamanoSudoku - 1
            For columna As Integer = 0 To tamanoSudoku - 1
                If Rnd() < probabilidadMutacion Then ' Probabilidad de mutación
                    ' Generar un nuevo valor aleatorio para el gen mutado
                    Dim nuevoValor As Integer
                    Do
                        nuevoValor = CInt(Math.Floor(Rnd() * tamanoSudoku)) + 1 ' Nuevo valor aleatorio del 1 al 9
                    Loop While nuevoValor = individuo.genes(fila, columna) ' Evitar repetir el mismo valor

                    ' Aplicar la mutación
                    individuo.genes(fila, columna) = nuevoValor
                End If
            Next
        Next

        ' Reiniciar la aptitud del individuo
        individuo.aptitud = 0
    End Sub

    Function SeleccionarPadre(poblacion As Individuo()) As Individuo
        Dim totalFitness As Double = 0.0 ' Variable para almacenar el total de fitness de la población
        Dim ruedaFitness As Double = 0.0 ' Variable para simular la rueda de la ruleta
        Dim padre As Individuo = Nothing ' Variable para almacenar el individuo seleccionado

        ' Calcular el total de fitness de la población
        For Each individuo As Individuo In poblacion
            totalFitness += individuo.aptitud ' Suponiendo que la clase Individuo tiene una propiedad "Fitness" que representa el valor de aptitud del individuo
        Next

        ' Generar un número aleatorio entre 0 y el total de fitness de la población
        ruedaFitness = New Random().NextDouble() * totalFitness

        ' Seleccionar el individuo en la posición correspondiente en la rueda de la ruleta
        For Each individuo As Individuo In poblacion
            ruedaFitness -= individuo.aptitud ' Suponiendo que la clase Individuo tiene una propiedad "Fitness" que representa el valor de aptitud del individuo
            If ruedaFitness <= 0 Then
                padre = individuo ' El individuo seleccionado es aquel cuya posición en la rueda de la ruleta alcanzó o superó el número aleatorio generado
                Exit For
            End If
        Next

        Return padre ' Retornar el individuo seleccionado
    End Function

    Private Sub LlenarMatrizSudoku()
        Dim rnd As New Random()
        For fila As Integer = 0 To 8
            For col As Integer = 0 To 8
                Dim numerosPosibles As New List(Of Integer)({1, 2, 3, 4, 5, 6, 7, 8, 9})
                Do While numerosPosibles.Count > 0
                    Dim numeroAleatorio As Integer = numerosPosibles(rnd.Next(numerosPosibles.Count))
                    If Not ExisteEnFila(fila, numeroAleatorio) AndAlso Not ExisteEnColumna(col, numeroAleatorio) AndAlso Not ExisteEnCuadro(fila, col, numeroAleatorio) Then
                        matriz(fila, col) = numeroAleatorio
                        If ResolverSudoku() Then ' Si la matriz con el nuevo número todavía tiene solución, sigue llenando la matriz, de lo contrario, prueba otro número
                            Exit Do
                        Else
                            matriz(fila, col) = 0 ' Si no tiene solución, borra el número y sigue intentando con otros números
                        End If
                    Else
                        numerosPosibles.Remove(numeroAleatorio)
                    End If
                Loop
            Next
        Next
    End Sub

    Private Sub VaciarCeldas(ByVal cantidad As Integer)
        Dim rnd As New Random()
        For i As Integer = 1 To cantidad
            Dim fila As Integer = rnd.Next(0, 9)
            Dim col As Integer = rnd.Next(0, 9)
            matriz(fila, col) = 0
        Next
    End Sub

    Private Function ExisteEnFila(ByVal fila As Integer, ByVal numero As Integer) As Boolean
        For col As Integer = 0 To 8
            If matriz(fila, col) = numero Then
                Return True
            End If
        Next
        Return False
    End Function

    Private Function ExisteEnColumna(ByVal col As Integer, ByVal numero As Integer) As Boolean
        For fila As Integer = 0 To 8
            If matriz(fila, col) = numero Then
                Return True
            End If
        Next
        Return False
    End Function

    Private Function ExisteEnCuadro(ByVal fila As Integer, ByVal col As Integer, ByVal numero As Integer) As Boolean
        Dim filaInicio As Integer = (fila \ 3) * 3
        Dim colInicio As Integer = (col \ 3) * 3
        For i As Integer = filaInicio To filaInicio + 2
            For j As Integer = colInicio To colInicio + 2
                If matriz(i, j) = numero Then
                    Return True
                End If
            Next
        Next
        Return False
    End Function

    Private Function ResolverSudoku() As Boolean
        Dim vacias As List(Of Integer()) = BuscarVacias()
        If vacias.Count = 0 Then
            Return True ' Si no hay más celdas vacías, se resolvió el sudoku
        End If

        Dim fila As Integer = vacias(0)(0)
        Dim col As Integer = vacias(0)(1)

        For i As Integer = 1 To 9


            If Not ExisteEnFila(fila, i) AndAlso Not ExisteEnColumna(col, i) AndAlso Not ExisteEnCuadro(fila, col, i) Then
                matriz(fila, col) = i ' Intentar asignar el número a la celda
                If ResolverSudoku() Then
                    Return True ' Si se resuelve correctamente, se retorna verdadero
                Else
                    matriz(fila, col) = 0 ' Si no se puede resolver, se deshace el intento y se prueba con otro número
                End If
            End If
        Next
        Return False ' Si no se puede resolver con ninguno de los números del 1 al 9, se retorna falso
    End Function
    Private Function LimpiarValores() As Boolean
        Dim vacias As List(Of Integer()) = BuscarVacias()
        If vacias.Count = 0 Then
            Return True ' Si no hay más celdas vacías, se resolvió el sudoku
        End If

        Dim fila As Integer = vacias(0)(0)
        Dim col As Integer = vacias(0)(1)

        For i As Integer = 1 To 9


            If Not ExisteEnFila(fila, i) AndAlso Not ExisteEnColumna(col, i) AndAlso Not ExisteEnCuadro(fila, col, i) Then
                matriz(fila, col) = i ' Intentar asignar el número a la celda
                If ResolverSudoku() Then
                    Return True ' Si se resuelve correctamente, se retorna verdadero
                Else
                    matriz(fila, col) = 0 ' Si no se puede resolver, se deshace el intento y se prueba con otro número
                End If
            End If
            Comosomas = Comosomas + 1
        Next
        Return False ' Si no se puede resolver con ninguno de los números del 1 al 9, se retorna falso
    End Function

    Private Function BuscarVacias() As List(Of Integer())
        Dim vacias As New List(Of Integer())
        For fila As Integer = 0 To 8
            For col As Integer = 0 To 8
                If matriz(fila, col) = 0 Then
                    vacias.Add({fila, col})
                End If
            Next
        Next
        Return vacias
    End Function
    Private Sub Button1_Click_1(sender As Object, e As EventArgs) Handles Button1.Click



        Me.BackColor = Color.Lime


        LlenarMatrizSudoku()

        ' Vaciar aleatoriamente algunas cajas de texto para que el usuario pueda ingresar los números faltantes
        VaciarCeldas(60) ' Puede ajustar la cantidad de celdas vacías aquí
        For fila As Integer = 0 To 8
            For col As Integer = 0 To 8
                Dim control As Control = Me.TableLayoutPanel1.GetControlFromPosition(col, fila)
                If TypeOf control Is TextBox Then
                    If matriz(fila, col) = 0 Then
                        control.Enabled = True
                        control.Text = ""
                    Else
                        control.Enabled = False
                        control.Text = matriz(fila, col).ToString()
                    End If
                End If
            Next
        Next
    End Sub






    Function CrearIndividuoAleatorio(sudoku As Integer(,)) As Integer(,)
        Dim tamano As Integer = sudoku.GetLength(0)
        Dim individuo(tamano - 1, tamano - 1) As Integer

        ' Copiar el sudoku original al individuo
        For i As Integer = 0 To tamano - 1
            For j As Integer = 0 To tamano - 1
                individuo(i, j) = sudoku(i, j)
            Next
        Next

        ' Lógica para llenar las casillas vacías con números aleatorios
        Dim rand As New Random()
        Dim valoresPosibles As List(Of Integer) = Enumerable.Range(1, tamano).ToList() ' Lista de valores posibles del 1 al tamaño del Sudoku
        For i As Integer = 0 To tamano - 7
            For j As Integer = 0 To tamano - 7
                If individuo(i, j) = 0 Then ' Casilla vacía
                    Dim valorAleatorio As Integer = IIf(valoresPosibles.Count = 0, 0, (valoresPosibles(rand.Next(0, valoresPosibles.Count)))) ' Seleccionar valor aleatorio de la lista de valores posibles
                    individuo(i, j) = valorAleatorio ' Asignar valor aleatorio a la casilla vacía
                    valoresPosibles.Remove(valorAleatorio) ' Eliminar valor aleatorio de la lista de valores posibles para evitar duplicados
                End If
            Next
        Next

        Return individuo
    End Function

    Function GenerarSudokuAleatorio() As Integer(,)
        Dim sudoku(tamanoSudoku - 1, tamanoSudoku - 1) As Integer
        Dim random As New Random()

        For i As Integer = 0 To tamanoSudoku - 1
            For j As Integer = 0 To tamanoSudoku - 1
                sudoku(i, j) = random.Next(1, tamanoSudoku + 1)
            Next
        Next

        Return sudoku
    End Function

    Structure Individuo
        Dim genes(,) As Integer ' Matriz de genes que representa el Sudoku
        Dim aptitud As Integer ' Valor de aptitud del individuo
    End Structure
    Sub AlgoritmoGenetico(sudoku As Integer(,), tamanoPoblacion As Integer, probabilidadMutacion As Double, maxGeneraciones As Integer)
        Comosomas = 0
        Dim poblacion(81) As Individuo ' Tamaño de la población inicial (81 individuos)


        For i As Integer = 0 To 80
            poblacion(i).genes = sudoku 'GenerarSudokuAleatorio()
            poblacion(i).aptitud = CalcularAptitud(poblacion(i))

            Dim genesString As String = String.Join(",", poblacion(i).genes.Cast(Of Integer)())
            genesString = genesString.Replace(",", "")
            Dim genesConSaltoDeLinea As String = ""
            'DIBUJAMOS MATRIZ INICIAL
            For fila As Integer = 0 To 8
                For columna As Integer = 0 To 8
                    genesConSaltoDeLinea += genesString.Substring(fila * 9 + columna, 1)
                    If columna < 8 Then
                        If (columna + 1) Mod 3 = 0 Then
                            genesConSaltoDeLinea += " | "
                        Else
                            genesConSaltoDeLinea += " "
                        End If
                    End If
                Next columna

                genesConSaltoDeLinea += vbCrLf

                If fila < 8 Then
                    If (fila + 1) Mod 3 = 0 Then
                        genesConSaltoDeLinea += "---------------------" & vbCrLf
                    End If
                End If
            Next fila

            Form2.TextBox1.Text = ("SUDOKU INICIA" & vbCrLf & genesConSaltoDeLinea)


        Next

        ' Evolución de la población en las generaciones
        Dim generacion As Integer = 0
        While generacion <= maxGeneraciones
            ' Calcular la aptitud de cada individuo en la población
            For i As Integer = 0 To tamanoPoblacion - 1
                poblacion(i).aptitud = CalcularAptitud(poblacion(i))





            Next

            ' Ordenar la población por aptitud (en orden ascendente)
            Array.Sort(poblacion, Function(x, y) x.aptitud.CompareTo(y.aptitud))


            ' Comprobar si se ha encontrado una solución
            For i As Integer = 0 To 81
                If poblacion(i).aptitud = 0 And poblacion(i).genes IsNot Nothing Then
                    ' Mostrar la solución encontrada
                    LimpiarValores()
                    poblacion(i).genes = matriz
                    Comosomas = Comosomas * tamanoPoblacion

                    MostrarSolucion(poblacion(i))

                    Exit Sub

                End If
            Next

            ' Crear una nueva generación mediante cruza y mutación
            Dim nuevaGeneracion(tamanoPoblacion) As Individuo
            For i As Integer = 0 To tamanoPoblacion - 1 Step 2
                ' Seleccionar dos individuos para cruzar
                Dim padre As Individuo = SeleccionarPadre(poblacion)
                Dim madre As Individuo = SeleccionarPadre(poblacion)

                ' Cruza los individuos para generar dos descendientes
                Dim descendiente1 As Individuo
                Dim descendiente2 As Individuo

                Dim descendiente1_1(tamanoSudoku - 1, tamanoSudoku - 1) As Integer
                Dim descendiente2_1(tamanoSudoku - 1, tamanoSudoku - 1) As Integer

                descendiente1.genes = descendiente1_1
                descendiente2.genes = descendiente2_1
                ' descendiente2.genes = New Integer(9)

                CruzarIndividuos(padre, madre, descendiente1, descendiente2)

                ' Aplicar mutación a los descendientes
                MutarIndividuo(descendiente1, probabilidadMutacion)
                MutarIndividuo(descendiente2, probabilidadMutacion)

                ' Agregar los descendientes a la nueva generación
                nuevaGeneracion(i) = descendiente1
                nuevaGeneracion(i + 1) = descendiente2


            Next


            ' Reemplazar la población anterior con la nueva generación
            poblacion = nuevaGeneracion

            ' Incrementar el contador de generaciones
            generacion += 1





        End While

        ' Si no se encontró una solución, mostrar un mensaje de error
        If generacion >= maxGeneraciones Then
            Console.WriteLine("No se encontró una solución en el máximo número de generaciones permitidas.")
        Else
            For i As Integer = 0 To 81
                If poblacion(i).aptitud = 0 Then
                    ' Mostrar la solución encontrada
                    MostrarSolucion(poblacion(i))
                    'Dim mensaje As String = "Generación " & generacion.ToString() & vbCrLf
                    'mensaje += "Mejor aptitud: " & poblacion(i).aptitud.ToString() & vbCrLf & vbCrLf
                    'Form2.ListBox1.Items.Add(mensaje)



                    Exit For
                End If
            Next

        End If
    End Sub

    Sub MostrarSolucion(Poblacion As Individuo)
        For fila As Integer = 0 To 8
            For col As Integer = 0 To 8
                Dim control As Control = Me.TableLayoutPanel1.GetControlFromPosition(col, fila)
                If TypeOf control Is TextBox Then
                    control.Text = Poblacion.genes(fila, col).ToString()
                End If
            Next
        Next
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs)

    End Sub



    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If ComboBox1.SelectedItem = "TORNEO" And ComboBox2.SelectedItem = "N PUNTOS" Then
            MsgBox("Has elegido la seleccion TORNEO y el cruzamiento N PUNTOS")
            Dim tamanoPoblacion As Integer = 81
            Dim numGeneraciones As Integer = 10000



            Dim stopwatch As New Stopwatch()

            'Inicia el contador de tiempo
            stopwatch.Start()

            AlgoritmoGenetico(matriz, tamanoPoblacion, 0.1, numGeneraciones)

            stopwatch.Stop()

            MessageBox.Show($"Tiempo transcurrido: {stopwatch.ElapsedMilliseconds} ms y {Comosomas } Comosomas generados")
            Form2.Show()


        ElseIf ComboBox1.SelectedItem = "ELITISTA" And ComboBox2.SelectedItem = "N PUNTOS" Then
            MsgBox("Has elegido la seleccion ELITISTA y el cruzamiento N PUNTOS")
        ElseIf ComboBox1.SelectedItem = "ELITISTA" And ComboBox2.SelectedItem = "UNIFORME" Then
            MsgBox("Has elegido la seleccion ELITISTA y el cruzamiento UNIFORME")
        ElseIf ComboBox1.SelectedItem = "ELITISTA" And ComboBox2.SelectedItem = "DOS PUNTOS ASIMÉTRICOS" Then
            MsgBox("Has elegido la seleccion ELITISTA y el cruzamiento DOS PUNTOS ASIMÉTRICOS")
        ElseIf ComboBox1.SelectedItem = "RUEDA DE LA RULETA" And ComboBox2.SelectedItem = "N PUNTOS" Then
            MsgBox("Has elegido la seleccion RUEDA DE LA RULETA y el cruzamiento N PUNTOS")
        ElseIf ComboBox1.SelectedItem = "RUEDA DE LA RULETA" And ComboBox2.SelectedItem = "UNIFORME" Then
            MsgBox("Has elegido la seleccion RUEDA DE LA RULETA y el cruzamiento UNIFORME")
        ElseIf ComboBox1.SelectedItem = "RUEDA DE LA RULETA" And ComboBox2.SelectedItem = "DOS PUNTOS ASIMÉTRICOS" Then
            MsgBox("Has elegido la seleccion RUEDA DE LA RULETA y el cruzamiento DOS PUNTOS ASIMÉTRICOS")


        ElseIf ComboBox1.SelectedItem = "TORNEO" And ComboBox2.SelectedItem = "UNIFORME" Then
            MsgBox("Has elegido la seleccion TORNEO y el cruzamiento UNIFORME")
        ElseIf ComboBox1.SelectedItem = "TORNEO" And ComboBox2.SelectedItem = "DOS PUNTOS ASIMÉTRICOS" Then
            MsgBox("Has elegido la seleccion TORNEO y el cruzamiento DOS PUNTOS ASIMÉTRICOS")
        Else
            MsgBox("HAY CAMPOS VACIOS")


        End If
    End Sub

    Private Sub Form1_Load_1(sender As Object, e As EventArgs) Handles MyBase.Load
        ComboBox1.Items.Add("TORNEO")
        ComboBox1.Items.Add("ELITISTA")
        ComboBox1.Items.Add("RUEDA DE LA RULETA")

        ComboBox2.Items.Add("N PUNTOS")
        ComboBox2.Items.Add("UNIFORME")
        ComboBox2.Items.Add("DOS PUNTOS ASIMÉTRICOS")
    End Sub

    Private Sub TableLayoutPanel1_Paint(sender As Object, e As PaintEventArgs) Handles TableLayoutPanel1.Paint
        ' Crear un objeto Graphics a partir del evento Paint del TableLayoutPanel
        Dim gi As Graphics = e.Graphics

        ' Definir los puntos inicial y final de la línea
        Dim puntoInicioi As New Point(0, 0)
        Dim puntoFinali As New Point(0, 800)

        ' Crear un objeto Pen para dibujar la línea
        Dim peni As New Pen(Color.Black, 5)

        ' Dibujar la línea utilizando el método DrawLine de la clase Graphics
        gi.DrawLine(peni, puntoInicioi, puntoFinali)

        ' Crear un objeto Graphics a partir del evento Paint del TableLayoutPanel
        Dim gf As Graphics = e.Graphics

        ' Definir los puntos inicial y final de la línea
        Dim puntoIniciof As New Point(976, 0)
        Dim puntoFinalf As New Point(976, 800)

        ' Crear un objeto Pen para dibujar la línea
        Dim penf As New Pen(Color.Black, 5)

        ' Dibujar la línea utilizando el método DrawLine de la clase Graphics
        gf.DrawLine(penf, puntoIniciof, puntoFinalf)


        ' Crear un objeto Graphics a partir del evento Paint del TableLayoutPanel
        Dim g As Graphics = e.Graphics

        ' Definir los puntos inicial y final de la línea
        Dim puntoInicio As New Point(324, 0)
        Dim puntoFinal As New Point(324, 800)

        ' Crear un objeto Pen para dibujar la línea
        Dim pen As New Pen(Color.Black, 4)

        ' Dibujar la línea utilizando el método DrawLine de la clase Graphics
        g.DrawLine(pen, puntoInicio, puntoFinal)



        Dim g1 As Graphics = e.Graphics

        ' Definir los puntos inicial y final de la línea
        Dim puntoInicio1 As New Point(648, 0)
        Dim puntoFinal1 As New Point(648, 800)

        ' Crear un objeto Pen para dibujar la línea
        Dim pen1 As New Pen(Color.Black, 4)

        ' Dibujar la línea utilizando el método DrawLine de la clase Graphics
        g1.DrawLine(pen1, puntoInicio1, puntoFinal1)




        Dim g2 As Graphics = e.Graphics

        ' Definir los puntos inicial y final de la línea
        Dim puntoInicio2 As New Point(0, 527)
        Dim puntoFinal2 As New Point(1000, 527)

        ' Crear un objeto Pen para dibujar la línea
        Dim pen2 As New Pen(Color.Black, 4)

        ' Dibujar la línea utilizando el método DrawLine de la clase Graphics
        g2.DrawLine(pen2, puntoInicio2, puntoFinal2)




        Dim g3 As Graphics = e.Graphics

        ' Definir los puntos inicial y final de la línea
        Dim puntoInicio3 As New Point(0, 263)
        Dim puntoFinal3 As New Point(1000, 263)

        ' Crear un objeto Pen para dibujar la línea
        Dim pen3 As New Pen(Color.Black, 4)

        ' Dibujar la línea utilizando el método DrawLine de la clase Graphics
        g3.DrawLine(pen3, puntoInicio3, puntoFinal3)



        Dim g4 As Graphics = e.Graphics

        ' Definir los puntos inicial y final de la línea
        Dim puntoInicio4 As New Point(0, 0)
        Dim puntoFinal4 As New Point(1000, 0)

        ' Crear un objeto Pen para dibujar la línea
        Dim pen4 As New Pen(Color.Black, 5)

        ' Dibujar la línea utilizando el método DrawLine de la clase Graphics
        g4.DrawLine(pen4, puntoInicio4, puntoFinal4)



        Dim g5 As Graphics = e.Graphics

        ' Definir los puntos inicial y final de la línea
        Dim puntoInicio5 As New Point(0, 795)
        Dim puntoFinal5 As New Point(1000, 795)

        ' Crear un objeto Pen para dibujar la línea
        Dim pen5 As New Pen(Color.Black, 5)

        ' Dibujar la línea utilizando el método DrawLine de la clase Graphics
        g5.DrawLine(pen5, puntoInicio5, puntoFinal5)
    End Sub


End Class

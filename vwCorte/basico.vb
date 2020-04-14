Imports MySql.Data.MySqlClient
Imports System.Security.Cryptography
Imports System.Text
Imports System.IO
Imports System.Data
Imports System.Net.Mail
Imports System.Net

Module basico
    Public errorBD As String
    Public horaDesde As DateTime
    Public ultimaFalla
    Public autenticado As Boolean
    Public usuarioCerrar As String
    Public rutaBD As String = "logisticar"

    Sub Main()
        Dim cadSQL As String = "SELECT ultimo_corte FROM " & rutaBD & ".configuracion"
        Dim reader As DataSet = consultaSEL(cadSQL)
        Dim regsAfectados = 0
        If errorBD.Length > 0 Then
            agregarLOG("Ocurrió un error al intentar leer MySQL. Error: " + Microsoft.VisualBasic.Strings.Left(errorBD, 250), 9, 0)
        Else
            If reader.Tables(0).Rows.Count > 0 Then
                Dim cortar As Boolean = False
                Dim miFMinima
                If reader.Tables(0).Rows(0)!ultimo_corte.Equals(System.DBNull.Value) Then
                    cortar = True
                    cadSQL = "SELECT MIN(inicio) as fminima FROM " & rutaBD & ".alarmas"
                    Dim fMinima As DataSet = consultaSEL(cadSQL)
                    If fMinima.Tables(0).Rows.Count > 0 Then
                        miFMinima = fMinima.Tables(0).Rows(0)!fminima
                    End If
                Else
                    miFMinima = DateAdd(DateInterval.Hour, 1, CDate(Format(reader.Tables(0).Rows(0)!ultimo_corte, "yyyy/MM/dd HH") & ":00:00"))
                    If Now() >= miFMinima Then cortar = True
                End If
                'Se hace el corte
                If cortar Then
                    'Se hace el algoritmo de cálculo de la hora
                    'Calcular el piso
                    Dim piso = Format(miFMinima, "yyyy/MM/dd HH") & ":00:00"
                    Dim techo = Format(miFMinima, "yyyy/MM/dd HH") & ":59:59"

                    Dim Salir = False
                    Dim cortes = 0
                    Do While Not Salir

                        regsAfectados = consultaACT("DELETE FROM " & rutaBD & ".vw_resumen WHERE desde = '" & piso & "';INSERT INTO " & rutaBD & ".vw_resumen (desde, hasta, nave, estacion, responsable, tecnologia, codigo, fallas_generadas, fallas_cerradas, fallas_escaladas, fallas_entiempo, fallas_total) SELECT '" & piso & "', '" & techo & "', vw_alarmas.nave, vw_alarmas.estacion, vw_alarmas.responsable, vw_alarmas.tecnologia, vw_alarmas.codigo, SUM(IF(vw_alarmas.inicio >= '" & piso & "' AND vw_alarmas.inicio <= '" & techo & "', 1, 0)), SUM(IF(vw_alarmas.fin >= '" & piso & "' AND vw_alarmas.fin <= '" & techo & "' AND vw_alarmas.tiempo > 0, 1, 0)), SUM(IF(vw_reportes.escalamientos > 0 AND vw_alarmas.tiempo > 0, 1, 0)), SUM(IF((vw_reportes.escalamientos = 0 OR ISNULL(vw_reportes.escalamientos)) AND vw_alarmas.tiempo > 0, 1, 0)), SUM(IF(vw_alarmas.tiempo = 0, 1, 0)) FROM " & rutaBD & ".alarmas LEFT JOIN " & rutaBD & ".vw_reportes ON vw_alarmas.reporte = vw_reportes.id GROUP BY vw_alarmas.nave, vw_alarmas.estacion, vw_alarmas.responsable, vw_alarmas.tecnologia, vw_alarmas.codigo")
                        cortes = cortes + regsAfectados
                        Dim nPiso = DateAdd(DateInterval.Hour, 1, CDate(piso))
                        If Format(nPiso, "yyyy/MM/dd HH") >= Format(Now(), "yyyy/MM/dd HH") Then
                            Salir = True
                        Else
                            piso = Format(DateAdd(DateInterval.Hour, 1, CDate(piso)), "yyyy/MM/dd HH") & ":00:00"
                            techo = Format(CDate(piso), "yyyy/MM/dd HH") & ":59:59"
                        End If
                    Loop
                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".configuracion SET ultimo_corte = '" & Format(Now, "yyyy/MM/dd HH:mm:ss") & "'")
                    agregarLOG(IIf(cortes = 1, "Se agregó un registro", "Se agregaron " & cortes & " registros") & " a las estadísticas de fallas (cortes)", 9, 0)
                End If
            End If
        End If

    End Sub

    Private Sub agregarLOG(cadena As String, tipo As Integer, reporte As Integer, Optional aplicacion As Integer = 1)
        'Se agrega a la base de datos
        'tipo 1: Info
        'tipo 2: Incongruencia en los datos (usuario)
        'tipo 8: Error crítico de Base de datos infofallas
        'tipo 9: Error crítico de Base de datos sigma
        Dim regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".log (aplicacion, tipo, reporte, texto) VALUES (30, " & tipo & ", " & reporte & ", '" & Microsoft.VisualBasic.Strings.Left(cadena, 250) & "')")
        If aplicacion = 10 Then
            regsAfectados = consultaACT("UPDATE " & rutaBD & ".configuracion SET flag_monitor = 'S'")
        End If
    End Sub
    Public Function consultaACT(cadena As String) As Integer
        Dim miConexion = New MySqlConnection

        miConexion.ConnectionString = cadenaConexion()

        miConexion.Open()
        consultaACT = 0
        errorBD = ""
        If miConexion.State = ConnectionState.Open Then
            Try
                Dim comandoSQL As MySqlCommand = New MySqlCommand(cadena)
                comandoSQL.Connection = miConexion
                consultaACT = comandoSQL.ExecuteNonQuery()

            Catch ex As Exception
                errorBD = ex.Message
            End Try
        End If
        miConexion.Dispose()
        miConexion.Close()
        miConexion = Nothing
    End Function

    Public Function consultaSEL(cadena As String) As Data.DataSet

        Try
            errorBD = ""
            Dim miConexion = New MySqlConnection

            miConexion.ConnectionString = cadenaConexion()

            miConexion.Open()

            If miConexion.State = ConnectionState.Open Then
                Try
                    Dim comandoSQL As MySqlCommand = New MySqlCommand(cadena)
                    comandoSQL.Connection = miConexion
                    Dim adapter As New MySqlDataAdapter(comandoSQL)
                    Dim LaData As New DataSet
                    adapter.Fill(LaData, "miData")

                    Return LaData
                Catch ex As Exception
                    errorBD = ex.Message
                End Try
            End If
            miConexion.Dispose()
            miConexion.Close()
            miConexion = Nothing
        Catch ex As Exception
            errorBD = ex.Message
        End Try

    End Function

    Function ValNull(ByVal ArVar As Object, ByVal arTipo As String) As Object
        Try
            'para columnas vacias sin datos
            If ArVar.Equals(System.DBNull.Value) Then
                Select Case arTipo
                    Case "A"
                        ValNull = ""
                    Case "N"
                        ValNull = 0
                    Case "D"
                        ValNull = 0
                    Case "F"
                        ValNull = CDate("00/00/0000")
                    Case "DT"
                        ValNull = New DateTime(1, 1, 1)
                    Case Else
                        ValNull = ""
                End Select
                Exit Function
            End If

            If Len(ArVar) > 0 Then
                Select Case arTipo
                    Case "A"
                        ValNull = ArVar
                    Case "N"
                        ValNull = Val(ArVar)
                    Case "D"
                        ValNull = CDec(ArVar)
                    Case "F"
                        If ArVar = "0" Then
                            ValNull = ""
                        Else
                            If InStr(ArVar, "/") > 0 Then
                                ValNull = ArVar
                            Else
                                ValNull = Format(ArVar, "dd/MM/yyyy")
                            End If
                        End If
                    Case Else
                        ValNull = ArVar
                End Select
            Else
                Select Case arTipo
                    Case "A"
                        ValNull = ""
                    Case "N"
                        ValNull = 0
                    Case "D"
                        ValNull = 0
                    Case "F"
                        ValNull = CDate("dd/MM/yyyy")
                    Case Else
                        ValNull = ""
                End Select
            End If
        Catch ex As Exception
            Select Case arTipo
                Case "A"
                    ValNull = ""
                Case "N"
                    ValNull = 0
                Case "D"
                    ValNull = 0
                Case "F"
                    ValNull = CDate("00000000")
                Case Else
                    ValNull = " "
            End Select
        End Try
    End Function

    Function cadenaConexion() As String
        cadenaConexion = "server=localhost;user id=root;password=usbw;port=3307;Convert Zero Datetime=True"
        'cadenaConexion = "server=10.241.241.30;user id=root;password=usbw;port=3307;Convert Zero Datetime=True"
    End Function


End Module

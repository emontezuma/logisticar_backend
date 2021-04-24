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
    Public cadenaConexion As String
    Public be_log_activar As Boolean = False
    Public rutaBD As String = "logisticar2"

    Sub Main(argumentos As String())
        If Process.GetProcessesByName _
          (Process.GetCurrentProcess.ProcessName).Length > 1 Then
        ElseIf argumentos.Length = 0 Then
            MsgBox("No se puede la generación de archivos para SMS: Se requiere la cadena de conexión", MsgBoxStyle.Critical, "SIGMA Monitor")
        Else
            cadenaConexion = argumentos(0)
            'cadenaConexion = "server=localhost;user id=root;password=usbw;port=3307;Convert Zero Datetime=True"
            Dim idProceso = Process.GetCurrentProcess.Id

            Dim mensajesDS As DataSet
            Dim eMensaje = ""
            Dim eTitulo = ""
            Dim audiosGen = 0
            Dim audiosNGen = 0
            Dim mTotal = 0
            'Escalada 4
            Dim miError As String = ""
            Dim optimizar As Boolean = False
            Dim mantenerPrioridad As Boolean = False
            Dim rutaSMS As String = ""
            Dim mensajeGenerado As Boolean = False
            Dim be_alarmas_sms As Boolean = False
            Dim regsAfectados = 0
            Dim registroDS As DataSet

            Dim canales As String = ""
            Dim laLinea As String = ""
            Dim laMaquina As String = ""
            Dim laArea As String = ""
            Dim laFalla As String = ""
            Dim fecha
            Dim tiempo As String = ""
            Dim nroReporte As Integer = 0
            Dim cadSQL As String = "SELECT * FROM " & rutaBD & ".configuracion"
            Dim readerDS As DataSet = consultaSEL(cadSQL)
            If readerDS.Tables(0).Rows.Count > 0 Then
                Dim reader As DataRow = readerDS.Tables(0).Rows(0)
                optimizar = ValNull(reader!optimizar_sms, "A") = "S"
                be_log_activar = ValNull(reader!be_log_activar, "A") = "S"
                mantenerPrioridad = ValNull(reader!mantener_prioridad, "A") = "S"
                be_alarmas_sms = ValNull(reader!be_alarmas_sms, "A") = "S"
                rutaSMS = ValNull(reader!ruta_sms, "A")
            End If
            If be_alarmas_sms Then
                If rutaSMS.Length = 0 Then
                    rutaSMS = My.Computer.FileSystem.SpecialDirectories.MyDocuments
                Else
                    rutaSMS = Strings.Replace(rutaSMS, "/", "\")
                End If
                If Not My.Computer.FileSystem.DirectoryExists(rutaSMS) Then
                    Try
                        My.Computer.FileSystem.CreateDirectory(rutaSMS)
                    Catch ex As Exception
                        rutaSMS = My.Computer.FileSystem.SpecialDirectories.MyDocuments
                    End Try
                End If
                regsAfectados = consultaACT("UPDATE " & rutaBD & ".mensajes SET estatus = '" & idProceso & "' WHERE canal = 1 AND estatus = 'E'")
                Dim agrupado As Boolean = True
                If Not optimizar Then
                    cadSQL = "SELECT a.id, d.telefonos, a.prioridad, z.texto, z.titulo, 1 AS cuenta FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".mensajes_procesados z ON a.id = z.mensaje INNER JOIN " & rutaBD & ".cat_alertas b on a.alerta = b.id INNER JOIN " & rutaBD & ".cat_distribucion d ON a.lista = d.id AND b.estatus = 'A' WHERE a.estatus = '" & idProceso & "' ORDER BY a.prioridad DESC, a.id"
                    agrupado = False
                ElseIf mantenerPrioridad Then
                    cadSQL = "SELECT a.lista, a.prioridad, b.telefonos, COUNT(*) AS cuenta, MAX(a.id) AS id FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".mensajes_procesados z ON a.id = z.mensaje INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c on a.alerta = c.id WHERE a.estatus = '" & idProceso & "' GROUP BY a.prioridad, a.lista, b.telefonos ORDER BY prioridad DESC"
                Else
                    cadSQL = "SELECT a.lista, b.telefonos, 0 AS prioridad, COUNT(*) AS cuenta, MAX(a.id) AS id FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".mensajes_procesados z ON a.id = z.mensaje INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c on a.alerta = c.id WHERE a.estatus = '" & idProceso & "' GROUP BY a.lista, b.telefonos"

                End If
                'Se preselecciona la voz
                mensajesDS = consultaSEL(cadSQL)
                If mensajesDS.Tables(0).Rows.Count > 0 Then
                    For Each elmensaje In mensajesDS.Tables(0).Rows
                        canales = ValNull(elmensaje!telefonos, "A")
                        eMensaje = ""
                        If elmensaje!cuenta > 1 Then
                            eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES POR ATENDER"
                            If mantenerPrioridad And elmensaje!prioridad > 0 Then
                                eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES PRIORITARIOS"
                            End If
                        ElseIf agrupado Then
                            cadSQL = "SELECT * FROM " & rutaBD & ".mensajes_procesados WHERE mensaje = " & elmensaje!id
                            Dim mensajeAgrupado As DataSet = consultaSEL(cadSQL)
                            If mensajeAgrupado.Tables(0).Rows.Count > 0 Then
                                eMensaje = ValNull(mensajeAgrupado.Tables(0).Rows(0)!texto, "A")
                            End If
                        Else
                            eMensaje = ValNull(elmensaje!texto, "A")
                        End If
                        If eMensaje.Length > 0 And canales.Length > 0 Then
                            Dim antes As String = "ÃÀÁÄÂÈÉËÊÌÍÏÎÒÓÖÔÙÚÜÛãàáäâèéëêìíïîòóöôùúüûÑñÇç"
                            Dim ahora As String = "AAAAAEEEEIIIIOOOOUUUUaaaaaeeeeiiiioooouuuunncc"
                            For i = 0 To antes.Length - 1
                                eMensaje = Replace(eMensaje, antes(i), ahora(i))
                            Next
                            eMensaje = Replace(eMensaje, ";", " ")
                            eMensaje = Replace(eMensaje, "\", "-")
                            eMensaje = Replace(eMensaje, "/", "-")

                            eMensaje = Replace(eMensaje, System.Environment.NewLine, " ")
                            eMensaje = Replace(eMensaje, "[90]", "")

                            Dim telefonos As String()
                            Dim tempArray As String()
                            Dim totalItems = 0
                            If canales.Length > 0 Then
                                Dim arreCanales = canales.Split(New Char() {";"c})
                                For i = LBound(arreCanales) To UBound(arreCanales)
                                    'Redimensionamos el Array temporal y preservamos el valor  
                                    ReDim Preserve telefonos(totalItems + i)
                                    telefonos(totalItems + i) = arreCanales(i)
                                Next
                                tempArray = telefonos
                                totalItems = telefonos.Length

                                Dim x As Integer, y As Integer
                                Dim z As Integer

                                For x = 0 To UBound(telefonos)
                                    z = 0
                                    For y = 0 To UBound(telefonos) - 1
                                        'Si el elemento del array es igual al array temporal  
                                        If telefonos(x) = tempArray(z) And y <> x Then
                                            'Entonces Eliminamos el valor duplicado  
                                            telefonos(y) = ""
                                        End If
                                        z = z + 1
                                    Next y
                                Next x
                                canales = ""
                            End If
                            eMensaje = Microsoft.VisualBasic.Strings.Left(eMensaje, 120).Trim
                            mensajeGenerado = False
                            For i = 0 To UBound(telefonos)
                                If telefonos(i).Length > 0 Then
                                    Try
                                        'System.IO.File.Create(rutaSMS & "\" & telefonos(i) & Format(Now, "hhmmss") & i & ".txt").Dispose()
                                        Dim objWriter As New System.IO.StreamWriter(rutaSMS & "\" & telefonos(i) & Format(Now, "hhmmss") & i & ".txt", True)
                                        objWriter.WriteLine(eMensaje)
                                        objWriter.Close()
                                        audiosGen = audiosGen + 1
                                        mensajeGenerado = True
                                    Catch ex As Exception
                                        audiosNGen = audiosNGen + 1
                                        miError = ex.Message
                                    End Try
                                End If
                            Next
                        End If
                        cadSQL = "UPDATE " & rutaBD & ".mensajes SET estatus = 'Z', enviada = NOW() WHERE id = " & elmensaje!id
                        If optimizar Then
                            If elmensaje!cuenta > 1 Then
                                cadSQL = "UPDATE " & rutaBD & ".mensajes SET estatus = 'Z', enviada = NOW() WHERE canal = 1 AND lista = " & elmensaje!lista & " AND estatus = '" & idProceso & "'"
                                If mantenerPrioridad Then
                                    cadSQL = cadSQL & " AND prioridad = " & elmensaje!prioridad
                                End If
                            End If
                        End If
                        regsAfectados = consultaACT(cadSQL)
                    Next

                    If audiosNGen > 0 Then
                        agregarLOG("No se generaron " & audiosNGen & " mensaje(s) de texto")
                    End If
                    ' agregarLOG("Se generaron " & audiosGen & " mensaje(s) de texto. Inicia ARDUINO")
                    ' Shell(Application.StartupPath & "\arduino.exe " & Chr(34) & cadenaConexion & Chr(34), AppWinStyle.MinimizedNoFocus)
                    'End If
                End If
            End If
        End If
        Application.Exit()
    End Sub
    Public Function consultaACT(cadena As String) As Integer
        Dim miConexion = New MySqlConnection

        miConexion.ConnectionString = cadenaConexion

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

            miConexion.ConnectionString = cadenaConexion

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

    Function calcularTiempo(Seg) As String
        calcularTiempo = ""
        If Seg < 60 Then
            calcularTiempo = Seg & " seg"
        ElseIf Seg < 3600 Then
            calcularTiempo = Math.Round(Seg / 60, 1) & " min"
        Else
            calcularTiempo = Math.Round(Seg / 3600, 1) & " hr"
        End If
    End Function

    Function calcularTiempoCad(Seg) As String
        calcularTiempoCad = "-"
        Dim horas = Math.Floor(Seg / 3600)
        Dim minutos = Math.Floor((Seg Mod 3600) / 60)
        Dim segundos = (Seg Mod 3600) Mod 60
        calcularTiempoCad = horas & ":" & Format(minutos, "00") & ":" & Format(segundos, "00")
    End Function

    Private Sub agregarLOG(cadena As String, Optional reporte As Integer = 0, Optional tipo As Integer = 0, Optional aplicacion As Integer = 50)
        If Not be_log_activar Then Exit Sub
        'tipo 0: Info
        'tipo 2: Advertencia
        'tipo 9: Error
        Dim regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".log (aplicacion, tipo, proceso, texto) VALUES (" & aplicacion & ", " & tipo & ", " & reporte & ", '" & Microsoft.VisualBasic.Strings.Left(cadena, 250) & "')")
    End Sub


End Module

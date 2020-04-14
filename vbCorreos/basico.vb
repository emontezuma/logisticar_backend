
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
    Public rutaBD As String = "logisticar"

    Sub Main(argumentos As String())
        If Process.GetProcessesByName _
          (Process.GetCurrentProcess.ProcessName).Length > 1 Then
        ElseIf argumentos.Length = 0 Then
            MsgBox("No se puede iniciar el envío de correos: Se requiere la cadena de conexión", MsgBoxStyle.Critical, "SIGMA Monitor")
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
            Dim correo_titulo_falla As Boolean
            Dim correo_titulo As String
            Dim correo_cuerpo As String
            Dim correo_firma As String
            Dim correo_cuenta As String
            Dim correo_puerto As String
            Dim correo_ssl As Boolean
            Dim correo_clave As String
            Dim correo_host As String
            Dim separador_mail As String
            Dim mensajeGenerado As Boolean = False
            Dim be_alarmas_correos As Boolean = False
            Dim regsAfectados = 0
            Dim registroDS As DataSet

            Dim canales As String = ""
            Dim laLinea As String = ""
            Dim laMaquina As String = ""
            Dim laArea As String = ""
            Dim laFalla As String = ""
            Dim elLote As String = ""
            Dim elProceso As String = ""
            Dim laCarga As String = ""
            Dim fecha
            Dim tiempo As String = ""
            Dim nroReporte As Integer = 0

            Dim cadSQL As String = "SELECT * FROM " & rutaBD & ".configuracion"
            Dim readerDS As DataSet = consultaSEL(cadSQL)
            Dim escape_mensaje = ""
            If readerDS.Tables(0).Rows.Count > 0 Then
                Dim reader As DataRow = readerDS.Tables(0).Rows(0)
                optimizar = ValNull(reader!optimizar_correo, "A") = "S"
                be_log_activar = ValNull(reader!be_log_activar, "A") = "S"
                mantenerPrioridad = ValNull(reader!optimizar_mmcall, "A") = "S"
                correo_titulo_falla = ValNull(reader!correo_titulo_falla, "A") = "S"
                be_alarmas_correos = ValNull(reader!be_alarmas_correos, "A") = "S"
                correo_titulo = ValNull(reader!correo_titulo, "A")
                correo_cuerpo = ValNull(reader!correo_cuerpo, "A")
                correo_firma = ValNull(reader!correo_firma, "A")
                correo_cuenta = ValNull(reader!correo_cuenta, "A")
                correo_clave = ValNull(reader!correo_clave, "A")
                correo_puerto = ValNull(reader!correo_puerto, "A")
                correo_ssl = ValNull(reader!correo_ssl, "A") = "S"
                escape_mensaje = ValNull(reader!escape_mensaje, "A")
                correo_host = ValNull(reader!correo_host, "A")
                separador_mail = ValNull(reader!separador_mail, "A")
            End If
            If separador_mail = "" Then separador_mail = ";"
            If correo_firma.Length = 0 Then
                correo_firma = "Le agradecemos no responder a este correo, se envía desde una cuenta no supervisada."
            End If
            If correo_titulo.Length = 0 Then
                correo_titulo = "SIGMA Monitor versión 1.0"
            End If

            If be_alarmas_correos Then

                regsAfectados = consultaACT("UPDATE " & rutaBD & ".mensajes SET estatus = '" & idProceso & "' WHERE canal = 2 AND estatus = 'E'")
                Dim agrupado As Boolean = True
                If Not optimizar Then
                    cadSQL = "SELECT a.id, d.correos, a.prioridad, z.texto, z.titulo, 1 AS cuenta FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".mensajes_procesados z ON a.id = z.mensaje INNER JOIN " & rutaBD & ".cat_alertas b on a.alerta = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_distribucion d ON a.lista = d.id AND d.estatus = 'A' WHERE a.estatus = '" & idProceso & "' ORDER BY a.prioridad DESC, a.id"
                    agrupado = False
                ElseIf mantenerPrioridad Then
                    cadSQL = "SELECT a.lista, a.prioridad, b.correos, COUNT(*) AS cuenta, MAX(a.id) AS id FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".mensajes_procesados z ON a.id = z.mensaje INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c on a.alerta = c.id WHERE a.estatus = '" & idProceso & "' GROUP BY a.prioridad, a.lista, b.correos ORDER BY prioridad DESC"
                Else
                    cadSQL = "SELECT a.lista, b.correos, 0 AS prioridad, COUNT(*) AS cuenta, MAX(a.id) AS id FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".mensajes_procesados z ON a.id = z.mensaje INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c on a.alerta = c.id WHERE a.estatus = '" & idProceso & "' GROUP BY a.lista, b.correos"

                End If
                'Se preselecciona la voz
                mensajesDS = consultaSEL(cadSQL)
                If mensajesDS.Tables(0).Rows.Count > 0 Then
                    Dim enlazado As Boolean = False
                    Dim smtpServer As New SmtpClient()

                    Try
                        smtpServer.Credentials = New Net.NetworkCredential(correo_cuenta, correo_clave)
                        smtpServer.Port = correo_puerto
                        smtpServer.Host = correo_host '"smtp.live.com" '"smtp.gmail.com"
                        smtpServer.EnableSsl = correo_ssl
                        enlazado = True
                    Catch ex As Exception
                        regsAfectados = consultaACT("UPDATE " & rutaBD & ".mensajes SET estatus = 'A' WHERE canal = 2 AND estatus = '" & idProceso & "'")
                        smtpServer.Dispose()
                    End Try
                    If enlazado Then
                        For Each elmensaje In mensajesDS.Tables(0).Rows
                            canales = ValNull(elmensaje!correos, "A")
                            If canales.Length > 0 Then
                                eMensaje = ""
                                eTitulo = ""
                                Dim cadWhere = "AND a.lista = " & elmensaje!lista
                                If elmensaje!cuenta > 1 Then
                                    eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES POR ATENDER"
                                    If mantenerPrioridad And elmensaje!prioridad > 0 Then
                                        eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES PRIORITARIOS"
                                    End If

                                    If mantenerPrioridad Then
                                        cadWhere = cadWhere & " AND a.prioridad = " & elmensaje!prioridad
                                    End If
                                    cadSQL = "SELECT a.id, z.texto, z.titulo FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".mensajes_procesados z ON a.id = z.mensaje WHERE a.estatus = '" & idProceso & "' " & cadWhere
                                Else
                                    cadSQL = "SELECT a.id, z.texto, z.titulo FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".mensajes_procesados z ON a.id = z.mensaje WHERE a.id = " & elmensaje!id
                                End If
                                Dim mensajeCorreo As String = ""
                                registroDS = consultaSEL(cadSQL)
                                Dim linCorreo = 0
                                If registroDS.Tables(0).Rows.Count > 0 Then
                                    For Each elCorreo In registroDS.Tables(0).Rows
                                        linCorreo = linCorreo + 1
                                        If elmensaje!cuenta > 1 Then
                                            If linCorreo = 1 Then
                                                mensajeCorreo = "Mensaje " & linCorreo & " de " & elmensaje!cuenta & vbCrLf
                                            Else
                                                mensajeCorreo = mensajeCorreo & vbCrLf & vbCrLf & "Mensaje " & linCorreo & " de " & elmensaje!cuenta & vbCrLf
                                            End If
                                        End If
                                        eMensaje = ValNull(elCorreo!texto, "A")
                                        mensajeCorreo = mensajeCorreo & eMensaje & vbCrLf
                                        eTitulo = ValNull(elCorreo!titulo, "A")
                                    Next
                                    If eTitulo.Length = 0 Then
                                        eTitulo = correo_titulo
                                    End If
                                    mensajeCorreo = mensajeCorreo & vbCrLf & vbCrLf & correo_firma

                                    Dim correos As String()
                                    Dim tempArray As String()
                                    Dim totalItems = 0
                                    If canales.Length > 0 Then
                                        Dim arreCanales = canales.Split(New Char() {";"c})
                                        For i = LBound(arreCanales) To UBound(arreCanales)
                                            'Redimensionamos el Array temporal y preservamos el valor  
                                            ReDim Preserve correos(totalItems + i)
                                            correos(totalItems + i) = arreCanales(i)
                                        Next
                                        tempArray = correos
                                        totalItems = correos.Length

                                        Dim x As Integer, y As Integer
                                        Dim z As Integer

                                        For x = 0 To UBound(correos)
                                            z = 0
                                            For y = 0 To UBound(correos) - 1
                                                'Si el elemento del array es igual al array temporal  
                                                If correos(x) = tempArray(z) And y <> x Then
                                                    'Entonces Eliminamos el valor duplicado  
                                                    correos(y) = ""
                                                End If
                                                z = z + 1
                                            Next y
                                        Next x
                                    End If
                                    mensajeGenerado = False
                                    Dim mail As New MailMessage
                                    Try
                                        mail.From = New MailAddress(correo_cuenta)
                                        For i = 0 To UBound(correos)
                                            If correos(i).Length > 0 Then
                                                mail.To.Add(correos(i))
                                            End If
                                        Next i

                                        mail.Subject = eTitulo
                                        mail.Body = mensajeCorreo
                                        smtpServer.Send(mail)
                                        audiosGen = audiosGen + 1
                                        mensajeGenerado = True
                                    Catch ex As Exception
                                        audiosNGen = audiosNGen + 1
                                        agregarLOG(ex.Message, 9, nroReporte)
                                    Finally
                                        mail.Dispose()
                                    End Try
                                    cadSQL = "UPDATE " & rutaBD & ".mensajes SET estatus = 'Z', enviada = NOW() WHERE id = " & elmensaje!id
                                    If optimizar Then
                                        If elmensaje!cuenta > 1 Then
                                            cadSQL = "UPDATE " & rutaBD & ".mensajes SET estatus = 'Z', enviada = NOW() WHERE canal = 2 AND lista = " & elmensaje!lista & " AND estatus = '" & idProceso & "'"
                                            If mantenerPrioridad Then
                                                cadSQL = cadSQL & " AND prioridad = " & elmensaje!prioridad
                                            End If
                                        End If
                                    End If
                                    regsAfectados = consultaACT(cadSQL)
                                End If
                            End If

                        Next
                        cadSQL = "SELECT a.id, a.texto, b.correos FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' WHERE a.alerta = 0 AND a.canal = 2 AND a.estatus = '" & idProceso & "' ORDER BY a.prioridad DESC, a.id"
                        mensajesDS = consultaSEL(cadSQL)
                        Dim generarMensaje = False
                        If mensajesDS.Tables(0).Rows.Count > 0 Then
                            For Each elmensaje In mensajesDS.Tables(0).Rows
                                canales = ValNull(elmensaje!correos, "A")
                                eMensaje = "Se agotó el número de intentos de llamada para el teléfono " & ValNull(elmensaje!texto, "A")
                                If canales.Length > 0 Then
                                    Dim arreCanales = canales.Split(New Char() {";"c})
                                    Dim correos As String()
                                    Dim tempArray As String()
                                    Dim totalItems = 0

                                    For i = LBound(arreCanales) To UBound(arreCanales)
                                        'Redimensionamos el Array temporal y preservamos el valor  
                                        ReDim Preserve correos(totalItems + i)
                                        correos(totalItems + i) = arreCanales(i)
                                    Next
                                    tempArray = correos
                                    totalItems = correos.Length

                                    Dim x As Integer, y As Integer
                                    Dim z As Integer

                                    For x = 0 To UBound(correos)
                                        z = 0
                                        For y = 0 To UBound(correos) - 1
                                            'Si el elemento del array es igual al array temporal  
                                            If correos(x) = tempArray(z) And y <> x Then
                                                'Entonces Eliminamos el valor duplicado  
                                                correos(y) = ""
                                            End If
                                            z = z + 1
                                        Next y
                                    Next x
                                    mensajeGenerado = False
                                    Dim mail As New MailMessage
                                    Try
                                        mail.From = New MailAddress(correo_cuenta)
                                        For i = 0 To UBound(correos)
                                            If correos(i).Length > 0 Then
                                                mail.To.Add(correos(i))
                                            End If
                                        Next i

                                        mail.Subject = eMensaje
                                        mail.Body = eMensaje
                                        smtpServer.Send(mail)
                                        audiosGen = audiosGen + 1
                                        mensajeGenerado = True
                                    Catch ex As Exception
                                        audiosNGen = audiosNGen + 1
                                        agregarLOG(ex.Message, 9, nroReporte)
                                    Finally
                                        mail.Dispose()
                                    End Try
                                End If
                                If mensajeGenerado Then
                                    cadSQL = "UPDATE " & rutaBD & ".mensajes SET estatus = 'Z', enviada = NOW() WHERE id = " & elmensaje!id
                                    regsAfectados = consultaACT(cadSQL)
                                End If
                            Next
                            If audiosGen > 0 Or audiosNGen > 0 Then
                                agregarLOG("Se generaron " & audiosGen & " audio(s) y no se generaron " & audiosNGen & " audio(s)")
                            End If
                        End If
                    End If
                    If audiosGen > 0 Or audiosNGen > 0 Then
                        agregarLOG("Se generaron " & audiosGen & " correo(s) y no se generaron " & audiosNGen & " correo(s)")
                    End If
                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".mensajes SET estatus = 'Z' WHERE canal = 2 AND estatus = '" & idProceso & "'")
                    smtpServer.Dispose()
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

    'Function cadenaConexion() As String
    '   cadenaConexion = "server=localhost;user id=root;password=usbw;port=3307;Convert Zero Datetime=True"
    'cadenaConexion = "server=10.241.241.30;user id=root;password=usbw;port=3307;Convert Zero Datetime=True"

    'End Function

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

    Private Sub agregarLOG(cadena As String, Optional reporte As Integer = 0, Optional tipo As Integer = 0, Optional aplicacion As Integer = 40)
        If Not be_log_activar Then Exit Sub
        'tipo 0: Info
        'tipo 2: Advertencia
        'tipo 9: Error
        Dim regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".log (aplicacion, tipo, proceso, texto) VALUES (" & aplicacion & ", " & tipo & ", " & reporte & ", '" & Microsoft.VisualBasic.Strings.Left(cadena, 250) & "')")
    End Sub

    Sub MainAntes(argumentos As String())
        If Process.GetProcessesByName _
          (Process.GetCurrentProcess.ProcessName).Length > 1 Then
        ElseIf argumentos.Length = 0 Then
            MsgBox("No se puede iniciar el envío de correos: Se requiere la cadena de conexión", MsgBoxStyle.Critical, "SIGMA Monitor")
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
            Dim correo_titulo_falla As Boolean
            Dim correo_titulo As String
            Dim correo_cuerpo As String
            Dim correo_firma As String
            Dim correo_cuenta As String
            Dim correo_puerto As String
            Dim correo_ssl As Boolean
            Dim correo_clave As String
            Dim correo_host As String
            Dim separador_mail As String
            Dim mensajeGenerado As Boolean = False
            Dim be_alarmas_correos As Boolean = False
            Dim regsAfectados = 0
            Dim registroDS As DataSet

            Dim canales As String = ""
            Dim laLinea As String = ""
            Dim laMaquina As String = ""
            Dim laArea As String = ""
            Dim laFalla As String = ""
            Dim elLote As String = ""
            Dim elProceso As String = ""
            Dim laCarga As String = ""
            Dim fecha
            Dim tiempo As String = ""
            Dim nroReporte As Integer = 0

            Dim cadSQL As String = "SELECT * FROM " & rutaBD & ".configuracion"
            Dim readerDS As DataSet = consultaSEL(cadSQL)
            Dim escape_mensaje = ""
            If readerDS.Tables(0).Rows.Count > 0 Then
                Dim reader As DataRow = readerDS.Tables(0).Rows(0)
                optimizar = ValNull(reader!optimizar_correo, "A") = "S"
                be_log_activar = ValNull(reader!be_log_activar, "A") = "S"
                mantenerPrioridad = ValNull(reader!optimizar_mmcall, "A") = "S"
                correo_titulo_falla = ValNull(reader!correo_titulo_falla, "A") = "S"
                be_alarmas_correos = ValNull(reader!be_alarmas_correos, "A") = "S"
                correo_titulo = ValNull(reader!correo_titulo, "A")
                correo_cuerpo = ValNull(reader!correo_cuerpo, "A")
                correo_firma = ValNull(reader!correo_firma, "A")
                correo_cuenta = ValNull(reader!correo_cuenta, "A")
                correo_clave = ValNull(reader!correo_clave, "A")
                correo_puerto = ValNull(reader!correo_puerto, "A")
                correo_ssl = ValNull(reader!correo_ssl, "A") = "S"
                escape_mensaje = ValNull(reader!escape_mensaje, "A")
                correo_host = ValNull(reader!correo_host, "A")
                separador_mail = ValNull(reader!separador_mail, "A")
            End If
            If separador_mail = "" Then separador_mail = ";"
            If correo_firma.Length = 0 Then
                correo_firma = "Le agradecemos no responder a este correo, se envía desde una cuenta no supervisada."
            End If
            If correo_titulo.Length = 0 Then
                correo_titulo = "SIGMA Monitor versión 1.0"
            End If

            If be_alarmas_correos Then

                regsAfectados = consultaACT("UPDATE " & rutaBD & ".mensajes SET estatus = '" & idProceso & "' WHERE canal = 2 AND estatus = 'A'")

                If Not optimizar Then
                    cadSQL = "SELECT a.id, 1 AS cuenta, b.evento FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_alertas b on a.alerta = b.id WHERE a.canal = 2 AND a.estatus = '" & idProceso & "' ORDER BY a.prioridad DESC, a.id"
                ElseIf mantenerPrioridad Then
                    cadSQL = "SELECT a.prioridad, a.lista, c.evento, b.correos, COUNT(*) AS cuenta, MAX(a.id) AS id FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c on a.alerta = c.id WHERE a.canal = 2 AND a.estatus = '" & idProceso & "' GROUP BY a.prioridad, a.lista, c.evento, b.correos ORDER BY prioridad DESC"
                Else
                    cadSQL = "SELECT a.lista, c.evento, b.correos, 0 AS prioridad, COUNT(*) AS cuenta, MAX(a.id) AS id FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c on a.alerta = c.id WHERE a.canal = 2 AND a.estatus = '" & idProceso & "' GROUP BY a.lista, c.evento, b.correos, 4"
                End If
                'Se preselecciona la voz

                mensajesDS = consultaSEL(cadSQL)
                Dim generarMensaje As Boolean
                If mensajesDS.Tables(0).Rows.Count > 0 Then
                    Dim enlazado As Boolean = False
                    Dim smtpServer As New SmtpClient()

                    Try
                        smtpServer.Credentials = New Net.NetworkCredential(correo_cuenta, correo_clave)
                        smtpServer.Port = correo_puerto
                        smtpServer.Host = correo_host '"smtp.live.com" '"smtp.gmail.com"
                        smtpServer.EnableSsl = correo_ssl
                        enlazado = True
                    Catch ex As Exception
                        regsAfectados = consultaACT("UPDATE " & rutaBD & ".mensajes SET estatus = 'A' WHERE canal = 2 AND estatus = '" & idProceso & "'")
                        smtpServer.Dispose()
                    End Try
                    If enlazado Then
                        For Each elmensaje In mensajesDS.Tables(0).Rows
                            generarMensaje = False
                            eMensaje = ""
                            eTitulo = ""
                            Dim cadWhere = "AND a.lista = " & elmensaje!lista
                            If elmensaje!evento < 200 Then
                                If elmensaje!cuenta > 1 Then
                                    eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES POR ATENDER"
                                    If mantenerPrioridad And elmensaje!prioridad > 0 Then
                                        eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES PRIORITARIOS"
                                    End If
                                    eTitulo = "SIGMA Monitor v1.0: " & elmensaje!cuenta & " MENSAJE(S) POR ATENDER"
                                    canales = ValNull(elmensaje!correos, "A")
                                    laLinea = ""
                                    laMaquina = ""
                                    laArea = ""
                                    laFalla = ""
                                    tiempo = ""
                                    generarMensaje = True
                                    nroReporte = 0

                                    If mantenerPrioridad Then
                                        cadWhere = cadWhere & " AND a.prioridad = " & elmensaje!prioridad
                                    End If
                                    cadSQL = "SELECT a.*, 0 AS rate, 0 AS oee, b.correos, e.nombre as nlinea, f.nombre as nmaquina, g.nombre as narea, h.nombre as nfalla, c.id AS idalerta, c.evento AS tipoalerta, c.resolucion_mensaje, c.cancelacion_mensaje, c.acumular, c.mensaje, c.titulo, d.fecha, d.inicio_atencion, d.inicio_reporte, d.estatus, i.repeticiones, i.fase, i.escalamientos1, i.escalamientos2, i.escalamientos3, i.escalamientos4, i.escalamientos5 FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c ON a.alerta = c.id INNER JOIN " & rutaBD & ".alarmas i ON a.alarma = i.id LEFT JOIN " & rutaBD & ".reportes d ON a.proceso = d.id LEFT JOIN " & rutaBD & ".cat_lineas e ON d.linea = e.id LEFT JOIN " & rutaBD & ".cat_maquinas f ON d.maquina = f.id LEFT JOIN " & rutaBD & ".cat_areas g ON d.area = g.id LEFT JOIN " & rutaBD & ".cat_fallas h ON d.falla = h.id WHERE a.estatus = '" & idProceso & "' " & cadWhere
                                Else
                                    cadSQL = "SELECT a.*, 0 AS rate, 0 AS oee, b.correos, e.nombre as nlinea, f.nombre as nmaquina, g.nombre as narea, h.nombre as nfalla, c.id AS idalerta, c.evento AS tipoalerta, c.resolucion_mensaje, c.cancelacion_mensaje, c.acumular, c.mensaje, c.titulo, d.fecha, d.inicio_atencion, d.inicio_reporte, d.estatus, i.repeticiones, i.fase, i.escalamientos1, i.escalamientos2, i.escalamientos3, i.escalamientos4, i.escalamientos5 FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c ON a.alerta = c.id INNER JOIN " & rutaBD & ".alarmas i ON a.alarma = i.id LEFT JOIN " & rutaBD & ".reportes d ON a.proceso = d.id LEFT JOIN " & rutaBD & ".cat_lineas e ON d.linea = e.id LEFT JOIN " & rutaBD & ".cat_maquinas f ON d.maquina = f.id LEFT JOIN " & rutaBD & ".cat_areas g ON d.area = g.id LEFT JOIN " & rutaBD & ".cat_fallas h ON d.falla = h.id WHERE a.id = " & elmensaje!id
                                End If
                            ElseIf elmensaje!evento > 200 And elmensaje!evento < 300 Then
                                If elmensaje!cuenta > 1 Then
                                    eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES POR ATENDER"
                                    If mantenerPrioridad And elmensaje!prioridad > 0 Then
                                        eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES PRIORITARIOS"
                                    End If
                                    eTitulo = "SIGMA Monitor v1.0: " & elmensaje!cuenta & " MENSAJE(S) POR ATENDER"
                                    canales = ValNull(elmensaje!correos, "A")
                                    laLinea = ""
                                    laMaquina = ""
                                    laArea = ""
                                    laFalla = ""
                                    tiempo = ""
                                    generarMensaje = True
                                    nroReporte = 0

                                    If mantenerPrioridad Then
                                        cadWhere = cadWhere & " AND a.prioridad = " & elmensaje!prioridad
                                    End If
                                    cadSQL = "SELECT a.*, IF(d.rate_teorico > 0, d.rate / d.rate_teorico * 100, 0) AS rate, d.oee, b.correos, e.nombre as nlinea, f.nombre as nmaquina, '' as narea, '' as nfalla, c.id AS idalerta, c.evento AS tipoalerta, c.resolucion_mensaje, c.cancelacion_mensaje, c.acumular, c.mensaje, c.titulo, d.rate_tendencia_baja AS fecha, d.rate_tendencia_alta AS inicio_atencion, d.parada_desde AS inicio_reporte, d.estatus, i.repeticiones, i.fase, i.escalamientos1, i.escalamientos2, i.escalamientos3, i.escalamientos4, i.escalamientos5 FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c ON a.alerta = c.id INNER JOIN " & rutaBD & ".alarmas i ON a.alarma = i.id LEFT JOIN " & rutaBD & ".relacion_maquinas_lecturas d ON a.proceso = d.equipo LEFT JOIN " & rutaBD & ".cat_maquinas f ON d.equipo = f.id LEFT JOIN " & rutaBD & ".cat_lineas e ON f.linea = e.id  WHERE a.estatus = '" & idProceso & "' " & cadWhere
                                Else
                                    cadSQL = "SELECT a.*, IF(d.rate_teorico > 0, d.rate / d.rate_teorico * 100, 0) AS rate, d.oee, b.correos, e.nombre as nlinea, f.nombre as nmaquina, '' as narea, '' as nfalla, c.id AS idalerta, c.evento AS tipoalerta, c.resolucion_mensaje, c.cancelacion_mensaje, c.acumular, c.mensaje, c.titulo, d.rate_tendencia_baja AS fecha, d.rate_tendencia_alta AS inicio_atencion, d.parada_desde AS inicio_reporte, d.estatus, i.repeticiones, i.fase, i.escalamientos1, i.escalamientos2, i.escalamientos3, i.escalamientos4, i.escalamientos5 FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c ON a.alerta = c.id INNER JOIN " & rutaBD & ".alarmas i ON a.alarma = i.id LEFT JOIN " & rutaBD & ".relacion_maquinas_lecturas d ON a.proceso = d.equipo LEFT JOIN " & rutaBD & ".cat_maquinas f ON d.equipo = f.id LEFT JOIN " & rutaBD & ".cat_lineas e ON f.linea = e.id WHERE a.id = " & elmensaje!id
                                End If
                            ElseIf elmensaje!evento = 302 Or elmensaje!evento = 303 Or elmensaje!evento = 305 Or elmensaje!evento = 306 Then
                                If elmensaje!cuenta > 1 Then
                                    eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES POR ATENDER"
                                    If mantenerPrioridad And elmensaje!prioridad > 0 Then
                                        eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES PRIORITARIOS"
                                    End If
                                    eTitulo = "SIGMA Monitor v1.0: " & elmensaje!cuenta & " MENSAJE(S) POR ATENDER"
                                    canales = ValNull(elmensaje!correos, "A")
                                    laLinea = ""
                                    laMaquina = ""
                                    laArea = ""
                                    laFalla = ""
                                    tiempo = ""
                                    generarMensaje = True
                                    nroReporte = 0

                                    If mantenerPrioridad Then
                                        cadWhere = cadWhere & " AND a.prioridad = " & elmensaje!prioridad
                                    End If
                                    cadSQL = "SELECT a.*, d.hasta, d.numero AS nlote, d.fecha, TIME_TO_SEC(TIMEDIFF(d.hasta, NOW())) AS previo, d.ruta_secuencia, c1.referencia, c1.nombre AS producto, IFNULL(b1.nombre, 'N/A') AS ruta_actual, IFNULL(e1.nombre, 'N/A') as equipo, IFNULL(d1.nombre, 'N/A') as nproceso, b.correos, c.id AS idalerta, c.evento AS tipoalerta, c.resolucion_mensaje, c.cancelacion_mensaje, c.acumular, c.mensaje, c.titulo, i.inicio AS inicio_atencion, NOW() AS inicio_reporte, d.estatus, i.repeticiones, i.fase, i.escalamientos1, i.escalamientos2, i.escalamientos3, i.escalamientos4, i.escalamientos5 FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c ON a.alerta = c.id INNER JOIN " & rutaBD & ".alarmas i ON a.alarma = i.id LEFT JOIN " & rutaBD & ".lotes d ON a.proceso = d.id LEFT JOIN " & rutaBD & ".det_rutas b1 ON d.ruta_detalle = b1.id LEFT JOIN " & rutaBD & ".cat_partes c1 ON d.parte = c1.id LEFT JOIN " & rutaBD & ".cat_procesos d1 ON d.proceso = d1.id LEFT JOIN " & rutaBD & ".cat_maquinas e1 ON d.equipo= e1.id WHERE a.estatus = '" & idProceso & "' " & cadWhere
                                Else
                                    cadSQL = "SELECT a.*, d.hasta, d.numero AS nlote, d.fecha, TIME_TO_SEC(TIMEDIFF(d.hasta, NOW())) AS previo, d.ruta_secuencia, c1.referencia, c1.nombre AS producto, IFNULL(b1.nombre, 'N/A') AS ruta_actual, IFNULL(e1.nombre, 'N/A') as equipo, IFNULL(d1.nombre, 'N/A') as nproceso, b.correos, c.id AS idalerta, c.evento AS tipoalerta, c.resolucion_mensaje, c.cancelacion_mensaje, c.acumular, c.mensaje, c.titulo, i.inicio AS inicio_atencion, NOW() AS inicio_reporte, d.estatus, i.repeticiones, i.fase, i.escalamientos1, i.escalamientos2, i.escalamientos3, i.escalamientos4, i.escalamientos5 FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c ON a.alerta = c.id INNER JOIN " & rutaBD & ".alarmas i ON a.alarma = i.id LEFT JOIN " & rutaBD & ".lotes d ON a.proceso = d.id LEFT JOIN " & rutaBD & ".det_rutas b1 ON d.ruta_detalle = b1.id LEFT JOIN " & rutaBD & ".cat_partes c1 ON d.parte = c1.id LEFT JOIN " & rutaBD & ".cat_procesos d1 ON d.proceso = d1.id LEFT JOIN " & rutaBD & ".cat_maquinas e1 ON d.equipo= e1.id WHERE a.id = " & elmensaje!id
                                End If
                            ElseIf elmensaje!evento = 301 Then
                                If elmensaje!cuenta > 1 Then
                                    eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES POR ATENDER"
                                    If mantenerPrioridad And elmensaje!prioridad > 0 Then
                                        eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES PRIORITARIOS"
                                    End If
                                    eTitulo = "SIGMA Monitor v1.0: " & elmensaje!cuenta & " MENSAJE(S) POR ATENDER"
                                    canales = ValNull(elmensaje!correos, "A")
                                    laLinea = ""
                                    laMaquina = ""
                                    laArea = ""
                                    laFalla = ""
                                    tiempo = ""
                                    generarMensaje = True
                                    nroReporte = 0

                                    If mantenerPrioridad Then
                                        cadWhere = cadWhere & " AND a.prioridad = " & elmensaje!prioridad
                                    End If
                                    cadSQL = "SELECT a.*, e1.referencia, e1.nombre AS producto, b1.numero AS nlote, IFNULL((SELECT MIN(orden) FROM " & rutaBD & ".prioridades WHERE parte = b1.parte AND fecha >= NOW() AND estatus = 'A'), 100) AS prioridad, d.ruta_secuencia, d.ruta_secuencia_antes, IFNULL(c1.nombre, 'N/A') AS ruta_antes, IFNULL(d1.nombre, 'N/A') AS ruta_despues, b.correos, c.id AS idalerta, c.evento AS tipoalerta, c.resolucion_mensaje, c.cancelacion_mensaje, c.acumular, c.mensaje, c.titulo, i.inicio AS fecha, NOW() AS inicio_atencion, NOW() AS inicio_reporte, i.repeticiones, i.fase, i.escalamientos1, i.escalamientos2, i.escalamientos3, i.escalamientos4, i.escalamientos5 FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c ON a.alerta = c.id INNER JOIN " & rutaBD & ".alarmas i ON a.alarma = i.id LEFT JOIN " & rutaBD & ".lotes_historia d ON a.proceso = d.id INNER JOIN " & rutaBD & ".lotes b1 ON d.lote = b1.id LEFT JOIN " & rutaBD & ".det_rutas c1 ON d.ruta_detalle_anterior = c1.id LEFT JOIN " & rutaBD & ".det_rutas d1 ON d.ruta_detalle = d1.id LEFT JOIN " & rutaBD & ".cat_partes e1 ON b1.parte = e1.id WHERE a.estatus = '" & idProceso & "' " & cadWhere
                                Else
                                    cadSQL = "SELECT a.*, e1.referencia, e1.nombre AS producto, b1.numero AS nlote, IFNULL((SELECT MIN(orden) FROM " & rutaBD & ".prioridades WHERE parte = b1.parte AND fecha >= NOW() AND estatus = 'A'), 100) AS prioridad, d.ruta_secuencia, d.ruta_secuencia_antes, IFNULL(c1.nombre, 'N/A') AS ruta_antes, IFNULL(d1.nombre, 'N/A') AS ruta_despues, b.correos, c.id AS idalerta, c.evento AS tipoalerta, c.resolucion_mensaje, c.cancelacion_mensaje, c.acumular, c.mensaje, c.titulo, i.inicio AS fecha, NOW() AS inicio_atencion, NOW() AS inicio_reporte, i.repeticiones, i.fase, i.escalamientos1, i.escalamientos2, i.escalamientos3, i.escalamientos4, i.escalamientos5 FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c ON a.alerta = c.id INNER JOIN " & rutaBD & ".alarmas i ON a.alarma = i.id LEFT JOIN " & rutaBD & ".lotes_historia d ON a.proceso = d.id INNER JOIN " & rutaBD & ".lotes b1 ON d.lote = b1.id LEFT JOIN " & rutaBD & ".det_rutas c1 ON d.ruta_detalle_anterior = c1.id LEFT JOIN " & rutaBD & ".det_rutas d1 ON d.ruta_detalle = d1.id LEFT JOIN " & rutaBD & ".cat_partes e1 ON b1.parte = e1.id WHERE a.id = " & elmensaje!id
                                End If
                            ElseIf elmensaje!evento = 304 Or elmensaje!evento = 307 Then
                                If elmensaje!cuenta > 1 Then
                                    eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES POR ATENDER"
                                    If mantenerPrioridad And elmensaje!prioridad > 0 Then
                                        eMensaje = "HAY " & elmensaje!cuenta & " MENSAJES PRIORITARIOS"
                                    End If
                                    eTitulo = "SIGMA Monitor v1.0: " & elmensaje!cuenta & " MENSAJE(S) POR ATENDER"
                                    canales = ValNull(elmensaje!correos, "A")
                                    laLinea = ""
                                    laMaquina = ""
                                    laArea = ""
                                    laFalla = ""
                                    tiempo = ""
                                    generarMensaje = True
                                    nroReporte = 0

                                    If mantenerPrioridad Then
                                        cadWhere = cadWhere & " AND a.prioridad = " & elmensaje!prioridad
                                    End If
                                    cadSQL = "SELECT a.*, 0 AS previo, d.carga, d.alarma, d.alarma_rep, d.fecha, d.permitir_reprogramacion, d.equipo, d.fecha, IFNULL(b1.nombre, 'N/A') as nequipo, IFNULL(c1.nombre, 'N/A') as nproceso, IFNULL((SELECT SUM(cantidad) FROM " & rutaBD & ".programacion WHERE carga = d.id AND estatus = 'A'), 0) AS piezas, (SELECT COUNT(*) FROM " & rutaBD & ".lotes WHERE carga = d.id) AS avance, b.correos, c.id AS idalerta, c.evento AS tipoalerta, c.resolucion_mensaje, c.cancelacion_mensaje, c.acumular, c.mensaje, c.titulo, i.inicio AS inicio_atencion, NOW() AS inicio_reporte, d.estatus, i.repeticiones, i.fase, i.escalamientos1, i.escalamientos2, i.escalamientos3, i.escalamientos4, i.escalamientos5 FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c ON a.alerta = c.id INNER JOIN " & rutaBD & ".alarmas i ON a.alarma = i.id LEFT JOIN " & rutaBD & ".cargas d ON a.proceso = d.id LEFT JOIN " & rutaBD & ".cat_maquinas b1 ON d.equipo = b1.id AND b1.estatus = 'A' LEFT JOIN " & rutaBD & ".cat_procesos c1 ON b1.proceso = c1.id AND c1.estatus = 'A' WHERE a.estatus = '" & idProceso & "' " & cadWhere
                                Else
                                    cadSQL = "SELECT a.*, 0 AS previo, d.carga, d.alarma, d.alarma_rep, d.fecha, d.permitir_reprogramacion, d.equipo, d.fecha, IFNULL(b1.nombre, 'N/A') as nequipo, IFNULL(c1.nombre, 'N/A') as nproceso, IFNULL((SELECT SUM(cantidad) FROM " & rutaBD & ".programacion WHERE carga = d.id AND estatus = 'A'), 0) AS piezas, (SELECT COUNT(*) FROM " & rutaBD & ".lotes WHERE carga = d.id) AS avance, b.correos, c.id AS idalerta, c.evento AS tipoalerta, c.resolucion_mensaje, c.cancelacion_mensaje, c.acumular, c.mensaje, c.titulo, i.inicio AS inicio_atencion, NOW() AS inicio_reporte, d.estatus, i.repeticiones, i.fase, i.escalamientos1, i.escalamientos2, i.escalamientos3, i.escalamientos4, i.escalamientos5 FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c ON a.alerta = c.id INNER JOIN " & rutaBD & ".alarmas i ON a.alarma = i.id LEFT JOIN " & rutaBD & ".cargas d ON a.proceso = d.id LEFT JOIN " & rutaBD & ".cat_maquinas b1 ON d.equipo = b1.id AND b1.estatus = 'A' LEFT JOIN " & rutaBD & ".cat_procesos c1 ON b1.proceso = c1.id AND c1.estatus = 'A' WHERE a.id = " & elmensaje!id
                                End If
                            End If

                            Dim mensajeCorreo As String = ""
                            registroDS = consultaSEL(cadSQL)
                            Dim linCorreo = 0
                            If registroDS.Tables(0).Rows.Count > 0 Then
                                canales = ValNull(registroDS.Tables(0).Rows(0)!correos, "A")
                                If canales.Length > 0 Then
                                    generarMensaje = True
                                    For Each elCorreo In registroDS.Tables(0).Rows
                                        linCorreo = linCorreo + 1
                                        nroReporte = elCorreo!proceso
                                        generarMensaje = True
                                        If elCorreo!tipoalerta < 300 Then
                                            laLinea = ValNull(elCorreo!nlinea, "A")
                                            laMaquina = ValNull(elCorreo!nmaquina, "A")
                                            laArea = ValNull(elCorreo!narea, "A")
                                            laFalla = ValNull(elCorreo!nfalla, "A")
                                        Else
                                            laLinea = ""
                                            laMaquina = ""
                                            laArea = ""
                                            laFalla = ""
                                        End If
                                        If elCorreo!tipoalerta = 101 Or elCorreo!tipoalerta = 201 Or elCorreo!tipoalerta = 301 Then
                                            fecha = elCorreo!fecha
                                        ElseIf elCorreo!tipoalerta = 102 Or elCorreo!tipoalerta = 202 Or elCorreo!tipoalerta > 300 Then
                                            fecha = elCorreo!inicio_atencion
                                        ElseIf elCorreo!tipoalerta = 103 Or elCorreo!tipoalerta = 203 Then
                                            fecha = elCorreo!inicio_reporte
                                        End If
                                        tiempo = calcularTiempoCad(DateAndTime.DateDiff(DateInterval.Second, fecha, Now))
                                        If elmensaje!cuenta <= 1 Then eTitulo = ValNull(elCorreo!titulo, "A")
                                        If elCorreo!tipo = 0 Then
                                            eMensaje = ValNull(elCorreo!mensaje, "A")
                                        ElseIf elCorreo!tipo = 8 Then
                                            eMensaje = ValNull(elCorreo!resolucion_mensaje, "A")
                                        ElseIf elCorreo!tipo = 7 Then
                                            eMensaje = ValNull(elCorreo!cancelacion_mensaje, "A")

                                        Else
                                            eMensaje = ValNull(elCorreo!mensaje, "A")
                                        End If
                                        If elmensaje!cuenta <= 1 Then eTitulo = eMensaje
                                        If eTitulo.Length = 0 Then
                                            eTitulo = ValNull(elCorreo!titulo, "A")
                                        End If
                                        If eTitulo.Length > 0 Then
                                            eTitulo = Replace(eTitulo, "[0]", nroReporte)
                                            eTitulo = Replace(eTitulo, "[1]", laLinea)
                                            eTitulo = Replace(eTitulo, "[2]", laMaquina)
                                            eTitulo = Replace(eTitulo, "[3]", laArea)
                                            eTitulo = Replace(eTitulo, "[4]", laFalla)
                                            eTitulo = Replace(eTitulo, "[5]", Format(fecha, "ddd, dd-MMM-yyyy HH:mm"))
                                            eTitulo = Replace(eTitulo, "[11]", tiempo)
                                            If elCorreo!tipoalerta <= 300 Then
                                                eTitulo = Replace(eTitulo, "[12]", Format(elCorreo!rate, "0") & "%")
                                                eTitulo = Replace(eTitulo, "[13]", Format(elCorreo!oee, "0") & "%")
                                            End If

                                            If ValNull(elCorreo!repeticiones, "N") > 0 Then
                                                eTitulo = Replace(eTitulo, "[20]", "Repetición " & ValNull(elCorreo!repeticiones, "N"))
                                            Else
                                                eTitulo = Replace(eTitulo, "[20]", "")
                                            End If
                                            If ValNull(elCorreo!fase - 10, "N") > 0 Then
                                                Dim escala = ValNull(elCorreo!fase, "N") - 10
                                                eTitulo = Replace(eTitulo, "[30]", "Escalado al Nivel " & If(escala > 0, escala, 0))
                                            Else
                                                eTitulo = Replace(eTitulo, "[30]", "")
                                            End If
                                            If ValNull(elCorreo!escalamientos1, "N") > 0 Then
                                                eTitulo = Replace(eTitulo, "[31]", "Repetición " & ValNull(elCorreo!escalamientos1, "N"))
                                            Else
                                                eTitulo = Replace(eTitulo, "[31]", "")
                                            End If
                                            If ValNull(elCorreo!escalamientos2, "N") > 0 Then
                                                eTitulo = Replace(eTitulo, "[32]", "Repetición " & ValNull(elCorreo!escalamientos2, "N"))
                                            Else
                                                eTitulo = Replace(eTitulo, "[32]", "")
                                            End If
                                            If ValNull(elCorreo!escalamientos3, "N") > 0 Then
                                                eTitulo = Replace(eTitulo, "[33]", "Repetición " & ValNull(elCorreo!escalamientos3, "N"))
                                            Else
                                                eTitulo = Replace(eTitulo, "[33]", "")
                                            End If
                                            If ValNull(elCorreo!escalamientos4, "N") > 0 Then
                                                eTitulo = Replace(eTitulo, "[34]", "Repetición " & ValNull(elCorreo!escalamientos4, "N"))
                                            Else
                                                eTitulo = Replace(eTitulo, "[34]", "")
                                            End If
                                            If ValNull(elCorreo!escalamientos5, "N") > 0 Then
                                                eTitulo = Replace(eTitulo, "[35]", "Repetición " & ValNull(elCorreo!escalamientos5, "N"))
                                            Else
                                                eTitulo = Replace(eTitulo, "[35]", "")
                                            End If
                                            eTitulo = Replace(eTitulo, "[90]", "")
                                            eTitulo = Replace(eTitulo, System.Environment.NewLine, " ")

                                            If elmensaje!evento = 301 Then
                                                elLote = eTitulo = Replace(eTitulo, "[41]", ValNull(elCorreo!nlote, "A"))
                                                eTitulo = Replace(eTitulo, "[41]", ValNull(elCorreo!nlote, "A"))
                                                eTitulo = Replace(eTitulo, "[42]", ValNull(elCorreo!referencia, "A"))
                                                eTitulo = Replace(eTitulo, "[43]", ValNull(elCorreo!producto, "A"))
                                                eTitulo = Replace(eTitulo, "[70]", ValNull(elCorreo!ruta_antes, "A"))
                                                eTitulo = Replace(eTitulo, "[71]", ValNull(elCorreo!ruta_secuencia_antes, "A"))
                                                eTitulo = Replace(eTitulo, "[72]", ValNull(elCorreo!ruta_despues, "A"))
                                                eTitulo = Replace(eTitulo, "[73]", ValNull(elCorreo!ruta_secuencia, "A"))

                                            ElseIf elmensaje!evento = 302 Or elmensaje!evento = 305 Then
                                                eTitulo = Replace(eTitulo, "[41]", ValNull(elCorreo!nlote, "A"))
                                                eTitulo = Replace(eTitulo, "[42]", ValNull(elCorreo!referencia, "A"))
                                                eTitulo = Replace(eTitulo, "[43]", ValNull(elCorreo!producto, "A"))
                                                eTitulo = Replace(eTitulo, "[40]", ValNull(elCorreo!nproceso, "A"))
                                                eTitulo = Replace(eTitulo, "[44]", ValNull(elCorreo!ruta_actual, "A"))
                                                eTitulo = Replace(eTitulo, "[45]", ValNull(elCorreo!ruta_secuencia, "A"))
                                                eTitulo = Replace(eTitulo, "[50]", Format(elCorreo!fecha, "dd/MMM/yyyy HH:mm:ss"))
                                                eTitulo = Replace(eTitulo, "[51]", Format(elCorreo!hasta, "dd/MMM/yyyy HH:mm:ss"))
                                                eTitulo = Replace(eTitulo, "[52]", calcularTiempoCad(DateAndTime.DateDiff(DateInterval.Second, elCorreo!hasta, DateAndTime.Now)))
                                                eTitulo = Replace(eTitulo, "[83]", calcularTiempoCad(elCorreo!previo))
                                            ElseIf elmensaje!evento = 303 Or elmensaje!evento = 306 Then
                                                eTitulo = Replace(eTitulo, "[41]", ValNull(elCorreo!nlote, "A"))
                                                eTitulo = Replace(eTitulo, "[42]", ValNull(elCorreo!referencia, "A"))
                                                eTitulo = Replace(eTitulo, "[43]", ValNull(elCorreo!producto, "A"))
                                                eTitulo = Replace(eTitulo, "[40]", ValNull(elCorreo!nproceso, "A"))
                                                eTitulo = Replace(eTitulo, "[44]", ValNull(elCorreo!ruta_actual, "A"))
                                                eTitulo = Replace(eTitulo, "[45]", ValNull(elCorreo!ruta_secuencia, "A"))
                                                eTitulo = Replace(eTitulo, "[61]", ValNull(elCorreo!equipo, "A"))
                                                eTitulo = Replace(eTitulo, "[62]", Format(elCorreo!fecha, "dd/MMM/yyyy HH:mm:ss"))
                                                eTitulo = Replace(eTitulo, "[63]", Format(elCorreo!hasta, "dd/MMM/yyyy HH:mm:ss"))
                                                eTitulo = Replace(eTitulo, "[64]", calcularTiempoCad(DateAndTime.DateDiff(DateInterval.Second, elCorreo!hasta, DateAndTime.Now)))
                                                eTitulo = Replace(eTitulo, "[83]", calcularTiempoCad(elCorreo!previo))
                                            ElseIf elmensaje!evento = 304 Or elmensaje!evento = 307 Then
                                                eTitulo = Replace(eTitulo, "[80]", ValNull(elCorreo!carga, "A"))
                                                eTitulo = Replace(eTitulo, "[40]", ValNull(elCorreo!nproceso, "A"))
                                                eTitulo = Replace(eTitulo, "[61]", ValNull(elCorreo!nequipo, "A"))
                                                eTitulo = Replace(eTitulo, "[81]", Format(elCorreo!fecha, "dd/MMM/yyyy HH:mm:ss"))
                                                eTitulo = Replace(eTitulo, "[82]", calcularTiempoCad(DateAndTime.DateDiff(DateInterval.Second, elCorreo!fecha, DateAndTime.Now)))
                                                eTitulo = Replace(eTitulo, "[83]", calcularTiempoCad(elCorreo!previo))
                                                eTitulo = Replace(eTitulo, "[84]", ValNull(elCorreo!texto, "A"))

                                            End If



                                        End If


                                        If eMensaje.Length > 0 Then
                                            eMensaje = Replace(eMensaje, "[0]", nroReporte)
                                            eMensaje = Replace(eMensaje, "[1]", laLinea)
                                            eMensaje = Replace(eMensaje, "[2]", laMaquina)
                                            eMensaje = Replace(eMensaje, "[3]", laArea)
                                            eMensaje = Replace(eMensaje, "[4]", laFalla)
                                            eMensaje = Replace(eMensaje, "[5]", Format(fecha, "dd/MMM/yyyy HH:mm:ss"))
                                            eMensaje = Replace(eMensaje, "[11]", tiempo)
                                            If elCorreo!tipoalerta < 300 Then
                                                eMensaje = Replace(eMensaje, "[12]", Format(elCorreo!rate, "0") & "%")
                                                eMensaje = Replace(eMensaje, "[13]", Format(elCorreo!oee, "0") & "%")
                                            End If
                                            If ValNull(elCorreo!repeticiones, "N") > 0 Then
                                                eMensaje = Replace(eMensaje, "[20]", "Repetición " & ValNull(elCorreo!repeticiones, "N"))
                                            Else
                                                eMensaje = Replace(eMensaje, "[20]", "")
                                            End If
                                            If ValNull(elCorreo!fase - 10, "N") > 0 Then
                                                Dim escala = ValNull(elCorreo!fase, "N") - 10
                                                eMensaje = Replace(eMensaje, "[30]", "Escalado al Nivel " & If(escala > 0, escala, 0))
                                            Else
                                                eMensaje = Replace(eMensaje, "[30]", "")
                                            End If
                                            If ValNull(elCorreo!escalamientos1, "N") > 0 Then
                                                eMensaje = Replace(eMensaje, "[31]", "Repetición " & ValNull(elCorreo!escalamientos1, "N"))
                                            Else
                                                eMensaje = Replace(eMensaje, "[31]", "")
                                            End If
                                            If ValNull(elCorreo!escalamientos2, "N") > 0 Then
                                                eMensaje = Replace(eMensaje, "[32]", "Repetición " & ValNull(elCorreo!escalamientos2, "N"))
                                            Else
                                                eMensaje = Replace(eMensaje, "[32]", "")
                                            End If
                                            If ValNull(elCorreo!escalamientos3, "N") > 0 Then
                                                eMensaje = Replace(eMensaje, "[33]", "Repetición " & ValNull(elCorreo!escalamientos3, "N"))
                                            Else
                                                eMensaje = Replace(eMensaje, "[33]", "")
                                            End If
                                            If ValNull(elCorreo!escalamientos4, "N") > 0 Then
                                                eMensaje = Replace(eMensaje, "[34]", "Repetición " & ValNull(elCorreo!escalamientos4, "N"))
                                            Else
                                                eMensaje = Replace(eMensaje, "[34]", "")
                                            End If
                                            If ValNull(elCorreo!escalamientos5, "N") > 0 Then
                                                eMensaje = Replace(eMensaje, "[35]", "Repetición " & ValNull(elCorreo!escalamientos5, "N"))
                                            Else
                                                eMensaje = Replace(eMensaje, "[35]", "")
                                            End If


                                            If elmensaje!evento = 301 Then
                                                elLote = ValNull(elCorreo!nlote, "A")
                                                eMensaje = Replace(eMensaje, "[41]", ValNull(elCorreo!nlote, "A"))
                                                eMensaje = Replace(eMensaje, "[42]", ValNull(elCorreo!referencia, "A"))
                                                eMensaje = Replace(eMensaje, "[43]", ValNull(elCorreo!producto, "A"))
                                                eMensaje = Replace(eMensaje, "[70]", ValNull(elCorreo!ruta_antes, "A"))
                                                eMensaje = Replace(eMensaje, "[71]", ValNull(elCorreo!ruta_secuencia_antes, "A"))
                                                eMensaje = Replace(eMensaje, "[72]", ValNull(elCorreo!ruta_despues, "A"))
                                                eMensaje = Replace(eMensaje, "[73]", ValNull(elCorreo!ruta_secuencia, "A"))

                                            ElseIf elmensaje!evento = 302 Or elmensaje!evento = 305 Then
                                                elLote = ValNull(elCorreo!nlote, "A")
                                                eMensaje = Replace(eMensaje, "[41]", ValNull(elCorreo!nlote, "A"))
                                                eMensaje = Replace(eMensaje, "[42]", ValNull(elCorreo!referencia, "A"))
                                                eMensaje = Replace(eMensaje, "[43]", ValNull(elCorreo!producto, "A"))
                                                eMensaje = Replace(eMensaje, "[40]", ValNull(elCorreo!nproceso, "A"))
                                                eMensaje = Replace(eMensaje, "[44]", ValNull(elCorreo!ruta_actual, "A"))
                                                eMensaje = Replace(eMensaje, "[45]", ValNull(elCorreo!ruta_secuencia, "A"))
                                                eMensaje = Replace(eMensaje, "[50]", Format(elCorreo!fecha, "dd/MMM/yyyy HH:mm:ss"))
                                                eMensaje = Replace(eMensaje, "[51]", Format(elCorreo!hasta, "dd/MMM/yyyy HH:mm:ss"))
                                                eMensaje = Replace(eMensaje, "[52]", calcularTiempoCad(DateAndTime.DateDiff(DateInterval.Second, elCorreo!hasta, DateAndTime.Now)))
                                                eMensaje = Replace(eMensaje, "[83]", calcularTiempoCad(elCorreo!previo))
                                            ElseIf elmensaje!evento = 303 Or elmensaje!evento = 306 Then
                                                elLote = ValNull(elCorreo!nlote, "A")
                                                eMensaje = Replace(eMensaje, "[41]", ValNull(elCorreo!nlote, "A"))
                                                eMensaje = Replace(eMensaje, "[42]", ValNull(elCorreo!referencia, "A"))
                                                eMensaje = Replace(eMensaje, "[43]", ValNull(elCorreo!producto, "A"))
                                                eMensaje = Replace(eMensaje, "[40]", ValNull(elCorreo!nproceso, "A"))
                                                eMensaje = Replace(eMensaje, "[44]", ValNull(elCorreo!ruta_actual, "A"))
                                                eMensaje = Replace(eMensaje, "[45]", ValNull(elCorreo!ruta_secuencia, "A"))
                                                eMensaje = Replace(eMensaje, "[61]", ValNull(elCorreo!equipo, "A"))
                                                eMensaje = Replace(eMensaje, "[62]", Format(elCorreo!fecha, "dd/MMM/yyyy HH:mm:ss"))
                                                eMensaje = Replace(eMensaje, "[63]", Format(elCorreo!hasta, "dd/MMM/yyyy HH:mm:ss"))
                                                eMensaje = Replace(eMensaje, "[64]", calcularTiempoCad(DateAndTime.DateDiff(DateInterval.Second, elCorreo!hasta, DateAndTime.Now)))
                                                eMensaje = Replace(eMensaje, "[83]", calcularTiempoCad(elCorreo!previo))
                                            ElseIf elmensaje!evento = 304 Or elmensaje!evento = 307 Then
                                                laCarga = ValNull(elCorreo!carga, "A")
                                                eMensaje = Replace(eMensaje, "[80]", ValNull(elCorreo!carga, "A"))
                                                eMensaje = Replace(eMensaje, "[40]", ValNull(elCorreo!nproceso, "A"))
                                                eMensaje = Replace(eMensaje, "[61]", ValNull(elCorreo!nequipo, "A"))
                                                eMensaje = Replace(eMensaje, "[81]", Format(elCorreo!fecha, "dd/MMM/yyyy HH:mm:ss"))
                                                eMensaje = Replace(eMensaje, "[82]", calcularTiempoCad(DateAndTime.DateDiff(DateInterval.Second, elCorreo!fecha, DateAndTime.Now)))
                                                eMensaje = Replace(eMensaje, "[83]", calcularTiempoCad(elCorreo!previo))
                                                eMensaje = Replace(eMensaje, "[84]", ValNull(elCorreo!texto, "A"))

                                            End If


                                        Else
                                            eMensaje = correo_cuerpo
                                        End If
                                        eMensaje = Replace(eMensaje, "[90]", vbCrLf)
                                        If eMensaje.Length = 0 Then
                                            eMensaje = " Este mensaje es enviado desde SIGMA Monitor" & vbCrLf
                                            If elCorreo!tipoalerta = 101 Then
                                                eMensaje = eMensaje & "El reporte " & nroReporte & " ha excedido el tiempo de espera por atención " & tiempo
                                            ElseIf elCorreo!tipoalerta = 102 Then
                                                eMensaje = eMensaje & "El reporte " & nroReporte & " ha excedido el tiempo de la reparación " & tiempo
                                            ElseIf elCorreo!tipoalerta = 103 Then
                                                eMensaje = eMensaje & "El reporte " & nroReporte & " ha excedido el tiempo de espera por la generación del informe " & tiempo
                                            ElseIf elCorreo!tipoalerta = 201 Then
                                                eMensaje = eMensaje & "El equipo " & laMaquina & " presenta bajo rate " & tiempo
                                            ElseIf elCorreo!tipoalerta = 202 Then
                                                eMensaje = eMensaje & "El equipo " & laMaquina & " presenta sobre rate " & tiempo
                                            ElseIf elCorreo!tipoalerta = 203 Then
                                                eMensaje = eMensaje & "El equipo " & laMaquina & " no está detectando piezas" & tiempo
                                            ElseIf elCorreo!tipoalerta = 301 Then
                                                eMensaje = eMensaje & "El lote " & elLote & " presentó un salto de operación" & tiempo
                                            ElseIf elCorreo!tipoalerta = 302 Then
                                                eMensaje = eMensaje & "El lote " & elLote & " presentó tiempo de stock excedido" & tiempo
                                            ElseIf elCorreo!tipoalerta = 303 Then
                                                eMensaje = eMensaje & "El lote " & elLote & " presentó tiempo de proceso excedido" & tiempo
                                            ElseIf elCorreo!tipoalerta = 3043 Then
                                                eMensaje = eMensaje & "La carga de programación " & laCarga & " presentó fecha de promesa excedida" & tiempo
                                            End If
                                            agregarLOG("La alerta " & elCorreo!idalerta & " no tiene un mensaje de correos definido se tomó el mensaje por defecto", nroReporte, 2)
                                        End If
                                        If elmensaje!cuenta > 1 Then
                                            If linCorreo = 1 Then
                                                mensajeCorreo = "Mensaje " & linCorreo & " de " & elmensaje!cuenta & vbCrLf
                                            Else
                                                mensajeCorreo = mensajeCorreo & vbCrLf & vbCrLf & "Mensaje " & linCorreo & " de " & elmensaje!cuenta & vbCrLf
                                            End If
                                        End If
                                        mensajeCorreo = mensajeCorreo & eMensaje & vbCrLf
                                    Next
                                    If eTitulo.Length = 0 Then
                                        eTitulo = correo_titulo
                                    End If
                                    mensajeCorreo = mensajeCorreo & vbCrLf & vbCrLf & correo_firma


                                    If correo_titulo_falla And elmensaje!cuenta <= 1 And laFalla.Length > 0 Then
                                        eTitulo = "SIGMA Monitor v1.0 " & Format(fecha, "dd/MMM/yyyy HH:mm:ss") & " " & laMaquina & " " & laFalla & " (Van: " & tiempo & ")"
                                    End If
                                End If
                            End If

                            If generarMensaje And canales.Length > 0 Then
                                Dim correos As String()
                                Dim tempArray As String()
                                Dim totalItems = 0
                                If canales.Length > 0 Then
                                    Dim arreCanales = canales.Split(New Char() {";"c})
                                    For i = LBound(arreCanales) To UBound(arreCanales)
                                        'Redimensionamos el Array temporal y preservamos el valor  
                                        ReDim Preserve correos(totalItems + i)
                                        correos(totalItems + i) = arreCanales(i)
                                    Next
                                    tempArray = correos
                                    totalItems = correos.Length

                                    Dim x As Integer, y As Integer
                                    Dim z As Integer

                                    For x = 0 To UBound(correos)
                                        z = 0
                                        For y = 0 To UBound(correos) - 1
                                            'Si el elemento del array es igual al array temporal  
                                            If correos(x) = tempArray(z) And y <> x Then
                                                'Entonces Eliminamos el valor duplicado  
                                                correos(y) = ""
                                            End If
                                            z = z + 1
                                        Next y
                                    Next x
                                End If
                                mensajeGenerado = False
                                Dim mail As New MailMessage
                                Try
                                    mail.From = New MailAddress(correo_cuenta)
                                    For i = 0 To UBound(correos)
                                        If correos(i).Length > 0 Then
                                            mail.To.Add(correos(i))
                                        End If
                                    Next i

                                    mail.Subject = eTitulo
                                    mail.Body = mensajeCorreo
                                    smtpServer.Send(mail)
                                    audiosGen = audiosGen + 1
                                    mensajeGenerado = True
                                Catch ex As Exception
                                    audiosNGen = audiosNGen + 1
                                    agregarLOG(ex.Message, 9, nroReporte)
                                Finally
                                    mail.Dispose()
                                End Try
                                cadSQL = "UPDATE " & rutaBD & ".mensajes SET estatus = 'Z', enviada = NOW() WHERE id = " & elmensaje!id
                                If optimizar Then
                                    If elmensaje!cuenta > 1 Then
                                        cadSQL = "UPDATE " & rutaBD & ".mensajes SET estatus = 'Z', enviada = NOW() WHERE canal = 2 AND lista = " & elmensaje!lista & " AND estatus = '" & idProceso & "'"
                                        If mantenerPrioridad Then
                                            cadSQL = cadSQL & " AND prioridad = " & elmensaje!prioridad
                                        End If
                                    End If
                                End If
                                regsAfectados = consultaACT(cadSQL)
                            End If
                        Next
                        cadSQL = "SELECT a.id, a.texto, b.correos FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' WHERE a.alerta = 0 AND a.canal = 2 AND a.estatus = '" & idProceso & "' ORDER BY a.prioridad DESC, a.id"
                        mensajesDS = consultaSEL(cadSQL)
                        generarMensaje = False
                        If mensajesDS.Tables(0).Rows.Count > 0 Then
                            For Each elmensaje In mensajesDS.Tables(0).Rows
                                canales = ValNull(elmensaje!correos, "A")
                                eMensaje = "Se agotó el número de intentos de llamada para el teléfono " & ValNull(elmensaje!texto, "A")
                                If canales.Length > 0 Then
                                    Dim arreCanales = canales.Split(New Char() {";"c})
                                    Dim correos As String()
                                    Dim tempArray As String()
                                    Dim totalItems = 0

                                    For i = LBound(arreCanales) To UBound(arreCanales)
                                        'Redimensionamos el Array temporal y preservamos el valor  
                                        ReDim Preserve correos(totalItems + i)
                                        correos(totalItems + i) = arreCanales(i)
                                    Next
                                    tempArray = correos
                                    totalItems = correos.Length

                                    Dim x As Integer, y As Integer
                                    Dim z As Integer

                                    For x = 0 To UBound(correos)
                                        z = 0
                                        For y = 0 To UBound(correos) - 1
                                            'Si el elemento del array es igual al array temporal  
                                            If correos(x) = tempArray(z) And y <> x Then
                                                'Entonces Eliminamos el valor duplicado  
                                                correos(y) = ""
                                            End If
                                            z = z + 1
                                        Next y
                                    Next x
                                    mensajeGenerado = False
                                    Dim mail As New MailMessage
                                    Try
                                        mail.From = New MailAddress(correo_cuenta)
                                        For i = 0 To UBound(correos)
                                            If correos(i).Length > 0 Then
                                                mail.To.Add(correos(i))
                                            End If
                                        Next i

                                        mail.Subject = eMensaje
                                        mail.Body = eMensaje
                                        smtpServer.Send(mail)
                                        audiosGen = audiosGen + 1
                                        mensajeGenerado = True
                                    Catch ex As Exception
                                        audiosNGen = audiosNGen + 1
                                        agregarLOG(ex.Message, 9, nroReporte)
                                    Finally
                                        mail.Dispose()
                                    End Try
                                End If
                                If mensajeGenerado Then
                                    cadSQL = "UPDATE " & rutaBD & ".mensajes SET estatus = 'Z', enviada = NOW() WHERE id = " & elmensaje!id
                                    regsAfectados = consultaACT(cadSQL)
                                End If
                            Next
                            If audiosGen > 0 Or audiosNGen > 0 Then
                                agregarLOG("Se generaron " & audiosGen & " audio(s) y no se generaron " & audiosNGen & " audio(s)")
                            End If
                        End If
                    End If
                    If audiosGen > 0 Or audiosNGen > 0 Then
                        agregarLOG("Se generaron " & audiosGen & " correo(s) y no se generaron " & audiosNGen & " correo(s)")
                    End If
                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".mensajes SET estatus = 'Z' WHERE canal = 2 AND estatus = '" & idProceso & "'")
                    smtpServer.Dispose()
                End If
            End If
        End If
        Application.Exit()
    End Sub


End Module

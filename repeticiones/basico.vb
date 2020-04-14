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
    Public be_log_activar As Boolean = False
    Public cadenaConexion As String
    Public serialMmcall As String
    Public rutaBD As String = "logisticar"


    Sub Main(argumentos As String())
        If Process.GetProcessesByName _
          (Process.GetCurrentProcess.ProcessName).Length > 1 Then
        ElseIf argumentos.Length = 0 Then
            MsgBox("No se puede iniciar el envío de mensajes a MMCall: Se requiere la cadena de conexión", MsgBoxStyle.Critical, "SIGMA Monitor")
        Else

            'cadenaConexion = argumentos(0)
            cadenaConexion = "server=localhost;user id=root;password=usbw;port=3307;Convert Zero Datetime=True"
            Dim idProceso = Process.GetCurrentProcess.Id
            Dim laLinea As String = ""
            Dim laMaquina As String = ""
            Dim laArea As String = ""
            Dim laFalla As String = ""
            Dim tiempo As String = ""
            Dim nroReporte As Integer = 0
            Dim be_alarmas_mmcall As Boolean = False
            Dim regsAfectados = 0
            Dim tiempo_andon As Integer = 0
            Dim url_mmcall As String
            Dim accion_mmcall As String
            Dim validar_reloj As Boolean = False

            Dim mmCall As String
            Dim audiosGen = 0
            Dim audiosNGen = 0

            Dim cadSQL As String = "SELECT CONCAT(key_number, serial) AS mmcall FROM mmcall.locations"
            Dim reader As DataSet = consultaSEL(cadSQL)
            If reader.Tables(0).Rows.Count > 0 Then
                serialMmcall = ValNull(reader.Tables(0).Rows(0)!mmcall, "A")
            End If

            cadSQL = "SELECT pagers_val, tiempo_andon, url_mmcall, accion_mmcall FROM " & rutaBD & ".configuracion"
            reader = consultaSEL(cadSQL)
            If reader.Tables(0).Rows.Count > 0 Then
                tiempo_andon = ValNull(reader.Tables(0).Rows(0)!tiempo_andon, "N")
                accion_mmcall = ValNull(reader.Tables(0).Rows(0)!accion_mmcall, "A")
                url_mmcall = ValNull(reader.Tables(0).Rows(0)!url_mmcall, "A")
                validar_reloj = ValNull(reader.Tables(0).Rows(0)!pagers_val, "A") = "S"
            End If
            Dim antes As String = "ÃÀÁÄÂÈÉËÊÌÍÏÎÒÓÖÔÙÚÜÛãàáäâèéëêìíïîòóöôùúüûÑñÇç"
            Dim ahora As String = "AAAAAEEEEIIIIOOOOUUUUaaaaaeeeeiiiioooouuuunncc"
            Dim canales As String
            Dim mensajeGenerado = False

            If tiempo_andon > 0 Then
                cadSQL = "SELECT a.id, b.url_mmcall AS url_mmcall_linea, c.nombre AS nmaquina, c.url_mmcall AS url_mmcall_maquina, d.url_mmcall AS url_mmcall_area, e.url_mmcall AS url_mmcall_falla, e.nombre AS nfalla FROM " & rutaBD & ".reportes a INNER JOIN " & rutaBD & ".cat_lineas b ON a.linea = b.id INNER JOIN " & rutaBD & ".cat_maquinas c ON a.maquina = c.id INNER JOIN " & rutaBD & ".cat_areas d ON a.area = d.id INNER JOIN " & rutaBD & ".cat_fallas e ON a.falla_ajustada = e.id WHERE a.estatus = 0 AND TIME_TO_SEC(TIMEDIFF(NOW(), a.mmcall)) >= " & tiempo_andon & ""
                Dim eventos As DataSet = consultaSEL(cadSQL)
                If eventos.Tables(0).Rows.Count > 0 Then

                    For Each evento In eventos.Tables(0).Rows
                        Dim cadMMCALL = ""
                        Dim eMensaje = evento!nmaquina & " " & evento!nfalla
                        For i = 0 To antes.Length - 1
                            eMensaje = Replace(eMensaje, antes(i), ahora(i))
                        Next
                        eMensaje = Replace(eMensaje, ";", " ")
                        eMensaje = Replace(eMensaje, "\", "-")
                        eMensaje = Replace(eMensaje, "/", "-")
                        If accion_mmcall = "S" Then
                            canales = url_mmcall & ";"
                            canales = canales & ValNull(evento!url_mmcall_linea, "A") & ";"
                            canales = canales & ValNull(evento!url_mmcall_maquina, "A") & ";"
                            canales = canales & ValNull(evento!url_mmcall_area, "A") & ";"
                            canales = canales & ValNull(evento!url_mmcall_falla, "A") & ";"
                        Else
                            If ValNull(evento!url_mmcall_falla, "A") <> "" Then
                                canales = ValNull(evento!url_mmcall_falla, "A")
                            ElseIf ValNull(evento!url_mmcall_area, "A") <> "" Then
                                canales = ValNull(evento!url_mmcall_area, "A")
                            ElseIf ValNull(evento!url_mmcall_maquina, "A") <> "" Then
                                canales = ValNull(evento!url_mmcall_maquina, "A")
                            ElseIf ValNull(evento!url_mmcall_linea, "A") <> "" Then
                                canales = ValNull(evento!url_mmcall_linea, "A")
                            Else
                                canales = url_mmcall
                            End If

                        End If
                        If canales.Length > 0 Then
                            Dim mmcalls As String()
                            Dim tempArray As String()
                            Dim totalItems = 0
                            If canales.Length > 0 Then
                                Dim arreCanales = canales.Split(New Char() {";"c})
                                For i = LBound(arreCanales) To UBound(arreCanales)
                                    'Redimensionamos el Array temporal y preservamos el valor  
                                    ReDim Preserve mmcalls(totalItems + i)
                                    mmcalls(totalItems + i) = arreCanales(i)
                                Next
                                tempArray = mmcalls
                                totalItems = mmcalls.Length

                                Dim x As Integer, y As Integer
                                Dim z As Integer

                                For x = 0 To UBound(mmcalls)
                                    z = 0
                                    For y = 0 To UBound(mmcalls) - 1
                                        'Si el elemento del array es igual al array temporal  
                                        If mmcalls(x) = tempArray(z) And y <> x Then
                                            'Entonces Eliminamos el valor duplicado  
                                            mmcalls(y) = ""
                                        End If
                                        z = z + 1
                                    Next y
                                Next x
                                canales = ""
                            End If
                            eMensaje = Microsoft.VisualBasic.Strings.Left(eMensaje, 40).Trim
                            Dim respuestaWS = ""

                            For i = 0 To UBound(mmcalls)
                                If mmcalls(i).Length > 0 Then
                                    'Se valida el reloj
                                    Dim posReloj = Strings.InStr(mmcalls(i), "number=")
                                    Dim iReloj = Strings.Mid(mmcalls(i), posReloj + 7)
                                    If iReloj.Length = 0 Then
                                        If IsNumeric(mmcalls(i)) Then
                                            iReloj = mmcalls(i)
                                        Else
                                            iReloj = "-1"
                                        End If
                                    End If
                                    If iReloj <> "-1" Then
                                        If Not valReloj(iReloj) And validar_reloj Then
                                            agregarLOG("Servicio de MMCall: " & mmcalls(i) & "&message=" & eMensaje & " error: ID sin licencia", nroReporte, 9)
                                            Continue For
                                        Else
                                            If cadMMCALL = "" Then
                                                cadMMCALL = "INSERT INTO mmcall.tasks (location_id, task, message, recipients, status, created) VALUES "
                                            Else
                                                cadMMCALL = cadMMCALL + ","
                                            End If
                                            cadMMCALL = cadMMCALL & "(1, 'page', '" & eMensaje & "', '" & (iReloj + 100) & "', 0, NOW())"

                                        End If
                                    End If

                                    'Try
                                    'If validarURI(mmcalls(i) & "&message=" & eMensaje) Then
                                    'Dim fr As System.Net.HttpWebRequest
                                    'Dim targetURI As New Uri(mmcalls(i) & "&message=" & eMensaje)
                                    '
                                    'fr = DirectCast(HttpWebRequest.Create(targetURI), System.Net.HttpWebRequest)
                                    'If (fr.GetResponse().ContentLength > 0) Then
                                    'Dim str As New System.IO.StreamReader(fr.GetResponse().GetResponseStream())
                                    'respuestaWS = str.ReadToEnd
                                    'Str.Close()
                                    'End If
                                    '   mensajeGenerado = respuestaWS = "success" And Not mensajeGenerado
                                    '  If mensajeGenerado Then
                                    ' audiosGen = audiosGen + 1
                                    'Else
                                    '   audiosNGen = audiosNGen + 1
                                    '  agregarLOG("Servicio de MMCall: " & mmcalls(i) & "&message=" & eMensaje & " error: " & respuestaWS, evento!id, 9)
                                    'End If
                                    'Else
                                    '   audiosNGen = audiosNGen + 1
                                    '  agregarLOG("" & mmcalls(i) & "&message=" & eMensaje & " error: la dirección no es válida", evento!id, 9)
                                    'End If
                                    'Catch ex As System.Net.WebException
                                    'agregarLOG("Servicio de MMCall: " & mmcalls(i) & "&message=" & eMensaje & " error: " & ex.Message, evento!id, 9)
                                    'audiosNGen = audiosNGen + 1
                                    'End Try
                                End If
                            Next
                        End If
                        If cadMMCALL.Length > 0 Then cadMMCALL = cadMMCALL & ";"
                        'regsAfectados = consultaACT(cadMMCALL & "UPDATE " & rutaBD & ".reportes SET mmcall = NOW() WHERE id = " & evento!id)
                        agregarLOG("Se ha enviado una repetición (MMCall) asociado al reporte: " & evento!id)

                    Next
                End If
            End If
        End If
        Application.Exit()
    End Sub
    Private Sub agregarLOG(cadena As String, Optional reporte As Integer = 0, Optional tipo As Integer = 0, Optional aplicacion As Integer = 20)
        If Not be_log_activar Then Exit Sub
        'tipo 0: Info
        'tipo 2: Advertencia
        'tipo 9: Error
        Dim regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".log (aplicacion, tipo, proceso, texto) VALUES (" & aplicacion & ", " & tipo & ", " & reporte & ", '" & Microsoft.VisualBasic.Strings.Left(cadena, 250) & "')")
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
    'cadenaConexion = "server=localhost;user id=root;password=usbw;port=3307;Convert Zero Datetime=True"
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

    Function validarURI(ByVal cadena As String) As Boolean
        Dim validatedUri As System.Uri
        Return Uri.TryCreate(cadena, UriKind.RelativeOrAbsolute, validatedUri)
    End Function

    Function valReloj(id As String) As Boolean
        valReloj = False
        Dim tipo As String = "R"
        Dim claveInterna = "LogisEicarlronoVI201i"

        If id = "" Then
            Exit Function
        End If
        Dim cadSQL As String = "SELECT tipo, mmcall, cronos FROM " & rutaBD & ".licencias WHERE tipo = '" & tipo & "' AND mmcall = '" & id & "'"
        Dim reader As DataSet = consultaSEL(cadSQL)
        If reader.Tables(0).Rows.Count > 0 Then

            Dim temporal = ""
            Dim clavePublica = ""
            Dim buscarEn
            Dim numeroActual = 0
            Dim temporal2 = claveInterna
            Dim numero1
            Dim numero2

            Dim num1 = 0
            Dim num2 = 0
            Dim num3 = 0
            Dim posicion

            If claveInterna.Length > id.Length Then
                temporal = ""
                Dim recorrido = 0
                Do While temporal.Length < claveInterna.Length
                    If recorrido >= serialMmcall.Length Then
                        recorrido = 0
                    End If
                    numero1 = Asc(id(recorrido Mod id.Length))
                    numero2 = serialMmcall(recorrido)
                    If numero1.ToString.Length = 1 Then
                        buscarEn = numero1
                        numeroActual = 0
                        num1 = 0
                        num2 = 0
                        num3 = 0
                    ElseIf numero1.tostring.length = 2 Then

                        num1 = Val(Strings.Mid(numero1.ToString, 1, 1))
                        num2 = Val(Strings.Mid(numero1.ToString, 2))
                        If numeroActual = 0 Then
                            buscarEn = num1
                            numeroActual = 1
                        Else

                            buscarEn = num2
                            numeroActual = 0
                        End If
                        posicion = num1 + num2 + recorrido
                    ElseIf numero1.tostring.length = 3 Then

                        num1 = Val(Strings.Mid(numero1.ToString, 1, 1))
                        num2 = Val(Strings.Mid(numero1.ToString, 2, 1))
                        num3 = Val(Strings.Mid(numero1.ToString, 3))
                        If numeroActual = 0 Then
                            buscarEn = num1
                            numeroActual = 1
                        ElseIf numeroActual = 1 Then
                            buscarEn = num2
                            numeroActual = 2
                        Else
                            buscarEn = num3
                            numeroActual = 0
                        End If
                        posicion = num1 + num2 + num3 + recorrido
                    End If
                    posicion = posicion + Val(numero2)
                    If posicion > serialMmcall.Length - 1 Then
                        posicion = posicion Mod serialMmcall.Length
                    End If
                    temporal = IIf(temporal.Length = 0, reader.Tables(0).Rows(0)!tipo, "") + temporal + serialMmcall(posicion)
                    recorrido = recorrido + 1
                Loop
                temporal = Strings.Mid(temporal, 1, claveInterna.Length)
            ElseIf claveInterna.Length = id.Length Then
                temporal = id
            ElseIf id.Length > claveInterna.Length Then
                temporal = id
                temporal2 = claveInterna
                Do While temporal2.Length < id.Length
                    temporal2 = temporal2 & claveInterna
                Loop
                temporal2 = Strings.Mid(temporal2, 1, id.Length)
            End If
            Dim cadComparar = ""
            For i = 0 To temporal.Length - 1
                Dim numero As String = Asc(temporal(i)) Xor Asc(temporal2(i))
                If (numero.Length = 1) Then
                    cadComparar = numero
                ElseIf (numero.Length = 2) Then
                    cadComparar = Strings.Mid(numero, 2, 1)
                ElseIf (numero.Length = 3) Then
                    cadComparar = Strings.Mid(numero, 2, 1)
                End If
                clavePublica = clavePublica + cadComparar
            Next
            valReloj = True
            For i = 0 To temporal.Length - 1
                Dim numero As String = Asc(clavePublica(i)) Xor Asc(claveInterna(i))
                If (numero.Length = 1) Then
                    cadComparar = numero
                ElseIf (numero.Length = 2) Then
                    cadComparar = Strings.Mid(numero, 2, 1)
                ElseIf (numero.Length = 3) Then
                    cadComparar = Strings.Mid(numero, 2, 1)
                End If
                If cadComparar <> Strings.Mid(reader.Tables(0).Rows(0)!cronos, i + 1, 1) Then
                    valReloj = False
                    Exit Function
                End If
            Next
        End If
    End Function
End Module


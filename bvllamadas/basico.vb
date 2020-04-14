
Imports System.Security.Cryptography
Imports System.Text
Imports System.Globalization
Imports System.Speech.Synthesis
Imports MySql.Data.MySqlClient
Imports System.IO.Ports
Imports System.IO
Imports System.Data
Imports System.Net.Mail
Imports System.Net
Imports System.ComponentModel
Imports NAudio.Wave
Imports NAudio.Wave.SampleProviders


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
            MsgBox("No se puede la generación de archivos de voz: Se requiere la cadena de conexión", MsgBoxStyle.Critical, "SIGMA Monitor")
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
            Dim regsAfectados = 0
            Dim registroDS As DataSet
            Dim voz_audio As String

            Dim canales As String = ""
            Dim laLinea As String = ""
            Dim laMaquina As String = ""
            Dim laArea As String = ""
            Dim mensaje As String = ""
            Dim repeticiones As String = ""
            Dim escalado As String = ""
            Dim audios_ruta As String = ""
            Dim audios_prefijo As String = ""
            Dim audios_escalamiento As Integer

            Dim laFalla As String = ""
            Dim traducir As Boolean = False
            Dim audios_activar As Boolean = False
            Dim be_alarmas_llamadas As Boolean = False
            Dim fecha
            Dim tiempo As String = ""
            Dim nroReporte As Integer = 0

            Dim cadSQL As String = "SELECT * FROM " & rutaBD & ".configuracion"
            Dim UAudio
            Dim readerDS As DataSet = consultaSEL(cadSQL)
            If readerDS.Tables(0).Rows.Count > 0 Then
                Dim reader As DataRow = readerDS.Tables(0).Rows(0)
                optimizar = ValNull(reader!optimizar_llamada, "A") = "S"
                be_log_activar = ValNull(reader!be_log_activar, "A") = "S"
                mantenerPrioridad = ValNull(reader!mantener_prioridad, "A") = "S"
                rutaSMS = ValNull(reader!ruta_audios, "A")
                voz_audio = ValNull(reader!voz_predeterminada, "A")
                traducir = ValNull(reader!traducir, "A") = "S"
                be_alarmas_llamadas = ValNull(reader!be_alarmas_llamadas, "A") = "S"
                be_alarmas_llamadas = ValNull(reader!be_alarmas_llamadas, "A") = "S"
                audios_activar = ValNull(reader!audios_activar, "A") = "S"
                audios_ruta = ValNull(reader!audios_ruta, "A")
                audios_prefijo = ValNull(reader!audios_prefijo, "A")
                mensaje = ValNull(reader!mensaje, "A")
                UAudio = reader!ultimo_audio
                audios_escalamiento = reader!audios_escalamiento
            End If
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
            audios_ruta = Strings.Replace(audios_ruta, "/", "\")

            Dim indiceVoz = 0
            Dim primeraVoz As String
            Dim synthesizer As New SpeechSynthesizer()
            For Each voice In synthesizer.GetInstalledVoices

                indiceVoz = indiceVoz + 1
                Dim info As VoiceInfo
                info = voice.VoiceInfo
                If voz_audio = info.Name Then
                    indiceVoz = -1
                    Exit For
                End If
                If indiceVoz = 1 Then primeraVoz = info.Name
            Next
            If indiceVoz > 0 Then
                agregarLOG("La voz especificada en el archivo de configuración NO esta registrada en el sistema, se tomará la voz por defecto del PC", 0, 9)
                voz_audio = primeraVoz
            ElseIf indiceVoz = 0 Then
                agregarLOG("No se generaron audios para llamadas porque no se encontró alguna voz para reproducir audios en la PC. Por favor revise e intente de nuevo", 0, 9)
            End If
            Dim generarMensaje As Boolean
            If indiceVoz <> 0 Then
                Dim copiarGeneral As Boolean = True


                If be_alarmas_llamadas Then
                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".mensajes SET estatus = '" & idProceso & "' WHERE canal = 0 AND estatus = 'E'")
                    Dim agrupado As Boolean = True
                    If Not optimizar Then
                        cadSQL = "SELECT a.id, a.lista, d.telefonos, d.hora_desde, d.hora_hasta, a.prioridad, z.texto, z.titulo, 1 AS cuenta FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".mensajes_procesados z ON a.id = z.mensaje INNER JOIN " & rutaBD & ".cat_alertas b on a.alerta = b.id INNER JOIN " & rutaBD & ".cat_distribucion d ON a.lista = d.id AND b.estatus = 'A' WHERE a.estatus = '" & idProceso & "' ORDER BY a.prioridad DESC, a.id"
                        agrupado = False
                    ElseIf mantenerPrioridad Then
                        cadSQL = "SELECT a.lista, a.prioridad, b.telefonos, b.hora_desde, b.hora_hasta, COUNT(*) AS cuenta, MAX(a.id) AS id FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".mensajes_procesados z ON a.id = z.mensaje INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c on a.alerta = c.id WHERE a.estatus = '" & idProceso & "' GROUP BY a.prioridad, a.lista, b.telefonos, b.hora_desde, b.hora_hasta ORDER BY prioridad DESC"
                    Else
                        cadSQL = "SELECT a.lista, b.telefonos, 0 AS prioridad, b.hora_desde, b.hora_hasta, COUNT(*) AS cuenta, MAX(a.id) AS id FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".mensajes_procesados z ON a.id = z.mensaje INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c on a.alerta = c.id WHERE a.estatus = '" & idProceso & "' GROUP BY a.lista, b.telefonos, b.hora_desde, b.hora_hasta"

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
                            Dim generarLlamada As Boolean = False
                            If canales.Length > 0 Then
                                If elmensaje!hora_desde.Equals(System.DBNull.Value) And elmensaje!hora_hasta.Equals(System.DBNull.Value) Then
                                    generarLlamada = True
                                ElseIf elmensaje!hora_desde.Equals(System.DBNull.Value) Then
                                    If Format(DateAndTime.Now(), "HH:mm:ss") <= elmensaje!hora_hasta.ToString Then
                                        generarLlamada = True
                                    End If
                                ElseIf elmensaje!hora_hasta.Equals(System.DBNull.Value) Then
                                    If Format(DateAndTime.Now(), "HH:mm:ss") >= elmensaje!hora_desde.ToString Then
                                        generarLlamada = True
                                    End If
                                ElseIf Format(DateAndTime.Now(), "HH:mm:ss") >= elmensaje!hora_desde.ToString And Format(DateAndTime.Now(), "HH:mm:ss") <= elmensaje!hora_hasta.ToString Then
                                    generarLlamada = True
                                End If

                            End If
                            If generarLlamada Then
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
                                mensajeGenerado = False
                                Dim NArchivo = rutaSMS & "\numero_sustituir" & Format(Now, "hhmmss") & "1_1.wav"
                                For i = 0 To UBound(telefonos)
                                    If telefonos(i).Length > 0 Then
                                        Try
                                            Dim synthesizer0 As New SpeechSynthesizer()

                                            synthesizer0.SetOutputToWaveFile(NArchivo)
                                            synthesizer0.SelectVoice(voz_audio)
                                            synthesizer0.Volume = 100 '  // 0...100
                                            synthesizer0.Rate = 0 '     // -10...10
                                            Dim builder2 As New PromptBuilder()
                                            If traducir Then eMensaje = traducirMensaje(eMensaje)
                                            builder2.AppendText(eMensaje)
                                            builder2.Culture = synthesizer0.Voice.Culture
                                            synthesizer0.Speak(builder2)
                                            synthesizer0.SetOutputToDefaultAudioDevice()
                                            mensajeGenerado = True

                                            Exit For
                                        Catch ex As Exception
                                            miError = ex.Message
                                            audiosNGen = audiosNGen + 1
                                        End Try
                                    End If
                                Next
                                If mensajeGenerado Then
                                    For i = 0 To UBound(telefonos)
                                        If telefonos(i).Length > 0 Then
                                            Dim NuevoArchivo = Replace(NArchivo, "numero_sustituir", telefonos(i))
                                            My.Computer.FileSystem.CopyFile(NArchivo, NuevoArchivo)
                                            audiosGen = audiosGen + 1
                                        End If
                                    Next

                                End If
                                My.Computer.FileSystem.DeleteFile(NArchivo)
                            End If
                            cadSQL = "UPDATE " & rutaBD & ".mensajes SET estatus = 'Z', enviada = NOW() WHERE id = " & elmensaje!id
                            If optimizar Then
                                If elmensaje!cuenta > 1 Then
                                    cadSQL = "UPDATE " & rutaBD & ".mensajes SET estatus = 'Z', enviada = NOW() WHERE canal = 0 AND lista = " & elmensaje!lista & " AND estatus = '" & idProceso & "'"
                                    If mantenerPrioridad Then
                                        cadSQL = cadSQL & " AND prioridad = " & elmensaje!prioridad
                                    End If
                                End If
                            End If
                            regsAfectados = consultaACT(cadSQL)
                        Next
                        If audiosGen > 0 Or audiosNGen > 0 Then
                            agregarLOG("Se generaron " & audiosGen & " audio(s) y no se generaron " & audiosNGen & " audio(s)")
                        End If
                    End If

                    cadSQL = "SELECT a.id, a.texto, b.telefonos FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' WHERE a.alerta = 0 AND a.canal = 0 AND a.estatus = '" & idProceso & "' ORDER BY a.prioridad DESC, a.id"
                    mensajesDS = consultaSEL(cadSQL)
                    generarMensaje = False
                    If mensajesDS.Tables(0).Rows.Count > 0 Then
                        For Each elmensaje In mensajesDS.Tables(0).Rows
                            canales = ValNull(elmensaje!telefonos, "A")
                            eMensaje = "Se agotó el número de intentos de llamada para el teléfono " & ValNull(elmensaje!texto, "A")
                            If canales.Length > 0 Then
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
                                mensajeGenerado = False
                                Dim NArchivo = rutaSMS & "\numero_sustituir" & Format(Now, "hhmmss") & "2_1.wav"
                                For i = 0 To UBound(telefonos)
                                    If telefonos(i).Length > 0 Then
                                        Try
                                            Dim synthesizer0 As New SpeechSynthesizer()

                                            synthesizer0.SetOutputToWaveFile(NArchivo)
                                            synthesizer0.SelectVoice(voz_audio)
                                            synthesizer0.Volume = 100 '  // 0...100
                                            synthesizer0.Rate = 0 '     // -10...10
                                            Dim builder2 As New PromptBuilder()
                                            If traducir Then eMensaje = traducirMensaje(eMensaje)
                                            builder2.AppendText(eMensaje)
                                            builder2.Culture = synthesizer0.Voice.Culture
                                            synthesizer0.Speak(builder2)
                                            synthesizer0.SetOutputToDefaultAudioDevice()
                                            mensajeGenerado = True

                                            Exit For
                                        Catch ex As Exception
                                            miError = ex.Message
                                            audiosNGen = audiosNGen + 1
                                        End Try
                                    End If
                                Next
                                If mensajeGenerado Then
                                    For i = 0 To UBound(telefonos)
                                        If telefonos(i).Length > 0 Then
                                            Dim NuevoArchivo = Replace(NArchivo, "numero_sustituir", telefonos(i))
                                            My.Computer.FileSystem.CopyFile(NArchivo, NuevoArchivo)
                                            audiosGen = audiosGen + 1
                                        End If
                                    Next
                                    My.Computer.FileSystem.DeleteFile(NArchivo)
                                End If
                            End If
                            If mensajeGenerado Then
                                cadSQL = "UPDATE " & rutaBD & ".mensajes SET estatus = 'Z' WHERE id = " & elmensaje!id
                                regsAfectados = consultaACT(cadSQL)
                            End If
                        Next
                        If audiosNGen > 0 Then
                            agregarLOG("No se generaron " & audiosNGen & " audio(s)")
                        End If
                        'If audiosGen > 0 Then
                        ' agregarLOG("Se generaron " & audiosGen & " audio(s). Inicia ARDUINO")
                        ' Shell(Application.StartupPath & "\arduino.exe " & Chr(34) & cadenaConexion & Chr(34), AppWinStyle.MinimizedNoFocus)
                        'End If
                    End If
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

    Private Sub agregarLOG(cadena As String, Optional reporte As Integer = 0, Optional tipo As Integer = 0, Optional aplicacion As Integer = 70)
        If Not be_log_activar Then Exit Sub
        'tipo 0: Info
        'tipo 2: Advertencia
        'tipo 9: Error
        Dim regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".log (aplicacion, tipo, proceso, texto) VALUES (" & aplicacion & ", " & tipo & ", " & reporte & ", '" & Microsoft.VisualBasic.Strings.Left(cadena, 250) & "')")
    End Sub
    Function traducirMensaje(mensaje As String) As String
        traducirMensaje = mensaje
        Dim cadSQL As String = ""
        Dim cadCanales As String = ValNull(mensaje, "A")
        If cadCanales.Length > 0 Then
            'Se busca toda la frase

            cadSQL = "SELECT traduccion FROM " & rutaBD & ".traduccion WHERE literal = '" & cadCanales & "'"
            Dim reader As DataSet = consultaSEL(cadSQL)
            If reader.Tables(0).Rows.Count > 0 Then
                traducirMensaje = ValNull(reader.Tables(0).Rows(0)!traduccion, "A")
            Else
                traducirMensaje = ""
                Dim arreCanales = cadCanales.Split(New Char() {" "c})
                For i = LBound(arreCanales) To UBound(arreCanales)
                    'Redimensionamos el Array temporal y preservamos el valor  
                    cadSQL = "SELECT traduccion FROM " & rutaBD & ".traduccion WHERE literal = '" & arreCanales(i) & "'"
                    Dim reader2 As DataSet = consultaSEL(cadSQL)
                    If reader2.Tables(0).Rows.Count > 0 Then
                        traducirMensaje = traducirMensaje & " " & ValNull(reader2.Tables(0).Rows(0)!traduccion, "A")
                    Else
                        traducirMensaje = traducirMensaje & " " & arreCanales(i)
                    End If
                Next
            End If
        End If


    End Function

    Sub eliminarArchivo(archivo)
        Try

            File.Delete(archivo)
            My.Computer.FileSystem.DeleteFile(archivo)

        Catch ex As Exception

        End Try

    End Sub

    Sub stereo2mono(stereo, mono)
        Dim inputReader = New AudioFileReader(stereo)
        Dim monoFile = New StereoToMonoSampleProvider(inputReader)
        monoFile.LeftVolume = 0.0F
        monoFile.RightVolume = 1.0F
        WaveFileWriter.CreateWaveFile16(mono, monoFile)
    End Sub

    Sub resample(origen, destino)
        Dim reader = New AudioFileReader(origen)
        Dim resampler = New WdlResamplingSampleProvider(reader, 16000)
        WaveFileWriter.CreateWaveFile16(destino, resampler)
        reader.Close()
    End Sub


End Module


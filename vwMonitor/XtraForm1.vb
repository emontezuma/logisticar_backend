Imports DevExpress.XtraEditors
Imports DevExpress.Skins
Imports MySql.Data.MySqlClient
Imports System.Speech.Synthesis
Imports System.IO.Ports
Imports System.IO
Imports System.Text
Imports System.Net.Mail
Imports System.Net.Http
Imports System.Net
Imports System.ComponentModel
Imports NAudio.Wave
Imports NAudio.Wave.SampleProviders

Public Class XtraForm1
    Dim Estado As Integer = 0
    Dim leyendoLog As Boolean = False
    Dim enMonitor As Boolean = False
    Dim procesandoAudios As Boolean = False
    Dim reenviar As Boolean = False
    Dim procesandoEscalamientos As Boolean
    Dim procesandoRepeticiones As Boolean
    Dim estadoPrograma As Boolean
    Dim eSegundos = 0
    Dim MensajeLlamada = ""
    Dim procesandoMensajes As Boolean = False
    Dim enviandoReportes As Boolean = False
    Dim errorCorreos As String
    Dim telefonos As String()
    Dim mmcalls As String()
    Dim losCorreos As String()
    Dim depurando As Boolean = False, primerSensor As Boolean = True
    Dim revisandoSensores As Boolean = False
    Dim incluyeHoyos = False
    Dim be_log_lineas As Integer
    Dim be_log_activar As Boolean = False
    Dim entroReportes As Boolean = False
    Dim idProceso, repetir_mensaje
    Dim filestoDelete(0) As String

    Private Sub XtraForm1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Dim argumentos As String() = Environment.GetCommandLineArgs()
        If Process.GetProcessesByName _
          (Process.GetCurrentProcess.ProcessName).Length > 1 Then
            XtraMessageBox.Show("SIGMA Monitor ya se está ejecutando en este equipo", "Sesión iniciada", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Application.Exit()
            'ElseIf argumentos.Length <= 1 Then
            '    XtraMessageBox.Show("No se puede iniciar el monitor: Se requiere la cadena de conexión", "Sesión iniciada", MessageBoxButtons.OK, MessageBoxIcon.Error)
            '    Application.Exit()
        Else
            idProceso = Process.GetCurrentProcess.Id
            If argumentos.Length > 1 Then
                cadenaConexion = argumentos(1).ToUpper
            End If
            If cadenaConexion = "" Then
                cadenaConexion = "server=localhost;user id=root;password=usbw;port=3307;Convert Zero Datetime=True;Allow User Variables=True"
            Else
                Dim baseCadenaConexion = "server=localhost;user id=root;password=usbw;port=3307;Convert Zero Datetime=True;Allow User Variables=True"
                Dim arreParametros = baseCadenaConexion.Split(New Char() {";"c})
                Dim arreConexion = cadenaConexion.Split(New Char() {";"c})

                If arreConexion.Length > 0 Then
                    cadenaConexion = ""
                    For i = LBound(arreParametros) To UBound(arreParametros)
                        Dim variablesValores1 = arreParametros(i).Split(New Char() {"="c})
                        Dim encontrado As Integer = -1
                        For j = LBound(arreConexion) To UBound(arreConexion)
                            Dim variablesValores2 = arreConexion(j).Split(New Char() {"="c})
                            If variablesValores1(0).ToUpper = variablesValores2(0).ToUpper Then
                                encontrado = j
                                Exit For
                            End If
                        Next
                        If encontrado = -1 Then
                            cadenaConexion = cadenaConexion & arreParametros(i).ToLower & ";"
                        Else
                            cadenaConexion = cadenaConexion & arreConexion(encontrado).ToLower & ";"
                        End If
                    Next
                End If
            End If
            TextEdit1.Text = "Cadena de conexión: " & cadenaConexion
            'actualizarBD()
        End If
        BarStaticItem1.Caption = "Ejecutando desde " & Format(Now(), "dddd, dd-MMM-yyyy HH:mm:ss")
        estadoPrograma = True
    End Sub

    Sub iniciarPantalla()
        Dim regsAfectados As Integer = 0
        ListBoxControl1.Items.Clear()

        'Se escribe en la base de datos
        regsAfectados = consultaACT("UPDATE " & rutaBD & ".configuracion SET ejecutando_desde = NOW()")
        If errorBD.Length > 0 Then
            'Error en la base de datos
            agregarLOG("Ocurrió un error al intentar ejecutar una actualización en la base de datos de " & rutaBD & ". Error: " + errorBD, 9, 0)
        ElseIf regsAfectados = 0 Then
            regsAfectados = consultaACT("INSERT INTO configuracion (ejecutando_desde, revisar_cada) VALUES ('" & Format(horaDesde, "yyyy/MM/dd HH:mm:ss") & "', 60)")
        End If
        BarManager1.Items(0).Caption = "Ejecutandose desde: " + Format(horaDesde, "ddd, dd-MMM-yyyy HH:mm:ss")

    End Sub

    Private Sub XtraForm1_SizeChanged(sender As Object, e As EventArgs) Handles Me.SizeChanged
        ListBoxControl1.Width = Me.Width - 30
        TextEdit1.Width = ListBoxControl1.Width
        TextEdit1.Left = ListBoxControl1.Left
        GroupControl1.Width = ListBoxControl1.Width
        ListBoxControl1.Height = Me.Height - 250
        TextEdit1.Top = ListBoxControl1.Height + ListBoxControl1.Top + 6
        SimpleButton3.Left = Me.Width - SimpleButton3.Width - 20
        SimpleButton2.Left = Me.Width - SimpleButton2.Width - 20

    End Sub

    Private Sub SimpleButton1_Click(sender As Object, e As EventArgs) Handles SimpleButton1.Click
        If XtraMessageBox.Show("El log actual se quitará de la pantalla definitivamente. ¿Desea continuar?", "Inicializar LOG en pantalla", MessageBoxButtons.YesNo, MessageBoxIcon.Question) <> DialogResult.No Then
            Dim totalRegs As Integer = ListBoxControl1.Items.Count
            ListBoxControl1.Items.Clear()
            ListBoxControl1.Items.Add(Format(Now, "dd-MMM-yyyy HH:mm:ss") & ": " + "Se inicializa el LOG a solicitud del usuario. Se eliminan " & totalRegs & " registro(s) del LOG acumulandose desde " & Format(horaDesde, "dd-MMM-yyyy HH:mm:ss"))
            horaDesde = Now
            ContarLOG()
        End If
    End Sub

    Private Sub SimpleButton3_Click(sender As Object, e As EventArgs) Handles SimpleButton3.Click
        autenticado = False
        Dim Forma As New XtraForm2
        Forma.Text = "Detener aplicación"
        Forma.ShowDialog()
        If autenticado Then
            If XtraMessageBox.Show("Esta acción detendrá el envío de alertas. ¿Desea detener el monitor de alertas?", "Detener la aplicación", MessageBoxButtons.YesNo, MessageBoxIcon.Warning) <> DialogResult.No Then
                Estado = 1
                SimpleButton3.Visible = False
                SimpleButton2.Visible = True
                ContextMenuStrip1.Items(1).Enabled = False
                ContextMenuStrip1.Items(2).Enabled = True
                estadoPrograma = False
                agregarLOG("La interfaz ha sido detenida por el usuario: " & usuarioCerrar, 9, 0)
            End If
        End If
    End Sub

    Private Sub SimpleButton2_Click(sender As Object, e As EventArgs) Handles SimpleButton2.Click
        If XtraMessageBox.Show("Esta acción reanudará el envío de alertas. ¿Desea reanudar el monitor de alertas?", "Reanudar la aplicación", MessageBoxButtons.YesNo, MessageBoxIcon.Warning) <> DialogResult.No Then
            Estado = 1
            SimpleButton3.Visible = True
            SimpleButton2.Visible = False
            ContextMenuStrip1.Items(1).Enabled = True
            ContextMenuStrip1.Items(2).Enabled = False
            'enviarCorreos()
            estadoPrograma = True
            agregarLOG("La interfaz ha sido reanudada por un usuario", 9, 0)
        End If
    End Sub

    Private Sub agregarLOG(cadena As String, tipo As Integer, reporte As Integer, Optional aplicacion As Integer = 1)
        If Not be_log_activar Then Exit Sub
        'Se agrega a la base de datos
        'tipo 1: Info
        'tipo 2: Incongruencia en los datos (usuario)
        'tipo 9: Error crítico de Base de datos sigma
        Dim regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".log (aplicacion, tipo, texto) VALUES (0, " & tipo & ", '" & Microsoft.VisualBasic.Strings.Left(cadena, 250) & "')")

    End Sub

    Private Sub ContarLOG()
        If be_log_lineas = 0 Then be_log_lineas = 1000
        If ListBoxControl1.Items.Count > be_log_lineas Then
            For i = ListBoxControl1.Items.Count - 1 To be_log_lineas Step -1
                ListBoxControl1.Items.RemoveAt(i)
            Next
        End If
        BarStaticItem2.Caption = IIf(ListBoxControl1.Items.Count = 0, "Ningún registro en el visor", IIf(ListBoxControl1.Items.Count = 1, "Un registro en el visor", ListBoxControl1.Items.Count & " registros en el visor"))
    End Sub

    Private Sub HyperlinkLabelControl1_Click(sender As Object, e As EventArgs) Handles HyperlinkLabelControl1.Click
        System.Diagnostics.Process.Start("www.mmcallmexico.mx")
    End Sub

    Private Sub ComboBoxEdit2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBoxEdit2.SelectedIndexChanged
        Dim MiFuente As Font = New System.Drawing.Font("Lucida Sans", 9, FontStyle.Regular)

        If ComboBoxEdit2.SelectedIndex = 0 Then

            ListBoxControl1.Font = MiFuente

        ElseIf ComboBoxEdit2.SelectedIndex = 1 Then
            MiFuente = New System.Drawing.Font("Lucida Sans", 6, FontStyle.Regular)
            ListBoxControl1.Font = MiFuente
        ElseIf ComboBoxEdit2.SelectedIndex = 2 Then
            MiFuente = New System.Drawing.Font("Lucida Sans", 7, FontStyle.Regular)
            ListBoxControl1.Font = MiFuente

        ElseIf ComboBoxEdit2.SelectedIndex = 3 Then
            MiFuente = New System.Drawing.Font("Lucida Sans", 11, FontStyle.Regular)
            ListBoxControl1.Font = MiFuente
        ElseIf ComboBoxEdit2.SelectedIndex = 4 Then
            MiFuente = New System.Drawing.Font("Lucida Sans", 13, FontStyle.Regular)
            ListBoxControl1.Font = MiFuente
        ElseIf ComboBoxEdit2.SelectedIndex = 5 Then
            MiFuente = New System.Drawing.Font("Lucida Sans", 15, FontStyle.Regular)
            ListBoxControl1.Font = MiFuente
        End If
    End Sub

    Private Sub revisaFlag_Tick(sender As Object, e As EventArgs) Handles revisaFlag.Tick
        Me.Text = "Monitor de eventos (" & Format(DateAndTime.Now(), "ddd, dd/MMM/yyyy HH:mm:ss") & ")"
        If enMonitor Or Not estadoPrograma Then
            agregarSolo("No entró a la revisión, estaba ocupada")
            Exit Sub
        End If

        validarLicencia()

        horaDesde = Now
        Dim cadSQL As String = "SELECT tiempo_andon, mapa_solicitud, ruta_programa_mapa, be_log_lineas, be_log_activar FROM " & rutaBD & ".configuracion"
        Dim reader As DataSet = consultaSEL(cadSQL)
        Dim regsAfectados = 0
        Dim ruta_programa_mapa As String
        If errorBD.Length > 0 Then
            agregarLOG("No se logró la conexión con MySQL. Error: " + errorBD, 9, 0)

        Else
            repetir_mensaje = ValNull(reader.Tables(0).Rows(0)!tiempo_andon, "N")
            be_log_activar = ValNull(reader.Tables(0).Rows(0)!be_log_activar, "A") = "S"
            be_log_lineas = ValNull(reader.Tables(0).Rows(0)!be_log_lineas, "N")
            ruta_programa_mapa = ValNull(reader.Tables(0).Rows(0)!ruta_programa_mapa, "A")
            If be_log_lineas = 0 Then be_log_lineas = 1000
            If ValNull(reader.Tables(0).Rows(0)!mapa_solicitud, "A") = "S" Then
                Try
                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".configuracion SET mapa_solicitud = 'P'")
                    ruta_programa_mapa = Strings.Replace(ruta_programa_mapa, "/", "\")
                    Shell(ruta_programa_mapa, AppWinStyle.MinimizedNoFocus)

                Catch ex As Exception
                    agregarSolo("Se generó un error al convertir el mapa, revise el log")
                End Try
            ElseIf ValNull(reader.Tables(0).Rows(0)!mapa_solicitud, "A") = "Z" Then
                regsAfectados = consultaACT("UPDATE " & rutaBD & ".configuracion SET mapa_ultimo = NOW(), mapa_solicitud = 'A'")
                agregarSolo("Se ha procesado una presentación de manera satisfacoria")

            End If

        End If
        calcularRevision()
        enMonitor = True
        'revisaFlag.Enabled = False
        revisarEventos()
        cancelarAlertas()
        depurar()
        enMonitor = False
        rev_revisarMensajes()
        rev_llamadas()
        enviar_mensajes()

        'revisaFlag.Enabled = True

    End Sub

    Sub rev_revisarMensajes()
        Dim cadSQL = "SELECT * FROM " & rutaBD & ".requesters WHERE repeticiones > 0 AND NOT isnull(mensaje_mmcall)"
        Dim falla As DataSet = consultaSEL(cadSQL)

        If falla.Tables(0).Rows.Count > 0 Then
            Dim cadMMCALL = ""
            For Each lotes In falla.Tables(0).Rows
                Dim continuar = False
                If lotes!ultima_repeticion.Equals(System.DBNull.Value) Then
                    continuar = True
                ElseIf DateDiff(DateInterval.Second, lotes!ultima_repeticion, Now()) > repetir_mensaje Then
                    continuar = True
                End If
                If continuar Then
                    Dim eMensaje = ValNull(lotes!mensaje_mmcall, "A")
                    Dim antes As String = "ÃÀÁÄÂÈÉËÊÌÍÏÎÒÓÖÔÙÚÜÛãàáäâèéëêìíïîòóöôùúüûÑñÇç"
                    Dim ahora As String = "AAAAAEEEEIIIIOOOOUUUUaaaaaeeeeiiiioooouuuunncc"
                    For i = 0 To antes.Length - 1
                        eMensaje = Replace(eMensaje, antes(i), ahora(i))
                    Next
                    eMensaje = Replace(eMensaje, ";", " ")
                    eMensaje = Replace(eMensaje, "\", "-")
                    eMensaje = Replace(eMensaje, "/", "-")
                    Dim regsAfectados = consultaACT("INSERT INTO mmcall.tasks (location_id, task, message, recipients, status, created) VALUES (1, 'page', '" & eMensaje & "', " & (lotes!pager + 100) & ", 0, NOW());UPDATE " & rutaBD & ".requesters SET repeticiones = repeticiones - 1, ultima_repeticion = NOW() WHERE pager = " & lotes!pager)
                    agregarSolo("Se envió un mensaje de MMCall al pager " & lotes!pager)
                End If
            Next
        End If
    End Sub

    Private Sub rev_llamadas()
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
        Dim rutaSMS2 As String = ""
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
            rutaSMS2 = ValNull(reader!ruta_sms, "A")
            voz_audio = ValNull(reader!voz_predeterminada, "A")
            traducir = ValNull(reader!traducir, "A") = "S"
            be_alarmas_llamadas = ValNull(reader!be_alarmas_llamadas, "A") = "S"
            audios_activar = ValNull(reader!audios_activar, "A") = "S"
            audios_ruta = ValNull(reader!audios_ruta, "A")
            audios_prefijo = ValNull(reader!audios_prefijo, "A")
            mensaje = ValNull(reader!mensaje, "A")
            UAudio = reader!ultimo_audio
            audios_escalamiento = reader!audios_escalamiento
        End If
        If Not audios_activar Then Exit Sub
        If rutaSMS.Length = 0 Then
            rutaSMS = My.Computer.FileSystem.SpecialDirectories.MyDocuments
        Else
            rutaSMS = Strings.Replace(rutaSMS, "/", "\")
        End If
        If rutaSMS2.Length = 0 Then
            rutaSMS2 = My.Computer.FileSystem.SpecialDirectories.MyDocuments
        Else
            rutaSMS2 = Strings.Replace(rutaSMS2, "/", "\")
        End If
        If Not My.Computer.FileSystem.DirectoryExists(rutaSMS) Then
            Try
                My.Computer.FileSystem.CreateDirectory(rutaSMS)
            Catch ex As Exception
                rutaSMS = My.Computer.FileSystem.SpecialDirectories.MyDocuments
            End Try
        End If



        If Not My.Computer.FileSystem.DirectoryExists(rutaSMS2) Then
            Try
                My.Computer.FileSystem.CreateDirectory(rutaSMS2)
            Catch ex As Exception
                rutaSMS2 = My.Computer.FileSystem.SpecialDirectories.MyDocuments
            End Try
        End If


        audios_prefijo = Strings.Replace(audios_prefijo, "/", "\")
        audios_ruta = Strings.Replace(audios_ruta, "/", "\")

        Dim indiceVoz = 0
        Dim copiarGeneral As Boolean = False
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
            Dim numeroUnico = 0
            cadSQL = "SELECT a.*, c.nombre, c.audios_activar, c.audios_ruta, c.audios_prefijo, c.audios_general FROM " & rutaBD & ".requesters a LEFT JOIN " & rutaBD & ".cat_destinos c ON a.destino = c.id WHERE a.repeticiones_audio > 0 AND NOT isnull(a.mensaje)"
            mensajesDS = consultaSEL(cadSQL)
            Dim aBorrar = 0
            If mensajesDS.Tables(0).Rows.Count > 0 Then
                ReDim Preserve filestoDelete(0)
                For Each elmensaje In mensajesDS.Tables(0).Rows


                    Dim audio_unido = ""
                    Dim audioGenerado = ""
                    Dim continuar = False
                    If elmensaje!ultima_repeticion_audio.Equals(System.DBNull.Value) Then
                        continuar = True
                    ElseIf DateDiff(DateInterval.Second, elmensaje!ultima_repeticion_audio, Now()) > repetir_mensaje Then
                        continuar = True
                    End If
                    If continuar Then
                        Dim nombreFile = ""
                        Dim audioTMP = ""
                        Dim audioDEF = ""
                        audioGenerado = ""
                        numeroUnico = numeroUnico + 1
                        mensaje = ValNull(elmensaje!mensaje, "A")
                        Dim telefono As String = ValNull(elmensaje!movil, "A")
                        If ValNull(elmensaje!audios_activar, "A") = "S" Then
                            Dim rutaArea = ValNull(elmensaje!audios_ruta, "A")
                            rutaArea = Strings.Replace(rutaArea, "/", "\")
                            Dim prefijo = ValNull(elmensaje!audios_prefijo, "A")
                            prefijo = Strings.Replace(prefijo, "/", "\")
                            copiarGeneral = elmensaje!audios_general = "S"

                            'Se graba el audio en la ruta de la carpeta



                            If My.Computer.FileSystem.DirectoryExists(rutaArea) Then
                                'Se procesa el audio
                                Try
                                    Dim synthesizer0 As New SpeechSynthesizer()
                                    nombreFile = Format(DateAndTime.Now(), "yyyyMMddHHmmss" & numeroUnico) & ".wav"
                                    audioTMP = rutaArea & "\audio_tmp" & nombreFile
                                    audioDEF = rutaArea & "\audio_def" & nombreFile
                                    synthesizer0.SetOutputToWaveFile(audioTMP)
                                    synthesizer0.SelectVoice(voz_audio)
                                    synthesizer0.Volume = 100 '
                                    synthesizer0.Rate = 0 '    
                                    Dim builder2 As New PromptBuilder()
                                    If traducir Then mensaje = traducirMensaje(mensaje)
                                    builder2.AppendText(mensaje)
                                    builder2.Culture = synthesizer0.Voice.Culture
                                    synthesizer0.Speak(builder2)
                                    synthesizer0.SetOutputToDefaultAudioDevice()
                                    synthesizer0.Dispose()


                                    If prefijo.Length > 0 Then


                                        If My.Computer.FileSystem.DirectoryExists(prefijo) Then

                                            eliminarArchivo(prefijo & "\prefijo_mono.wav")
                                            eliminarArchivo(prefijo & "\prefijo_rs.wav")
                                            eliminarArchivo(prefijo & "\tmp_mono.wav")
                                            eliminarArchivo(prefijo & "\tmp_rs.wav")


                                            prefijo = prefijo & "\prefijo.wav"
                                            'Se aplican los procedimientos necesarios para unificar audios...
                                            'Se pasan de stereo a mono
                                            If My.Computer.FileSystem.FileExists(prefijo) Then
                                                Dim audio_mono = New IO.FileInfo(prefijo).DirectoryName & "\tmp_mono.wav"
                                                Dim audio_rs = New IO.FileInfo(prefijo).DirectoryName & "\tmp_rs.wav"

                                                eliminarArchivo(New IO.FileInfo(prefijo).DirectoryName & "\prefijo_mono.wav")
                                                eliminarArchivo(New IO.FileInfo(prefijo).DirectoryName & "\prefijo_rs.wav")
                                                Dim prefijo_mono = New IO.FileInfo(prefijo).DirectoryName & "\prefijo_mono.wav"
                                                Dim prefijo_rs = New IO.FileInfo(prefijo).DirectoryName & "\prefijo_rs.wav"
                                                Try
                                                    stereo2mono(prefijo, prefijo_mono)
                                                Catch ex As Exception
                                                    prefijo_mono = prefijo
                                                End Try
                                                resample(prefijo_mono, prefijo_rs)

                                                Try
                                                    stereo2mono(audioTMP, audio_mono)
                                                Catch ex As Exception
                                                    audio_mono = audioTMP
                                                End Try


                                                resample(audio_mono, audio_rs)

                                                Dim first = New AudioFileReader(prefijo_rs)

                                                Dim Second = New AudioFileReader(audio_rs)
                                                Dim playlist = New ConcatenatingSampleProvider({first, Second})
                                                WaveFileWriter.CreateWaveFile16(audioDEF, playlist)
                                                Second.Close()
                                                'Se borran todos los archivos previos 
                                                'ReDim Preserve filestoDelete(filestoDelete.Length + 3)
                                                '
                                                'filestoDelete(filestoDelete.Length - 3) = audio_rs
                                                'filestoDelete(filestoDelete.Length - 2) = audio_mono
                                                'filestoDelete(filestoDelete.Length - 1) = audioTMP
                                            Else
                                                My.Computer.FileSystem.CopyFile(audioTMP, audioDEF)
                                                '
                                            End If
                                        Else
                                            My.Computer.FileSystem.CopyFile(audioTMP, audioDEF)
                                        End If
                                    Else
                                        My.Computer.FileSystem.CopyFile(audioTMP, audioDEF)
                                    End If
                                Catch ex As Exception
                                    miError = ex.Message
                                    audiosNGen = audiosNGen + 1

                                End Try

                            End If
                            If copiarGeneral And audios_activar Then
                                Try
                                    File.Copy(audioDEF, audios_ruta & "\audio_def" & elmensaje!nombre & "_" & nombreFile)
                                Catch ex As Exception

                                End Try
                            End If
                        End If
                        If audios_activar Then
                            If My.Computer.FileSystem.DirectoryExists(audios_ruta) Then
                                'Se procesa el audio
                                Try
                                    Dim synthesizer0 As New SpeechSynthesizer()
                                    nombreFile = Format(DateAndTime.Now(), "yyyyMMddHHmmss" & numeroUnico) & ".wav"
                                    audioTMP = audios_ruta & "\audio_tmp" & nombreFile
                                    audioDEF = audios_ruta & "\audio_def" & nombreFile
                                    synthesizer0.SetOutputToWaveFile(audioTMP)
                                    synthesizer0.SelectVoice(voz_audio)
                                    synthesizer0.Volume = 100 '
                                    synthesizer0.Rate = 0 '    
                                    Dim builder2 As New PromptBuilder()
                                    If traducir Then mensaje = traducirMensaje(mensaje)
                                    builder2.AppendText(mensaje)
                                    builder2.Culture = synthesizer0.Voice.Culture
                                    synthesizer0.Speak(builder2)
                                    synthesizer0.SetOutputToDefaultAudioDevice()
                                    synthesizer0.Dispose()


                                    If audios_prefijo.Length > 0 Then


                                        If My.Computer.FileSystem.DirectoryExists(audios_prefijo) Then

                                            eliminarArchivo(audios_prefijo & "\prefijo_mono.wav")
                                            eliminarArchivo(audios_prefijo & "\prefijo_rs.wav")
                                            eliminarArchivo(audios_prefijo & "\tmp_mono.wav")
                                            eliminarArchivo(audios_prefijo & "\tmp_rs.wav")

                                            audios_prefijo = audios_prefijo & "\prefijo.wav"
                                            If My.Computer.FileSystem.FileExists(audios_prefijo) Then

                                                'Se aplican los procedimientos necesarios para unificar audios...
                                                'Se pasan de stereo a mono

                                                Dim audio_mono = New IO.FileInfo(audios_prefijo).DirectoryName & "\tmp_mono.wav"
                                                Dim audio_rs = New IO.FileInfo(audios_prefijo).DirectoryName & "\tmp_rs.wav"

                                                Dim prefijo_mono = New IO.FileInfo(audios_prefijo).DirectoryName & "\prefijo_mono.wav"
                                                Dim prefijo_rs = New IO.FileInfo(audios_prefijo).DirectoryName & "\prefijo_rs.wav"

                                                Try
                                                    stereo2mono(audios_prefijo, prefijo_mono)
                                                Catch ex As Exception
                                                    prefijo_mono = audios_prefijo
                                                End Try

                                                resample(prefijo_mono, prefijo_rs)

                                                Try
                                                    stereo2mono(audioTMP, audio_mono)
                                                Catch ex As Exception
                                                    audio_mono = audioTMP
                                                End Try


                                                resample(audio_mono, audio_rs)

                                                Dim first = New AudioFileReader(prefijo_rs)

                                                Dim Second = New AudioFileReader(audio_rs)
                                                Dim playlist = New ConcatenatingSampleProvider({first, Second})
                                                WaveFileWriter.CreateWaveFile16(audioDEF, playlist)
                                                Second.Close()
                                                'Se borran todos los archivos previos 
                                                'ReDim Preserve filestoDelete(filestoDelete.Length + 3)
                                                '
                                                'filestoDelete(filestoDelete.Length - 3) = audio_rs
                                                'filestoDelete(filestoDelete.Length - 2) = audio_mono
                                                'filestoDelete(filestoDelete.Length - 1) = audioTMP
                                                '
                                            Else
                                                My.Computer.FileSystem.CopyFile(audioTMP, audioDEF)
                                            End If
                                        Else
                                            My.Computer.FileSystem.CopyFile(audioTMP, audioDEF)
                                        End If
                                    Else
                                        My.Computer.FileSystem.CopyFile(audioTMP, audioDEF)

                                    End If
                                    audiosGen = audiosGen + 1
                                Catch ex As Exception
                                    miError = ex.Message
                                    audiosNGen = audiosNGen + 1
                                End Try

                            End If
                        End If

                        If telefono.Length > 0 Then
                            Dim NArchivo = rutaSMS & "\numero_sustituir" & Format(Now, "hhmmss") & "1_1.wav"
                            Try

                                Dim synthesizer0 As New SpeechSynthesizer()

                                synthesizer0.SetOutputToWaveFile(NArchivo)
                                synthesizer0.SelectVoice(voz_audio)
                                synthesizer0.Volume = 100 '  // 0...100
                                synthesizer0.Rate = 0 '     // -10...10
                                Dim builder2 As New PromptBuilder()
                                If traducir Then mensaje = traducirMensaje(mensaje)
                                builder2.AppendText(mensaje)
                                builder2.Culture = synthesizer0.Voice.Culture
                                synthesizer0.Speak(builder2)
                                synthesizer0.SetOutputToDefaultAudioDevice()
                                synthesizer0.Dispose()

                                mensajeGenerado = True
                            Catch ex As Exception
                                miError = ex.Message
                                audiosNGen = audiosNGen + 1
                            End Try
                            Dim NuevoArchivo = Replace(NArchivo, "numero_sustituir", telefono)
                            My.Computer.FileSystem.CopyFile(NArchivo, NuevoArchivo)
                            ReDim Preserve filestoDelete(filestoDelete.Length + 1)
                            filestoDelete(filestoDelete.Length - 1) = NArchivo

                            Try
                                Dim objWriter As New System.IO.StreamWriter(rutaSMS2 & "\" & telefono & Format(Now, "hhmmss") & ".txt", True)
                                objWriter.WriteLine(mensaje)
                                objWriter.Close()
                                audiosGen = audiosGen + 1
                                mensajeGenerado = True
                            Catch ex As Exception
                                audiosNGen = audiosNGen + 1
                                miError = ex.Message
                            End Try

                        End If
                        regsAfectados = consultaACT("UPDATE " & rutaBD & ".requesters SET repeticiones_audio = repeticiones_audio - 1, ultima_repeticion_audio = NOW() WHERE pager = " & elmensaje!pager)
                        agregarSolo("Se envió un audio para el pager " & elmensaje!pager)
                        borrarArchivos.Enabled = True
                    End If
                Next
            End If
        End If


    End Sub



    Private Sub enviar_mensajes()
        If Not estadoPrograma Then Exit Sub
        Dim cadSQL = "SELECT canal FROM " & rutaBD & ".mensajes WHERE estatus = 'E' GROUP BY canal"
        Dim falla As DataSet = consultaSEL(cadSQL)

        If falla.Tables(0).Rows.Count > 0 Then
            For Each lotes In falla.Tables(0).Rows
                If Not estadoPrograma Then Exit Sub
                sinEventos.Enabled = False
                sinEventos.Enabled = True
                Dim AppFuncion = ""
                Try
                    If lotes!canal = 0 Then
                        AppFuncion = "voz.exe"
                        Shell(Application.StartupPath & "\voz.exe " & Chr(34) & cadenaConexion & Chr(34), AppWinStyle.MinimizedNoFocus)
                        agregarSolo("Se inicia la aplicación de generación de audio de llamada")
                    ElseIf lotes!canal = 1 Then
                        AppFuncion = "sms.exe"
                        Shell(Application.StartupPath & "\sms.exe " & Chr(34) & cadenaConexion & Chr(34), AppWinStyle.MinimizedNoFocus)
                        agregarSolo("Se inicia la aplicación de generación de mensajes de texto (SMS)")
                    ElseIf lotes!canal = 2 Then
                        AppFuncion = "mensajes.exe"
                        Shell(Application.StartupPath & "\mensajes.exe " & Chr(34) & cadenaConexion & Chr(34), AppWinStyle.MinimizedNoFocus)
                        agregarSolo("Se inicia la aplicación de generación de correos electrónicos")
                    ElseIf lotes!canal = 3 Then
                        AppFuncion = "mmcall.exe"
                        Shell(Application.StartupPath & "\mmcall.exe " & Chr(34) & cadenaConexion & Chr(34), AppWinStyle.MinimizedNoFocus)
                        agregarSolo("Se inicia la aplicación de mensajes de texto a MMCall")
                    ElseIf lotes!canal = 4 Then
                        AppFuncion = "log.exe"
                        Shell(Application.StartupPath & "\log.exe " & Chr(34) & cadenaConexion & Chr(34), AppWinStyle.MinimizedNoFocus)
                        agregarSolo("Se inicia la aplicación de generación LOGs")
                    End If
                Catch ex As Exception
                    agregarLOG("Error en la ejecución de la aplicación " & AppFuncion & ". Error: " & ex.Message, 7, 0)
                End Try

            Next
        End If

    End Sub

    Private Sub cancelarAlertas()
        Dim regsAfectados = 0
        Dim veces = 0
        Dim pases = 0

        'Alertas ANDON
        Dim cadSQL = "SELECT a.*, c.informar_resolucion, c.evento AS tipoalerta FROM " & rutaBD & ".alarmas a INNER JOIN " & rutaBD & ".cat_alertas c ON a.alerta = c.id INNER JOIN " & rutaBD & ".requesters b ON a.proceso = b.id AND b.alarmado = 'N' WHERE a.estatus <> 9"

        Dim falla As DataSet = consultaSEL(cadSQL)
        If falla.Tables(0).Rows.Count > 0 Then
            For Each lotes In falla.Tables(0).Rows
                regsAfectados = consultaACT("UPDATE " & rutaBD & ".alarmas SET estatus = 9, fin = NOW(), tiempo = TIME_TO_SEC(TIMEDIFF(NOW(), inicio))" & IIf(ValNull(lotes!informar_resolucion, "A") = "S", ", informado = 'S'", "") & " WHERE id = " & lotes!id & ";UPDATE " & rutaBD & ".mensajes SET estatus = 'Z' WHERE alarma = " & lotes!id)
                If ValNull(lotes!informar_resolucion, "A") = "S" Then
                    'Se informa a los involucrados
                    regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, tipo, proceso, alarma, lista) SELECT a.alerta, b.canal, 8, a.proceso, a.id, b.lista FROM " & rutaBD & ".alarmas a INNER JOIN " & rutaBD & ".mensajes b ON a.id = b.alarma WHERE a.id = " & lotes!id & " and a.estatus = 9  GROUP BY a.alerta, b.canal, a.proceso, a.id, b.lista;")
                End If
                agregarLOG("Se ha cerrado la alarma del requester: " & lotes!proceso, 0, lotes!proceso)
            Next
        End If


        crearMensajes()
    End Sub

    Function tiempoValido(desde, hasta, maquina)
        incluyeHoyos = False
        tiempoValido = DateAndTime.DateDiff(DateInterval.Second, desde, hasta)
        If tiempoValido = 0 Then Exit Function
        Dim diaSemana = 0
        Dim fechaEspecifica As Boolean = False
        Dim maquinaEspecifico As Boolean = False
        Dim completado = False
        Dim fechaEstimada = desde
        Dim horaDesde = Format(fechaEstimada, "HH:mm:ss")
        Dim primerDia = True
        Dim MaximoDias = 14
        Dim diasContados = 0
        tiempoValido = 0
        Do While Not completado
            diasContados = diasContados + 1
            Dim rangosPositivosD(0) As String
            Dim rangosPositivosH(0) As String
            Dim rangosNegativosD(0) As String
            Dim rangosNegativosH(0) As String
            Dim rangoPositivo = 0
            Dim rangoNegativo = 0
            horaDesde = Format(fechaEstimada, "HH:mm:ss")
            'Recorrido por día
            diaSemana = DateAndTime.Weekday(fechaEstimada) - 1
            Dim cadSQL = "SELECT desde, hasta, dia, maquina, tipo FROM " & rutaBD & ".horarios WHERE clase = 0 AND (dia = " & diaSemana & " OR (dia = 9 AND fecha = '" & Format(fechaEstimada, "yyyy/MM/dd") & "')) AND (maquina = 0 OR maquina = " & maquina & ") AND hasta > '" & horaDesde & "' ORDER BY tipo DESC, maquina DESC, dia DESC, desde, hasta"
            Dim horarios As DataSet = consultaSEL(cadSQL)
            maquinaEspecifico = False
            fechaEspecifica = False
            Dim segundos = 0
            Dim primerRegistro = True
            Dim holgura = 0
            Dim combinacion = 0
            Dim sumando As Boolean = True
            Dim continuar = False
            If horarios.Tables(0).Rows.Count > 0 Then
                For Each rango In horarios.Tables(0).Rows

                    If primerRegistro Then
                        primerRegistro = False
                        If rango!tipo = "S" Then
                            rangoPositivo = rangoPositivo + 1
                            If fechaEstimada.date = desde.date Then
                                If rango!desde.ToString > horaDesde Then
                                    rangosPositivosD(rangoPositivo - 1) = rango!desde.ToString
                                Else
                                    rangosPositivosD(rangoPositivo - 1) = horaDesde
                                End If
                            Else
                                rangosPositivosD(rangoPositivo - 1) = rango!desde.ToString
                            End If
                            rangosPositivosH(rangoPositivo - 1) = rango!hasta.ToString
                            maquinaEspecifico = rango!maquina <> 0
                            fechaEspecifica = rango!dia = 9
                            If maquinaEspecifico And fechaEspecifica Then
                                combinacion = 1
                            ElseIf maquinaEspecifico Then
                                combinacion = 2
                            ElseIf fechaEspecifica Then
                                combinacion = 3
                            Else
                                combinacion = 4
                            End If
                        End If
                    Else
                        If sumando And rango!tipo = "N" Then
                            sumando = False
                            rangoNegativo = rangoNegativo + 1
                            If fechaEstimada.date = desde.date Then
                                If rango!desde.ToString > horaDesde Then
                                    rangosNegativosD(rangoNegativo - 1) = rango!desde.ToString
                                Else
                                    rangosNegativosD(rangoNegativo - 1) = rango!desde.ToString
                                    rangosNegativosH(rangoNegativo - 1) = horaDesde
                                End If
                            Else
                                rangosNegativosD(rangoNegativo - 1) = rango!desde.ToString
                            End If
                            rangosNegativosH(rangoNegativo - 1) = rango!hasta.ToString
                            maquinaEspecifico = rango!maquina <> 0
                            fechaEspecifica = rango!dia = 9
                            If maquinaEspecifico And fechaEspecifica Then
                                combinacion = 1
                            ElseIf maquinaEspecifico Then
                                combinacion = 2
                            ElseIf fechaEspecifica Then
                                combinacion = 3
                            Else
                                combinacion = 4
                            End If
                        Else
                            If combinacion = 1 Then
                                continuar = rango!maquina <> 0 And rango!dia <> 9
                            ElseIf combinacion = 2 Then
                                continuar = rango!maquina <> 0
                            ElseIf combinacion = 3 Then
                                continuar = rango!dia = 9
                            Else
                                continuar = True
                            End If
                        End If
                        If continuar Then
                            If sumando Then
                                rangoPositivo = rangoPositivo + 1
                                ReDim Preserve rangosPositivosD(rangoPositivo)
                                ReDim Preserve rangosPositivosH(rangoPositivo)
                                rangosPositivosD(rangoPositivo - 1) = rango!desde.ToString
                                rangosPositivosH(rangoPositivo - 1) = rango!hasta.ToString
                            Else
                                rangoNegativo = rangoNegativo + 1
                                ReDim Preserve rangosNegativosD(rangoNegativo)
                                ReDim Preserve rangosNegativosH(rangoNegativo)
                                rangosNegativosD(rangoNegativo - 1) = rango!desde.ToString
                                rangosNegativosH(rangoNegativo - 1) = rango!hasta.ToString
                            End If
                        End If
                    End If
                Next
            End If
            'Se crear un registro único por día
            If rangoPositivo > 0 Then
                Dim arreDefD(0) As String
                Dim arreDefH(0) As String
                Dim arreDefP(0) As String
                Dim totalItems = 1
                arreDefD(0) = rangosPositivosD(0)
                arreDefH(0) = rangosPositivosH(0)
                arreDefP(0) = "S"
                For i = 0 To rangoPositivo - 1
                    If rangosPositivosD(i) > arreDefH(totalItems - 1) Then
                        totalItems = totalItems + 1
                        ReDim Preserve arreDefD(totalItems)
                        ReDim Preserve arreDefH(totalItems)
                        ReDim Preserve arreDefP(totalItems)
                        arreDefD(totalItems - 1) = rangosPositivosD(i)
                        arreDefH(totalItems - 1) = rangosPositivosH(i)
                        arreDefP(totalItems - 1) = "S"
                    ElseIf rangosPositivosH(i) > arreDefH(totalItems - 1) Then
                        arreDefH(totalItems - 1) = rangosPositivosH(i)
                    End If
                Next
                If rangoNegativo > 0 Then
                    For i = 0 To rangoNegativo - 1
                        For j = 0 To totalItems - 1
                            If rangosNegativosD(i) <= arreDefD(j) And rangosNegativosH(i) >= arreDefH(j) Then
                                arreDefP(j) = "N"
                            End If
                            If rangosNegativosD(i) <= arreDefD(j) And rangosNegativosH(i) >= arreDefD(j) Then
                                arreDefD(j) = rangosNegativosH(i)
                            End If
                            If rangosNegativosD(i) >= arreDefD(j) And rangosNegativosD(i) < arreDefH(j) And rangosNegativosH(i) >= arreDefH(j) Then
                                arreDefH(j) = rangosNegativosD(i)
                            End If
                            If rangosNegativosD(i) > arreDefD(j) And rangosNegativosH(i) < arreDefH(j) Then
                                totalItems = totalItems + 1
                                ReDim Preserve arreDefD(totalItems)
                                ReDim Preserve arreDefH(totalItems)
                                ReDim Preserve arreDefP(totalItems)
                                arreDefD(totalItems - 1) = rangosNegativosH(i)
                                arreDefH(totalItems - 1) = arreDefH(j)
                                arreDefP(totalItems - 1) = "S"
                                arreDefH(j) = rangosNegativosD(i)
                            End If
                        Next
                    Next
                End If
                If totalItems > 0 Then
                    Dim swap1 = 0
                    Dim swap2 = 0
                    Dim swap3 = 0
                    For i = 0 To totalItems - 1
                        For j = 0 To totalItems - 2
                            If arreDefD(j) > arreDefD(j + 1) Then
                                swap1 = arreDefD(j)
                                swap2 = arreDefH(j)
                                swap3 = arreDefP(j)
                                arreDefD(j) = arreDefD(j + 1)
                                arreDefH(j) = arreDefH(j + 1)
                                arreDefP(j) = arreDefP(i + 1)
                                arreDefD(j + 1) = swap1
                                arreDefH(j + 1) = swap2
                                arreDefP(j + 1) = swap3
                            End If
                        Next
                    Next

                    Dim tiempoDisponible = 0
                    For i = 0 To totalItems - 1
                        If Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefH(i)) < hasta Then
                            If arreDefP(i) = "S" Then
                                tiempoValido = tiempoValido + DateDiff(DateInterval.Second, Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefD(i)), Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefH(i)))
                            Else
                                incluyeHoyos = True
                            End If
                        ElseIf Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefH(i)) >= hasta Then
                            If arreDefP(i) = "S" Then
                                If Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefD(i)) < hasta Then
                                    tiempoValido = tiempoValido + DateDiff(DateInterval.Second, Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefD(i)), Convert.ToDateTime(Format(hasta, "yyyy/MM/dd HH:mm:ss")))
                                End If

                            Else
                                incluyeHoyos = True
                            End If
                            completado = True
                        End If
                    Next
                    If Not completado Then
                        fechaEstimada = DateAdd(DateInterval.Day, 1, fechaEstimada)
                        fechaEstimada = Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " 00:00:00")
                    End If
                End If
            Else
                If Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " 23:59:59") >= hasta Then
                    completado = True
                Else
                    fechaEstimada = DateAdd(DateInterval.Day, 1, fechaEstimada)
                    fechaEstimada = Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " 00:00:00")
                End If
            End If
            If diasContados > MaximoDias Then completado = True
        Loop
        'If tiempoValido = 0 Then
        ' tiempoValido = DateAndTime.DateDiff(DateInterval.Second, desde, hasta)
        ' End If
    End Function

    Function tiempoValido2(desde, hasta, maquina)
        tiempoValido2 = DateAndTime.DateDiff(DateInterval.Second, desde, hasta)
        incluyeHoyos = False
        If tiempoValido2 = 0 Then Exit Function
        Dim diaSemana = 0
        Dim fechaEspecifica As Boolean = False
        Dim maquinaEspecifico As Boolean = False
        Dim completado = False
        Dim fechaEstimada = desde
        Dim horaDesde = Format(fechaEstimada, "HH:mm:ss")
        Dim primerDia = True
        Dim MaximoDias = 14
        Dim diasContados = 0
        Do While Not completado
            diasContados = diasContados + 1
            Dim rangosPositivosD(0) As String
            Dim rangosPositivosH(0) As String
            Dim rangosNegativosD(0) As String
            Dim rangosNegativosH(0) As String
            Dim rangoPositivo = 0
            Dim rangoNegativo = 0
            horaDesde = Format(fechaEstimada, "HH:mm:ss")
            'Recorrido por día
            diaSemana = DateAndTime.Weekday(fechaEstimada) - 1
            Dim cadSQL = "SELECT desde, hasta, dia, maquina, tipo FROM " & rutaBD & ".horarios WHERE (dia = " & diaSemana & " OR (dia = 9 AND fecha = '" & Format(fechaEstimada, "yyyy/MM/dd") & "')) AND (maquina = 0 OR maquina = " & maquina & ") AND hasta > '" & horaDesde & "' ORDER BY tipo DESC, maquina DESC, dia DESC, desde, hasta"
            Dim horarios As DataSet = consultaSEL(cadSQL)
            maquinaEspecifico = False
            fechaEspecifica = False
            Dim segundos = 0
            Dim primerRegistro = True
            Dim holgura = 0
            Dim combinacion = 0
            Dim sumando As Boolean = True
            Dim continuar = False
            If horarios.Tables(0).Rows.Count > 0 Then
                For Each rango In horarios.Tables(0).Rows

                    If primerRegistro Then
                        primerRegistro = False
                        If rango!tipo = "S" Then
                            rangoPositivo = rangoPositivo + 1
                            If fechaEstimada.date = desde.date Then
                                If rango!desde.ToString > horaDesde Then
                                    rangosPositivosD(rangoPositivo - 1) = rango!desde.ToString
                                Else
                                    rangosPositivosD(rangoPositivo - 1) = horaDesde
                                End If
                            Else
                                rangosPositivosD(rangoPositivo - 1) = rango!desde.ToString
                            End If
                            rangosPositivosH(rangoPositivo - 1) = rango!hasta.ToString
                            maquinaEspecifico = rango!maquina <> 0
                            fechaEspecifica = rango!dia = 9
                            If maquinaEspecifico And fechaEspecifica Then
                                combinacion = 1
                            ElseIf maquinaEspecifico Then
                                combinacion = 2
                            ElseIf fechaEspecifica Then
                                combinacion = 3
                            Else
                                combinacion = 4
                            End If
                        End If
                    Else
                        If sumando And rango!tipo = "N" Then
                            sumando = False
                            rangoNegativo = rangoNegativo + 1
                            If fechaEstimada.date = desde.date Then
                                If rango!desde.ToString > horaDesde Then
                                    rangosNegativosD(rangoNegativo - 1) = rango!desde.ToString
                                Else
                                    rangosNegativosH(rangoNegativo - 1) = horaDesde
                                End If
                            Else
                                rangosNegativosD(rangoNegativo - 1) = rango!desde.ToString
                            End If
                            rangosNegativosH(rangoNegativo - 1) = rango!hasta.ToString
                            maquinaEspecifico = rango!maquina <> 0
                            fechaEspecifica = rango!dia = 9
                            If maquinaEspecifico And fechaEspecifica Then
                                combinacion = 1
                            ElseIf maquinaEspecifico Then
                                combinacion = 2
                            ElseIf fechaEspecifica Then
                                combinacion = 3
                            Else
                                combinacion = 4
                            End If
                        Else
                            If combinacion = 1 Then
                                continuar = rango!maquina <> 0 And rango!dia <> 9
                            ElseIf combinacion = 2 Then
                                continuar = rango!maquina <> 0
                            ElseIf combinacion = 3 Then
                                continuar = rango!dia = 9
                            Else
                                continuar = True
                            End If
                        End If
                        If continuar Then
                            If sumando Then
                                rangoPositivo = rangoPositivo + 1
                                ReDim Preserve rangosPositivosD(rangoPositivo)
                                ReDim Preserve rangosPositivosH(rangoPositivo)
                                rangosPositivosD(rangoPositivo - 1) = rango!desde.ToString
                                rangosPositivosH(rangoPositivo - 1) = rango!hasta.ToString
                            Else
                                rangoNegativo = rangoNegativo + 1
                                ReDim Preserve rangosNegativosD(rangoNegativo)
                                ReDim Preserve rangosNegativosH(rangoNegativo)
                                rangosNegativosD(rangoNegativo - 1) = rango!desde.ToString
                                rangosNegativosH(rangoNegativo - 1) = rango!hasta.ToString
                            End If
                        End If
                    End If
                Next
            End If
            'Se crear un registro único por día
            If rangoPositivo > 0 Then
                Dim arreDefD(0) As String
                Dim arreDefH(0) As String
                Dim arreDefP(0) As String
                Dim totalItems = 1
                arreDefD(0) = rangosPositivosD(0)
                arreDefH(0) = rangosPositivosH(0)
                arreDefP(0) = "S"
                For i = 0 To rangoPositivo - 1
                    If rangosPositivosD(i) > arreDefD(totalItems - 1) Then
                        totalItems = totalItems + 1
                        ReDim Preserve arreDefD(totalItems)
                        ReDim Preserve arreDefH(totalItems)
                        ReDim Preserve arreDefP(totalItems)
                        arreDefD(totalItems - 1) = rangosPositivosD(i)
                        arreDefH(totalItems - 1) = rangosPositivosH(i)
                        arreDefP(totalItems - 1) = "S"
                    ElseIf rangosPositivosH(i) > arreDefH(totalItems - 1) Then
                        arreDefH(totalItems - 1) = rangosPositivosH(i)
                    End If
                Next
                If rangoNegativo > 0 Then
                    For i = 0 To rangoNegativo - 1
                        For j = 0 To totalItems - 1
                            If rangosNegativosD(i) <= arreDefD(j) And rangosNegativosH(i) >= arreDefH(j) Then
                                arreDefP(j) = "N"
                            End If
                            If rangosNegativosD(i) <= arreDefD(j) And rangosNegativosH(i) >= arreDefD(j) Then
                                arreDefD(j) = rangosNegativosH(i)
                            End If
                            If rangosNegativosD(i) >= arreDefD(j) And rangosNegativosD(i) < arreDefH(j) And rangosNegativosH(i) >= arreDefH(j) Then
                                arreDefH(j) = rangosNegativosD(i)
                            End If
                            If rangosNegativosD(i) > arreDefD(j) And rangosNegativosH(i) < arreDefH(j) Then
                                totalItems = totalItems + 1
                                ReDim Preserve arreDefD(totalItems)
                                ReDim Preserve arreDefH(totalItems)
                                ReDim Preserve arreDefP(totalItems)
                                arreDefD(totalItems - 1) = rangosNegativosH(i)
                                arreDefH(totalItems - 1) = arreDefH(j)
                                arreDefP(totalItems - 1) = "S"
                                arreDefH(j) = rangosNegativosD(i)
                            End If
                        Next
                    Next
                End If
                If totalItems > 0 Then
                    Dim swap1 = 0
                    Dim swap2 = 0
                    Dim swap3 = 0
                    For i = 0 To totalItems - 1
                        For j = 0 To totalItems - 2
                            If arreDefD(j) > arreDefD(j + 1) Then
                                swap1 = arreDefD(j)
                                swap2 = arreDefH(j)
                                swap3 = arreDefP(j)
                                arreDefD(j) = arreDefD(j + 1)
                                arreDefH(j) = arreDefH(j + 1)
                                arreDefP(j) = arreDefP(i + 1)
                                arreDefD(j + 1) = swap1
                                arreDefH(j + 1) = swap2
                                arreDefP(j + 1) = swap3
                            End If
                        Next
                    Next


                    tiempoValido2 = 0
                    Dim tiempoDisponible = 0
                    For i = 0 To totalItems - 1
                        If Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefH(i)) <= hasta Then
                            If arreDefP(i) = "S" Then
                                tiempoValido2 = tiempoValido2 + DateDiff(DateInterval.Second, Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefD(i)), Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefH(i)))
                            End If
                        ElseIf Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefH(i)) > hasta Then
                            If arreDefP(i) = "S" Then
                                tiempoValido2 = tiempoValido2 + DateDiff(DateInterval.Second, Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefD(i)), Convert.ToDateTime(Format(hasta, "yyyy/MM/dd HH:mm:ss")))
                            End If
                            completado = True
                        Else
                            incluyeHoyos = True
                        End If
                    Next
                    If Not completado Then
                        fechaEstimada = DateAdd(DateInterval.Day, 1, fechaEstimada)
                        fechaEstimada = Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " 00:00:00")
                    End If
                End If
            Else
                completado = True
                fechaEstimada = DateAdd(DateInterval.Day, 1, fechaEstimada)
                fechaEstimada = Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " 00:00:00")
            End If
            If diasContados > MaximoDias Then completado = True
        Loop
    End Function


    Function calcularFechaEstimada(fecha, tiempoNecesario, proceso)
        calcularFechaEstimada = fecha
        If tiempoNecesario = 0 Then Exit Function
        Dim diaSemana = 0
        Dim fechaEspecifica As Boolean = False
        Dim procesoEspecifico As Boolean = False
        Dim completado = False
        Dim fechaEstimada = fecha
        Dim horaDesde = Format(fechaEstimada, "HH:mm:ss")
        Dim primerDia = True
        Dim MaximoDias = 14
        Dim diasContados = 0
        Do While Not completado
            diasContados = diasContados + 1
            Dim rangosPositivosD(0) As String
            Dim rangosPositivosH(0) As String
            Dim rangosNegativosD(0) As String
            Dim rangosNegativosH(0) As String
            Dim rangoPositivo = 0
            Dim rangoNegativo = 0
            horaDesde = Format(fechaEstimada, "HH:mm:ss")
            'Recorrido por día
            diaSemana = DateAndTime.Weekday(fechaEstimada) - 1
            Dim cadSQL = "SELECT desde, hasta, dia, proceso, tipo FROM " & rutaBD & ".horarios WHERE (dia = " & diaSemana & " OR (dia = 9 AND fecha = '" & Format(fechaEstimada, "yyyy/MM/dd") & "')) AND (proceso = 0 OR proceso = " & proceso & ") AND hasta > '" & horaDesde & "' ORDER BY tipo DESC, proceso DESC, dia DESC, desde, hasta"
            Dim horarios As DataSet = consultaSEL(cadSQL)
            procesoEspecifico = False
            fechaEspecifica = False
            Dim segundos = 0
            Dim primerRegistro = True
            Dim holgura = 0
            Dim combinacion = 0
            Dim sumando As Boolean = True
            Dim continuar = False
            If horarios.Tables(0).Rows.Count > 0 Then
                For Each rango In horarios.Tables(0).Rows

                    If primerRegistro Then
                        primerRegistro = False
                        If rango!tipo = "S" Then
                            rangoPositivo = rangoPositivo + 1
                            If fechaEstimada.date = fecha.date Then
                                If rango!desde.ToString > horaDesde Then
                                    rangosPositivosD(rangoPositivo - 1) = rango!desde.ToString
                                Else
                                    rangosPositivosD(rangoPositivo - 1) = horaDesde
                                End If
                            Else
                                rangosPositivosD(rangoPositivo - 1) = rango!desde.ToString
                            End If
                            rangosPositivosH(rangoPositivo - 1) = rango!hasta.ToString
                            procesoEspecifico = rango!proceso <> 0
                            fechaEspecifica = rango!dia = 9
                            If procesoEspecifico And fechaEspecifica Then
                                combinacion = 1
                            ElseIf procesoEspecifico Then
                                combinacion = 2
                            ElseIf fechaEspecifica Then
                                combinacion = 3
                            Else
                                combinacion = 4
                            End If
                        End If
                    Else
                        If sumando And rango!tipo = "N" Then
                            sumando = False
                            rangoNegativo = rangoNegativo + 1
                            If fechaEstimada.date = fecha.date Then
                                If rango!desde.ToString > horaDesde Then
                                    rangosNegativosD(rangoNegativo - 1) = rango!desde.ToString
                                Else
                                    rangosNegativosH(rangoNegativo - 1) = horaDesde
                                End If
                            Else
                                rangosNegativosD(rangoNegativo - 1) = rango!desde.ToString
                            End If
                            rangosNegativosH(rangoNegativo - 1) = rango!hasta.ToString
                            procesoEspecifico = rango!proceso <> 0
                            fechaEspecifica = rango!dia = 9
                            If procesoEspecifico And fechaEspecifica Then
                                combinacion = 1
                            ElseIf procesoEspecifico Then
                                combinacion = 2
                            ElseIf fechaEspecifica Then
                                combinacion = 3
                            Else
                                combinacion = 4
                            End If
                        Else
                            If combinacion = 1 Then
                                continuar = rango!proceso <> 0 And rango!dia <> 9
                            ElseIf combinacion = 2 Then
                                continuar = rango!proceso <> 0
                            ElseIf combinacion = 3 Then
                                continuar = rango!dia = 9
                            Else
                                continuar = True
                            End If
                        End If
                        If continuar Then
                            If sumando Then
                                rangoPositivo = rangoPositivo + 1
                                ReDim Preserve rangosPositivosD(rangoPositivo)
                                ReDim Preserve rangosPositivosH(rangoPositivo)
                                rangosPositivosD(rangoPositivo - 1) = rango!desde.ToString
                                rangosPositivosH(rangoPositivo - 1) = rango!hasta.ToString
                            Else
                                rangoNegativo = rangoNegativo + 1
                                ReDim Preserve rangosNegativosD(rangoNegativo)
                                ReDim Preserve rangosNegativosH(rangoNegativo)
                                rangosNegativosD(rangoNegativo - 1) = rango!desde.ToString
                                rangosNegativosH(rangoNegativo - 1) = rango!hasta.ToString
                            End If
                        End If
                    End If
                Next
            End If
            'Se crear un registro único por día
            If rangoPositivo > 0 Then
                Dim arreDefD(0) As String
                Dim arreDefH(0) As String
                Dim arreDefP(0) As String
                Dim totalItems = 1
                arreDefD(0) = rangosPositivosD(0)
                arreDefH(0) = rangosPositivosH(0)
                arreDefP(0) = "S"
                For i = 0 To rangoPositivo - 1
                    If rangosPositivosD(i) > arreDefD(totalItems - 1) Then
                        totalItems = totalItems + 1
                        ReDim Preserve arreDefD(totalItems)
                        ReDim Preserve arreDefH(totalItems)
                        ReDim Preserve arreDefP(totalItems)
                        arreDefD(totalItems - 1) = rangosPositivosD(i)
                        arreDefH(totalItems - 1) = rangosPositivosH(i)
                        arreDefP(totalItems - 1) = "S"
                    ElseIf rangosPositivosH(i) > arreDefH(totalItems - 1) Then
                        arreDefH(totalItems - 1) = rangosPositivosH(i)
                    End If
                Next
                If rangoNegativo > 0 Then
                    For i = 0 To rangoNegativo - 1
                        For j = 0 To totalItems - 1
                            If rangosNegativosD(i) <= arreDefD(j) And rangosNegativosH(i) >= arreDefH(j) Then
                                arreDefP(j) = "N"
                            End If
                            If rangosNegativosD(i) <= arreDefD(j) And rangosNegativosH(i) >= arreDefD(j) Then
                                arreDefD(j) = rangosNegativosH(i)
                            End If
                            If rangosNegativosD(i) >= arreDefD(j) And rangosNegativosD(i) < arreDefH(j) And rangosNegativosH(i) >= arreDefH(j) Then
                                arreDefH(j) = rangosNegativosD(i)
                            End If
                            If rangosNegativosD(i) > arreDefD(j) And rangosNegativosH(i) < arreDefH(j) Then
                                totalItems = totalItems + 1
                                ReDim Preserve arreDefD(totalItems)
                                ReDim Preserve arreDefH(totalItems)
                                ReDim Preserve arreDefP(totalItems)
                                arreDefD(totalItems - 1) = rangosNegativosH(i)
                                arreDefH(totalItems - 1) = arreDefH(j)
                                arreDefP(totalItems - 1) = "S"
                                arreDefH(j) = rangosNegativosD(i)
                            End If
                        Next
                    Next
                End If
                If totalItems > 0 Then
                    Dim swap1 = 0
                    Dim swap2 = 0
                    Dim swap3 = 0
                    For i = 0 To totalItems - 1
                        For j = 0 To totalItems - 2
                            If arreDefD(j) > arreDefD(j + 1) Then
                                swap1 = arreDefD(j)
                                swap2 = arreDefH(j)
                                swap3 = arreDefP(j)
                                arreDefD(j) = arreDefD(j + 1)
                                arreDefH(j) = arreDefH(j + 1)
                                arreDefP(j) = arreDefP(i + 1)
                                arreDefD(j + 1) = swap1
                                arreDefH(j + 1) = swap2
                                arreDefP(j + 1) = swap3
                            End If
                        Next
                    Next


                    Dim tiempoDisponible = 0
                    For i = 0 To totalItems - 1
                        If arreDefP(i) = "S" Then
                            horaDesde = arreDefD(i)
                            tiempoDisponible = DateDiff(DateInterval.Second, Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefD(i)), Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefH(i)))
                            If tiempoDisponible > tiempoNecesario Then
                                'Se cubrió
                                completado = True

                                fechaEstimada = DateAdd(DateInterval.Second, tiempoNecesario, fechaEstimada)
                                tiempoNecesario = 0
                            Else
                                tiempoNecesario = tiempoNecesario - tiempoDisponible
                                fechaEstimada = Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " " & arreDefH(i))
                            End If
                        End If
                    Next
                    If Not completado Then
                        fechaEstimada = DateAdd(DateInterval.Day, 1, fechaEstimada)
                        fechaEstimada = Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " 00:00:00")
                    End If
                End If
            Else
                fechaEstimada = DateAdd(DateInterval.Day, 1, fechaEstimada)
                fechaEstimada = Convert.ToDateTime(Format(fechaEstimada, "yyyy/MM/dd") & " 00:00:00")
            End If
            If diasContados > MaximoDias Then completado = True
        Loop
        If tiempoNecesario > 0 Then
            'Es la misma fecha, se debe sumar obligatoriamente
            fechaEstimada = DateAdd(DateInterval.Second, tiempoNecesario, fechaEstimada)
        End If
        calcularFechaEstimada = fechaEstimada

    End Function

    Private Sub revisarEventos()
        'Se alarman los reportes cuyo de llenado ya pasó
        'Dim regsAfectados = consultaACT("UPDATE " & rutabd & ".reportes a, " & rutabd & ".configuracion b SET a.alarmado = 'S' WHERE TIME_TO_SEC(TIMEDIFF(NOW(), a.inicio_reporte)) >= b.tiempo_reporte AND b.tiempo_reporte > 0 AND a.estatus = 100")
        If Not estadoPrograma Then Exit Sub

        Dim cadSQL = "SELECT * FROM " & rutaBD & ".int_eventos WHERE monitor = 'S' AND (ISNULL(revisado) OR TIME_TO_SEC(TIMEDIFF(NOW(), revisado)) >= revision) ORDER BY prioridad"
        Dim eventos As DataSet = consultaSEL(cadSQL)
        If eventos.Tables(0).Rows.Count > 0 Then
            For Each evento In eventos.Tables(0).Rows
                If Not estadoPrograma Then Exit Sub
                If evento!alerta >= 101 And evento!alerta <= 104 Then
                    'Alertas referenciadas a la tabla reportes
                    alertarReporte(evento!alerta)
                    Dim regsAfectados = consultaACT("UPDATE " & rutaBD & ".int_eventos SET revisado = NOW() WHERE alerta = " & evento!alerta)
                End If
            Next
            crearMensajes()
        End If

    End Sub


    Sub alertarReporte(evento As Integer)
        Dim regsAfectados = 0
        Dim veces = 0
        Dim pases = 0
        Dim cadSQL As String = ""
        If evento = 101 Then
            cadSQL = "SELECT a.id, 0 AS lote, (SELECT id FROM " & rutaBD & ".cat_alertas z WHERE (z.linea = 'S' OR a.transporte IN (SELECT detalle FROM " & rutaBD & ".relaciones WHERE operacion = 1 AND indice = z.id)) AND evento = 101 AND estatus = 'A' AND TIME_TO_SEC(TIMEDIFF(NOW(), a.desde)) >= transcurrido ORDER BY prioridad DESC, linea LIMIT 1) AS idalerta FROM " & rutaBD & ".requesters a WHERE a.alarmado = 'N' AND a.estado = 10 AND (a.preasignado = 'N' OR a.preasignado = 'S' AND a.orden = 1) HAVING idalerta > 0 "
        ElseIf evento = 102 Then
            cadSQL = "SELECT a.id, 0 AS lote, (SELECT id FROM " & rutaBD & ".cat_alertas z WHERE (z.linea = 'S' OR a.transporte IN (SELECT detalle FROM " & rutaBD & ".relaciones WHERE operacion = 1 AND indice = z.id)) AND evento = 102 AND estatus = 'A' AND TIME_TO_SEC(TIMEDIFF(NOW(), a.desde)) >= (a.estimado + transcurrido) ORDER BY prioridad DESC, linea LIMIT 1) AS idalerta FROM " & rutaBD & ".requesters a WHERE a.alarmado = 'N' AND a.estado = 20 AND a.monitorear = 'S' AND a.estimado > 0 HAVING idalerta > 0"
        ElseIf evento = 103 Then
            cadSQL = "SELECT a.id, 0 AS lote, (SELECT id FROM " & rutaBD & ".cat_alertas z WHERE (z.linea = 'S' OR a.transporte IN (SELECT detalle FROM " & rutaBD & ".relaciones WHERE operacion = 1 AND indice = z.id)) AND evento = 103 AND estatus = 'A' AND TIME_TO_SEC(TIMEDIFF(NOW(), a.desde)) >= (a.des_estimado + transcurrido) ORDER BY prioridad DESC, linea LIMIT 1) AS idalerta FROM " & rutaBD & ".requesters a WHERE a.alarmado = 'N' AND a.estado = 30 AND a.des_monitorear = 'S' AND a.des_estimado > 0 HAVING idalerta > 0"
        ElseIf evento = 104 Then
            cadSQL = "SELECT a.id, 0 AS lote, (SELECT id FROM " & rutaBD & ".cat_alertas z WHERE (z.linea = 'S' OR a.transporte IN (SELECT detalle FROM " & rutaBD & ".relaciones WHERE operacion = 1 AND indice = z.id)) AND evento = 104 AND estatus = 'A' AND (TIME_TO_SEC(TIMEDIFF(a.fecha_recibo, NOW())) * -1) >= transcurrido ORDER BY prioridad DESC, linea LIMIT 1) AS idalerta FROM " & rutaBD & ".requesters a WHERE a.alarmado = 'N' AND a.estado = 10 AND a.preasignado = 'S' AND a.orden > 0 HAVING idalerta > 0"

        End If
        If Not estadoPrograma Then Exit Sub

        Dim falla As DataSet = consultaSEL(cadSQL)
        If falla.Tables(0).Rows.Count > 0 Then
            For Each lotes In falla.Tables(0).Rows

                If Not estadoPrograma Then Exit Sub
                Dim procesoID = lotes!id
                cadSQL = "SELECT * FROM " & rutaBD & ".cat_alertas WHERE id = " & ValNull(lotes!idalerta, "N")
                Dim alerta As DataSet = consultaSEL(cadSQL)
                Dim uID = 0

                If alerta.Tables(0).Rows.Count > 0 Then

                    Dim idAlerta = alerta.Tables(0).Rows(0)!id
                    Dim fechaDesde
                    Dim crearReporte As Boolean = True
                    Dim solapable As Boolean = False

                    'Se pregunta si hay un rperte activo y si es solapable
                    If ValNull(alerta.Tables(0).Rows(0)!solapar, "A") = "S" Then
                        solapable = True
                    Else
                        cadSQL = "SELECT * FROM " & rutaBD & ".alarmas WHERE alerta = " & idAlerta & " AND estatus <> 9 "
                        Dim solapar As DataSet = consultaSEL(cadSQL)
                        crearReporte = solapar.Tables(0).Rows.Count = 0
                    End If
                    If crearReporte Then
                        Dim porAcumulacion = False

                        If ValNull(alerta.Tables(0).Rows(0)!acumular, "A") = "S" Then
                            crearReporte = False
                            porAcumulacion = True
                            regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".alarmas (alerta, proceso, prioridad) VALUES (" & idAlerta & ", " & procesoID & ", " & alerta.Tables(0).Rows(0)!prioridad & ")")

                            If ValNull(alerta.Tables(0).Rows(0)!acumular_inicializar, "A") = "S" Then
                                If alerta.Tables(0).Rows(0)!acumular_tiempo > 0 Then
                                    fechaDesde = DateAdd(DateInterval.Second, alerta.Tables(0).Rows(0)!acumular_tiempo * -1, Now)
                                    cadSQL = "SELECT COUNT(*) as cuenta FROM " & rutaBD & ".alarmas WHERE alerta = " & idAlerta & " AND estatus = 0 AND inicio >= '" & Format(fechaDesde, "yyyy/MM/dd HH:mm:ss") & "'"
                                Else
                                    cadSQL = "SELECT COUNT(*) as cuenta FROM " & rutaBD & ".alarmas WHERE alerta = " & idAlerta & " AND estatus = 0"
                                End If
                            Else
                                If alerta.Tables(0).Rows(0)!acumular_tiempo > 0 Then
                                    fechaDesde = DateAdd(DateInterval.Second, alerta.Tables(0).Rows(0)!acumular_tiempo * -1, Now)
                                    cadSQL = "SELECT COUNT(*) as cuenta FROM " & rutaBD & ".alarmas WHERE alerta = " & idAlerta & " AND inicio >= '" & Format(fechaDesde, "yyyy/MM/dd HH:mm:ss") & "'"
                                Else
                                    cadSQL = "SELECT COUNT(*) as cuenta FROM " & rutaBD & ".alarmas WHERE alerta = " & idAlerta
                                End If
                            End If

                            Dim acumulado = 0
                            Dim acum As DataSet = consultaSEL(cadSQL)
                            If acum.Tables(0).Rows.Count > 0 Then
                                acumulado = acum.Tables(0).Rows(0)!cuenta
                            End If
                            If acumulado >= alerta.Tables(0).Rows(0)!acumular_veces Then
                                veces = acumulado + 1
                                If alerta.Tables(0).Rows(0)!acumular_tiempo > 0 Then
                                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".alarmas SET acumulada = 'S', activada = NOW(), estatus = 1 WHERE alerta = " & idAlerta & " AND estatus = 0 AND inicio >= '" & Format(fechaDesde, "yyyy/MM/dd HH:mm:ss") & "'")
                                Else
                                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".alarmas SET acumulada = 'S', activada = NOW(), estatus = 1 WHERE alerta = " & idAlerta & " AND estatus = 0")
                                End If
                                crearReporte = True
                            End If
                        Else
                            regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".alarmas (alerta, proceso, prioridad, estatus, activada) VALUES (" & idAlerta & ", " & procesoID & ", " & alerta.Tables(0).Rows(0)!prioridad & ", 1, NOW())")
                        End If

                        If crearReporte Then
                            If ValNull(alerta.Tables(0).Rows(0)!llamada, "A") = "S" Then
                                regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma) SELECT alerta, 0, proceso, " & alerta.Tables(0).Rows(0)!prioridad & ", " & alerta.Tables(0).Rows(0)!lista & ", id FROM " & rutaBD & ".alarmas WHERE alerta = " & idAlerta & " AND fase = 0")
                            End If
                            If ValNull(alerta.Tables(0).Rows(0)!sms, "A") = "S" Then
                                regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma) Select alerta, 1, proceso, " & alerta.Tables(0).Rows(0)!prioridad & ", " & alerta.Tables(0).Rows(0)!lista & ", id FROM " & rutaBD & ".alarmas WHERE alerta = " & idAlerta & " AND fase = 0")
                            End If
                            If ValNull(alerta.Tables(0).Rows(0)!correo, "A") = "S" Then
                                regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma) Select alerta, 2, proceso, " & alerta.Tables(0).Rows(0)!prioridad & ", " & alerta.Tables(0).Rows(0)!lista & ", id FROM " & rutaBD & ".alarmas WHERE alerta = " & idAlerta & " AND fase = 0")
                            End If
                            If ValNull(alerta.Tables(0).Rows(0)!mmcall, "A") = "S" Then
                                regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma) Select alerta, 3, proceso, " & alerta.Tables(0).Rows(0)!prioridad & ", " & alerta.Tables(0).Rows(0)!lista & ", id FROM " & rutaBD & ".alarmas WHERE alerta = " & idAlerta & " AND fase = 0")
                            End If
                            If ValNull(alerta.Tables(0).Rows(0)!log, "A") = "S" Then
                                regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma) Select alerta, 4, proceso, " & alerta.Tables(0).Rows(0)!prioridad & ", " & alerta.Tables(0).Rows(0)!lista & ", id FROM " & rutaBD & ".alarmas WHERE alerta = " & idAlerta & " AND fase = 0")
                            End If
                            regsAfectados = consultaACT("UPDATE " & rutaBD & ".alarmas SET fase = 1 WHERE alerta = " & idAlerta & " AND estatus = 1 And fase = 0")

                            pases = pases + 1
                        End If
                        regsAfectados = consultaACT("UPDATE " & rutaBD & ".requesters Set alarmado = 'S', alarmado_desde = NOW(), alarmas = alarmas + 1 WHERE id = " & procesoID & ";UPDATE " & rutaBD & ".actualizaciones SET dispositivos = NOW()")
                        If evento = 102 Then
                            regsAfectados = consultaACT("UPDATE " & rutaBD & ".movimientos_det Set alarmado = 'S', alarmado_desde = NOW() WHERE requester = " & procesoID & ";UPDATE " & rutaBD & ".actualizaciones SET dispositivos = NOW()")
                        ElseIf evento = 103 Then
                            regsAfectados = consultaACT("UPDATE " & rutaBD & ".movimientos_det Set des_alarmado = 'S', des_alarmado_desde = NOW() WHERE requester = " & procesoID & ";UPDATE " & rutaBD & ".actualizaciones SET dispositivos = NOW()")
                        End If

                    Else
                        agregarLOG("Un evento con el reporte " & procesoID & " no generó alerta por solapamiento en la alerta " & idAlerta, 2, procesoID)
                    End If
                End If
            Next
        End If
        Dim cadAgregar = ""
        If pases > 0 Then
            cadAgregar = "Se alarmó un requester"
            If pases > 1 Then
                cadAgregar = "Se alarmaron " & pases & " requesters"
            End If
        End If

        If pases > 0 Then agregarSolo(cadAgregar)
    End Sub

    Private Sub calcularRevision()
        Dim cadSQL As String = "SELECT tiempo_andon, revisar_cada FROM " & rutaBD & ".configuracion"
        Dim reader As DataSet = consultaSEL(cadSQL)
        If errorBD.Length > 0 Then
            agregarLOG("Ocurrió un error al intentar leer MySQL. Error: " + errorBD, 9, 0)
        Else
            If reader.Tables(0).Rows.Count > 0 Then
                If ValNull(reader.Tables(0).Rows(0)!revisar_cada, "N") = 0 Then
                    eSegundos = 60
                    Dim regsAfectados = consultaACT("UPDATE " & rutaBD & ".configuracion SET revisar_cada = 60")
                    revisaFlag.Interval = 1000
                    revisaFlag.Enabled = False
                    revisaFlag.Enabled = True
                Else
                    eSegundos = ValNull(reader.Tables(0).Rows(0)!revisar_cada, "N")
                    If revisaFlag.Interval <> eSegundos * 1000 Then
                        revisaFlag.Interval = eSegundos * 1000
                        revisaFlag.Enabled = False
                        revisaFlag.Enabled = True
                    End If
                End If

            End If
        End If
        BarManager1.Items(1).Caption = "Conectado (cada " & eSegundos & " segundos)"
    End Sub

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


    Sub escalamientos()

        BarManager1.Items(1).Caption = "Conectado (revisando escalamientos...)"
        procesandoEscalamientos = True
        Dim regsAfectados = 0
        Dim cadSQL = ""


        'Escalada 5
        cadSQL = "SELECT a.*, b.evento, b.tiempo5, b.prioridad, b.lista5, b.escalar5, b.llamada5, sms5, log5, mmcall5, correo5 FROM " & rutaBD & ".alarmas a INNER JOIN " & rutaBD & ".cat_alertas b ON a.alerta = b.id AND b.estatus = 'A' WHERE b.escalar1 <> 'N' AND b.escalar2 <> 'N' AND b.escalar3 <> 'N' AND b.escalar4 <> 'N' AND b.escalar5 <> 'N' AND ((a.estatus = 5) OR (a.estatus >= 5 AND a.estatus < 9 AND b.repetir5 = 'S')) AND (a.escalamientos5 <= b.veces5 OR b.veces5 = 0)"
        Dim alertaDS As DataSet = consultaSEL(cadSQL)
        If alertaDS.Tables(0).Rows.Count > 0 Then

            For Each alerta In alertaDS.Tables(0).Rows
                If Not estadoPrograma Then
                    procesandoEscalamientos = False
                    Exit Sub
                End If
                Dim idAlerta = alerta!alerta
                Dim procesoID = alerta!proceso
                Dim segundos = 0
                Dim activarEscalada As Boolean = False
                Dim uID = alerta!id
                'Se verifica que no se haya repetido antes
                If alerta!escalada5.Equals(System.DBNull.Value) Then
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!escalada4, "yyyy/MM/dd HH:mm:ss")), Now)
                Else
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!escalada5, "yyyy/MM/dd HH:mm:ss")), Now)
                End If
                Dim tiempoCad = ""
                If segundos >= alerta!tiempo5 Then
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!activada, "yyyy/MM/dd HH:mm:ss")), Now)
                    tiempoCad = calcularTiempoCad(segundos)
                    If ValNull(alerta!escalar5, "A") = "T" And alerta!estatus = 5 Then
                        'Se valida si se repite el mesaje para el nivel anterior
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, alarma, lista, tipo, canal, proceso, prioridad) SELECT alerta, alarma, lista, 15, canal, proceso, prioridad FROM " & rutaBD & ".mensajes WHERE alarma = " & alerta!id & " AND tipo <= 4")
                    End If
                    If ValNull(alerta!llamada5, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 0, proceso, prioridad, " & alerta!lista5 & ", id, 5 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!sms5, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 1, proceso, prioridad, " & alerta!lista5 & ", id, 5 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!correo5, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 2, proceso, prioridad, " & alerta!lista5 & ", id, 5 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!mmcall5, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 3, proceso, prioridad, " & alerta!lista5 & ", id, 5 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!log5, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 4, proceso, prioridad, " & alerta!lista5 & ", id, 5 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    agregarLOG("Se han escalado el reporte: " & procesoID & " para el nivel 5", 0, procesoID)
                    Dim cadAdic = "fase = 15, "
                    If alerta!escalamientos5 > 0 Then
                        cadAdic = ""
                    End If
                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".alarmas SET " & cadAdic & "escalada5 = NOW(), escalamientos5 = escalamientos5 + 1, estatus = 6 WHERE id = " & uID)
                End If
            Next
        End If

        cadSQL = "SELECT a.*, b.evento, b.tiempo4, b.prioridad, b.lista4, b.escalar4, b.llamada4, sms4, log4, mmcall4, correo4 FROM " & rutaBD & ".alarmas a INNER JOIN " & rutaBD & ".cat_alertas b ON a.alerta = b.id AND b.estatus = 'A' WHERE b.escalar1 <> 'N' AND b.escalar2 <> 'N' AND b.escalar3 <> 'N' AND b.escalar4 <> 'N' AND ((a.estatus = 4) OR (a.estatus >= 4 AND a.estatus < 9 AND b.repetir4 = 'S')) AND (a.escalamientos4 <= b.veces4 OR b.veces4 = 0)"
        alertaDS = consultaSEL(cadSQL)
        If alertaDS.Tables(0).Rows.Count > 0 Then

            For Each alerta In alertaDS.Tables(0).Rows
                If Not estadoPrograma Then
                    procesandoEscalamientos = False
                    Exit Sub
                End If
                Dim idAlerta = alerta!alerta
                Dim procesoID = alerta!proceso
                Dim segundos = 0
                Dim activarEscalada As Boolean = False
                Dim uID = alerta!id
                'Se verifica que no se haya repetido antes
                If alerta!escalada4.Equals(System.DBNull.Value) Then
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!escalada3, "yyyy/MM/dd HH:mm:ss")), Now)
                Else
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!escalada4, "yyyy/MM/dd HH:mm:ss")), Now)
                End If
                Dim tiempoCad = ""
                If segundos >= alerta!tiempo4 Then
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!activada, "yyyy/MM/dd HH:mm:ss")), Now)
                    tiempoCad = calcularTiempoCad(segundos)
                    If ValNull(alerta!escalar4, "A") = "T" And alerta!estatus = 4 Then
                        'Se valida si se repite el mesaje para el nivel anterior
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, alarma, lista, tipo, canal, proceso, prioridad) SELECT alerta, alarma, lista, 14, canal, proceso, prioridad FROM " & rutaBD & ".mensajes WHERE alarma = " & alerta!id & " AND tipo <= 3")
                    End If
                    If ValNull(alerta!llamada4, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 0, proceso, prioridad, " & alerta!lista4 & ", id, 4 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!sms4, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 1, proceso, prioridad, " & alerta!lista4 & ", id, 4 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!correo4, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 2, proceso, prioridad, " & alerta!lista4 & ", id, 4 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!mmcall4, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 3, proceso, prioridad, " & alerta!lista4 & ", id, 4 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!log4, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 4, proceso, prioridad, " & alerta!lista4 & ", id, 4 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    agregarLOG("Se han escalado el reporte: " & procesoID & " para el nivel 4", 0, procesoID)
                    Dim cadAdic = "fase = 14, "
                    If alerta!escalamientos4 > 0 Then
                        cadAdic = ""
                    End If
                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".alarmas SET " & cadAdic & "escalada4 = NOW(), escalamientos4 = escalamientos4 + 1, estatus = 5 WHERE id = " & alerta!id)
                End If
            Next
        End If

        cadSQL = "SELECT a.*, b.evento, b.tiempo3, b.prioridad, b.lista3, b.escalar3, b.llamada3, sms3, log3, mmcall3, correo3 FROM " & rutaBD & ".alarmas a INNER JOIN " & rutaBD & ".cat_alertas b ON a.alerta = b.id AND b.estatus = 'A' WHERE b.escalar1 <> 'N' AND b.escalar2 <> 'N' AND b.escalar3 <> 'N' AND ((a.estatus = 3) OR (a.estatus >= 3 AND a.estatus < 9 AND b.repetir3 = 'S')) AND (a.escalamientos3 <= b.veces3 OR b.veces3 = 0)"
        alertaDS = consultaSEL(cadSQL)
        If alertaDS.Tables(0).Rows.Count > 0 Then

            For Each alerta In alertaDS.Tables(0).Rows
                If Not estadoPrograma Then
                    procesandoEscalamientos = False
                    Exit Sub
                End If
                Dim idAlerta = alerta!alerta
                Dim procesoID = alerta!proceso
                Dim segundos = 0
                Dim activarEscalada As Boolean = False
                Dim uID = alerta!id
                'Se verifica que no se haya repetido antes
                If alerta!escalada3.Equals(System.DBNull.Value) Then
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!escalada2, "yyyy/MM/dd HH:mm:ss")), Now)
                Else
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!escalada3, "yyyy/MM/dd HH:mm:ss")), Now)
                End If
                Dim tiempoCad = ""
                If segundos >= alerta!tiempo3 Then
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!activada, "yyyy/MM/dd HH:mm:ss")), Now)
                    tiempoCad = calcularTiempoCad(segundos)
                    If ValNull(alerta!escalar3, "A") = "T" And alerta!estatus = 3 Then
                        'Se valida si se repite el mesaje para el nivel anterior
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, alarma, lista, tipo, canal, proceso, prioridad) SELECT alerta, alarma, lista, 13, canal, proceso, prioridad FROM " & rutaBD & ".mensajes WHERE alarma = " & alerta!id & " AND tipo <= 2")
                    End If
                    If ValNull(alerta!llamada3, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 0, proceso, prioridad, " & alerta!lista3 & ", id, 3 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!sms3, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 1, proceso, prioridad, " & alerta!lista3 & ", id, 3 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!correo3, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 2, proceso, prioridad, " & alerta!lista3 & ", id, 3 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!mmcall3, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 3, proceso, prioridad, " & alerta!lista3 & ", id, 3 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!log3, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 4, proceso, prioridad, " & alerta!lista3 & ", id, 3 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    agregarLOG("Se han escalado el reporte: " & procesoID & " para el nivel 3", 0, procesoID)
                    Dim cadAdic = "fase = 13, "
                    If alerta!escalamientos3 > 0 Then
                        cadAdic = ""
                    End If
                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".alarmas SET " & cadAdic & "escalada3 = NOW(), escalamientos3 = escalamientos3 + 1, estatus = 4 WHERE id = " & alerta!id)
                End If
            Next
        End If

        cadSQL = "SELECT a.*, b.evento, b.tiempo2, b.prioridad, b.lista2, b.escalar2, b.llamada2, sms2, log2, mmcall2, correo2 FROM " & rutaBD & ".alarmas a INNER JOIN " & rutaBD & ".cat_alertas b ON a.alerta = b.id AND b.estatus = 'A' WHERE b.escalar1 <> 'N' AND b.escalar2 <> 'N' AND ((a.estatus = 2) OR (a.estatus >= 2 AND a.estatus < 9 AND b.repetir2 = 'S')) AND (a.escalamientos2 <= b.veces2 OR b.veces2 = 0)"
        alertaDS = consultaSEL(cadSQL)
        If alertaDS.Tables(0).Rows.Count > 0 Then

            For Each alerta In alertaDS.Tables(0).Rows
                If Not estadoPrograma Then
                    procesandoEscalamientos = False
                    Exit Sub
                End If
                Dim idAlerta = alerta!alerta
                Dim procesoID = alerta!proceso
                Dim segundos = 0
                Dim activarEscalada As Boolean = False
                Dim uID = alerta!id
                'Se verifica que no se haya repetido antes
                If alerta!escalada2.Equals(System.DBNull.Value) Then
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!escalada1, "yyyy/MM/dd HH:mm:ss")), Now)
                Else
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!escalada2, "yyyy/MM/dd HH:mm:ss")), Now)
                End If
                Dim tiempoCad = ""
                If segundos >= alerta!tiempo2 Then
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!activada, "yyyy/MM/dd HH:mm:ss")), Now)
                    tiempoCad = calcularTiempoCad(segundos)
                    If ValNull(alerta!escalar2, "A") = "T" And alerta!estatus = 2 Then
                        'Se valida si se repite el mesaje para el nivel anterior
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, alarma, lista, tipo, canal, proceso, prioridad) SELECT alerta, alarma, lista, 12, canal, proceso, prioridad FROM " & rutaBD & ".mensajes WHERE alarma = " & alerta!id & " AND tipo <= 1")
                    End If
                    If ValNull(alerta!llamada2, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 0, proceso, prioridad, " & alerta!lista2 & ", id, 2 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!sms2, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 1, proceso, prioridad, " & alerta!lista2 & ", id, 2 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!correo2, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 2, proceso, prioridad, " & alerta!lista2 & ", id, 2 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!mmcall2, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 3, proceso, prioridad, " & alerta!lista2 & ", id, 2 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!log2, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 4, proceso, prioridad, " & alerta!lista2 & ", id, 2 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    agregarLOG("Se han escalado el reporte: " & procesoID & " para el nivel 2", 0, procesoID)
                    Dim cadAdic = "fase = 12, "
                    If alerta!escalamientos2 > 0 Then
                        cadAdic = ""
                    End If
                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".alarmas SET " & cadAdic & "escalada2 = NOW(), escalamientos2 = escalamientos2 + 1, estatus = 3 WHERE id = " & alerta!id)
                End If
            Next
        End If

        cadSQL = "SELECT a.*, b.evento, b.tiempo1, b.prioridad, b.lista1, b.escalar1, b.llamada1, sms1, log1, mmcall1, correo1 FROM " & rutaBD & ".alarmas a INNER JOIN " & rutaBD & ".cat_alertas b ON a.alerta = b.id AND b.estatus = 'A' WHERE b.escalar1 <> 'N' AND ((a.estatus = 1) OR (a.estatus >= 1 AND a.estatus < 9 AND b.repetir1 = 'S')) AND (a.escalamientos1 < b.veces1 OR b.veces1 = 0)"
        alertaDS = consultaSEL(cadSQL)
        If alertaDS.Tables(0).Rows.Count > 0 Then

            For Each alerta In alertaDS.Tables(0).Rows
                If Not estadoPrograma Then
                    procesandoEscalamientos = False
                    Exit Sub
                End If
                Dim idAlerta = alerta!alerta
                Dim procesoID = alerta!proceso
                Dim segundos = 0
                Dim activarEscalada As Boolean = False
                Dim uID = alerta!id
                'Se verifica que no se haya repetido antes
                If alerta!escalada1.Equals(System.DBNull.Value) Then
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!activada, "yyyy/MM/dd HH:mm:ss")), Now)
                Else
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!escalada1, "yyyy/MM/dd HH:mm:ss")), Now)
                End If
                Dim tiempoCad = ""
                If segundos >= alerta!tiempo1 Then
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!activada, "yyyy/MM/dd HH:mm:ss")), Now)
                    tiempoCad = calcularTiempoCad(segundos)
                    If ValNull(alerta!escalar1, "A") = "T" And alerta!estatus = 1 Then
                        'Se valida si se repite el mesaje para el nivel anterior
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, alarma, lista, tipo, canal, proceso, prioridad) SELECT alerta, alarma, lista, 11, canal, proceso, prioridad FROM " & rutaBD & ".mensajes WHERE alarma = " & alerta!id & " AND tipo = 0")
                    End If
                    If ValNull(alerta!llamada1, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 0, proceso, prioridad, " & alerta!lista1 & ", id, 1 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!sms1, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 1, proceso, prioridad, " & alerta!lista1 & ", id, 1 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!correo1, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 2, proceso, prioridad, " & alerta!lista1 & ", id, 1 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!mmcall1, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 3, proceso, prioridad, " & alerta!lista1 & ", id, 1 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!log1, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 4, proceso, prioridad, " & alerta!lista1 & ", id, 1 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    agregarLOG("Se han escalado el reporte: " & procesoID & " para el nivel 1", 0, procesoID)
                    Dim cadAdic = "fase = 11, "
                    If alerta!escalamientos1 > 0 Then
                        cadAdic = ""
                    End If
                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".alarmas SET " & cadAdic & "escalada1 = NOW(), escalamientos1 = escalamientos1 + 1, estatus = 2 WHERE id = " & uID)
                End If
            Next
        End If

        cadSQL = "SELECT a.*, b.evento, b.repetir_tiempo, b.repetir_veces, b.prioridad, b.lista, b.llamada, sms, log, mmcall, correo FROM " & rutaBD & ".alarmas a INNER JOIN " & rutaBD & ".cat_alertas b ON a.alerta = b.id AND b.estatus = 'A' WHERE ((a.estatus = 1 AND b.repetir = 'T') OR (a.estatus >= 1 AND a.estatus < 9 AND b.repetir = 'S')) AND b.repetir_tiempo > 0 AND (a.repeticiones < b.repetir_veces OR b.repetir_veces = 0)"
        alertaDS = consultaSEL(cadSQL)
        If alertaDS.Tables(0).Rows.Count > 0 Then

            For Each alerta In alertaDS.Tables(0).Rows
                If Not estadoPrograma Then
                    procesandoEscalamientos = False
                    Exit Sub
                End If
                Dim idAlerta = alerta!alerta
                Dim procesoID = alerta!proceso
                Dim segundos = 0
                Dim activarEscalada As Boolean = False
                Dim uID = alerta!id
                'Se verifica que no se haya repetido antes
                Dim repeticiones As Integer = alerta!repeticiones + 1
                'Se verifica que no se haya repetido antes
                If alerta!repetida.Equals(System.DBNull.Value) Then
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!activada, "yyyy/MM/dd HH:mm:ss")), Now)
                Else
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!repetida, "yyyy/MM/dd HH:mm:ss")), Now)
                End If
                Dim tiempoCad = ""
                If segundos >= alerta!repetir_tiempo Then
                    segundos = DateDiff(DateInterval.Second, CDate(Format(alerta!activada, "yyyy/MM/dd HH:mm:ss")), Now)
                    tiempoCad = calcularTiempoCad(segundos)

                    If ValNull(alerta!llamada, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 0, proceso, prioridad, " & alerta!lista & ", " & uID & ", 9 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!sms, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 1, proceso, prioridad, " & alerta!lista & ", " & uID & ", 9 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!correo, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 2, proceso, prioridad, " & alerta!lista & ", " & uID & ", 9 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!mmcall, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 3, proceso, prioridad, " & alerta!lista & ", " & uID & ", 9 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    If ValNull(alerta!log, "A") = "S" Then
                        regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".mensajes (alerta, canal, proceso, prioridad, lista, alarma, tipo) SELECT alerta, 4, proceso, prioridad, " & alerta!lista & ", " & uID & ", 9 FROM " & rutaBD & ".alarmas WHERE id = " & uID)
                    End If
                    agregarLOG("Se ha enviado una repetición del reporte: " & procesoID, 0, procesoID)
                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".alarmas SET repeticiones = repeticiones + 1, repetida = NOW(), estatus = 1 WHERE id = " & alerta!id)
                End If
            Next
        End If


        procesandoEscalamientos = False
        BarManager1.Items(1).Caption = "Conectado (cada " & eSegundos & " segundos)"
        crearMensajes()
    End Sub

    Private Sub NotifyIcon1_MouseDoubleClick(sender As Object, e As MouseEventArgs) Handles NotifyIcon1.MouseDoubleClick
        Try
            Me.Visible = True
            NotifyIcon1.Visible = False
            Me.WindowState = FormWindowState.Maximized
        Catch ex As Exception

        End Try

    End Sub

    Private Sub XtraForm1_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        Me.Visible = False
        NotifyIcon1.Visible = True
    End Sub


    Private Sub ReanudarElMonitorToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ReanudarElMonitorToolStripMenuItem.Click
        If XtraMessageBox.Show("Esta acción reanudará el envío de alertas. ¿Desea reanudar el monitoreo de las fallas?", "Reanudar la aplicación", MessageBoxButtons.YesNo, MessageBoxIcon.Warning) <> DialogResult.No Then
            Estado = 1
            SimpleButton3.Visible = True
            SimpleButton2.Visible = False
            ContextMenuStrip1.Items(1).Enabled = True
            ContextMenuStrip1.Items(2).Enabled = False
            estadoPrograma = True
            agregarLOG("La interfaz ha sido reanudada por un usuario", 9, 0)
        End If
    End Sub

    Private Sub ToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem1.Click
        If XtraMessageBox.Show("Esta acción detendrá el envío de alertas. ¿Desea detener el monitor de las fallas?", "Detener la aplicación", MessageBoxButtons.YesNo, MessageBoxIcon.Warning) <> DialogResult.No Then
            Estado = 1
            SimpleButton3.Visible = False
            SimpleButton2.Visible = True
            ContextMenuStrip1.Items(1).Enabled = False
            ContextMenuStrip1.Items(2).Enabled = True
            estadoPrograma = False
            agregarLOG("La interfaz ha sido detenida por un usuario", 9, 0)
        End If
    End Sub

    Private Sub XtraForm1_Resize(sender As Object, e As EventArgs) Handles Me.Resize
        Dim f As Form
        f = sender

        'Check if the form is minimized
        If f.WindowState = FormWindowState.Minimized Then
            Me.Visible = False
            NotifyIcon1.Visible = True
        End If

    End Sub

    Private Sub XtraForm1_Closing(sender As Object, e As CancelEventArgs) Handles Me.Closing
        autenticado = False
        Dim Forma As New XtraForm2
        Forma.Text = "Detener aplicación"
        Forma.ShowDialog()
        If autenticado Then
            If XtraMessageBox.Show("Esta acción CERRARÁ la aplicación de monitoreo. ¿Desea continuar?", "Detener la aplicación", MessageBoxButtons.YesNo, MessageBoxIcon.Stop) <> DialogResult.No Then
                agregarLOG("La aplicación se cerró el usuario: " & usuarioCerrar, 9, 0)
            Else
                e.Cancel = True
            End If
        Else
            e.Cancel = True
        End If
    End Sub

    Sub agregarSolo(cadena As String)
        ListBoxControl1.Items.Insert(0, "MONITOR " & Format(Now, "dd-MMM HH:mm:ss") & ": " & cadena)
        ContarLOG()
    End Sub

    Sub depurar()
        'Se depura la BD
        If Format(DateAndTime.Now(), "HHmm") = "0000" And Not depurando Then
            depurando = True
            'Se produce la depuración a las 12 de la noche
            Dim cadSQL As String = "SELECT gestion_meses, gestion_log FROM " & rutaBD & ".configuracion WHERE gestion_meses > 0 AND (ISNULL(gestion_log) OR gestion_log < '" & Format(Now(), "yyyyMM") & "')"
            Dim reader As DataSet = consultaSEL(cadSQL)
            Dim regsAfectados = 0
            Dim eliminados = 0
            Dim mesesAtras = 1
            If reader.Tables(0).Rows.Count > 0 Then
                mesesAtras = reader.Tables(0).Rows(0)!gestion_meses
                If mesesAtras > 0 Then
                    regsAfectados = consultaACT("DELETE FROM " & rutaBD & ".movimientos_det WHERE inicio < '" & Format(DateAndTime.DateAdd(DateInterval.Month, mesesAtras * -1, Now()), "yyyy/MM") & "/01 00:00:00' AND estatus = 2")
                    eliminados = eliminados + regsAfectados

                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".configuracion SET gestion_log = '" & Format(Now(), "yyyyMM") & "'")

                    regsAfectados = consultaACT("DELETE FROM " & rutaBD & ".control WHERE fecha < '" & Format(DateAndTime.DateAdd(DateInterval.Month,
    mesesAtras * -1, Now()), "yyyyMM") & "0100'")


                    agregarLOG("Se ejecutó la depuración de la base de datos para el período " & Format(Now(), "MMMM-yyyy") & " (todo lo anterior al día: " & Format(DateAndTime.DateAdd(DateInterval.Month, mesesAtras * -1, Now()), "yyyy/MM") & "/01). Se eliminaron permanentemente " & eliminados & " registro(s)", 7, 0)

                End If
            End If
            mesesAtras = 1
            regsAfectados = consultaACT("DELETE FROM " & rutaBD & ".alarmas WHERE fin < '" & Format(DateAndTime.DateAdd(DateInterval.Day, -10, Now()), "yyyy/MM/dd") & " 00:00:00'")

            regsAfectados = consultaACT("DELETE FROM " & rutaBD & ".mensajes WHERE enviada < '" & Format(DateAndTime.DateAdd(DateInterval.Day, -2, Now()), "yyyy/MM/dd") & " 00:00:00'")
            eliminados = eliminados + regsAfectados
            regsAfectados = consultaACT("DELETE FROM " & rutaBD & ".log WHERE fecha < '" & Format(DateAndTime.DateAdd(DateInterval.Day, -15, Now()), "yyyy/MM/dd") & " 00:00:00'")
            eliminados = eliminados + regsAfectados
            regsAfectados = consultaACT("DELETE FROM " & rutaBD & ".mensajes_procesados WHERE fecha < '" & Format(DateAndTime.DateAdd(DateInterval.Day, -2, Now()), "yyyy/MM/dd") & "'")

            eliminados = eliminados + regsAfectados
            depurando = False
        End If
    End Sub

    Private Sub revisarLog_Tick(sender As Object, e As EventArgs) Handles revisarLog.Tick
        revisarLog.Enabled = False
        If leyendoLog Or Not estadoPrograma Then Exit Sub
        leyendoLog = True
        Dim regsAfectados = consultaACT("UPDATE " & rutaBD & ".log SET visto = 'P' WHERE visto = 'N'")
        Dim cadSQL = "SELECT id, texto, aplicacion FROM " & rutaBD & ".log WHERE visto = 'P' ORDER BY id"
        Dim reader = consultaSEL(cadSQL)
        If reader.Tables(0).Rows.Count > 0 Then
            sinEventos.Enabled = False
            sinEventos.Enabled = True
            For Each elmensaje In reader.Tables(0).Rows
                Dim appOrigen = "MONITOR"
                If elmensaje!aplicacion = 30 Then
                    appOrigen = "TELEFONIA"
                ElseIf elmensaje!aplicacion = 40 Then
                    appOrigen = "CORREOS"
                ElseIf elmensaje!aplicacion = 20 Then
                    appOrigen = "MMCALL"
                ElseIf elmensaje!aplicacion = 60 Then
                    appOrigen = "LOG"
                ElseIf elmensaje!aplicacion = 50 Then
                    appOrigen = "SMS"
                ElseIf elmensaje!aplicacion = 70 Then
                    appOrigen = "VOZ"
                ElseIf elmensaje!aplicacion = 80 Then
                    appOrigen = "REPORTES"
                End If
                ListBoxControl1.Items.Insert(0, appOrigen & " " & Format(Now, "dd-MMM HH:mm:ss") & ": " & elmensaje!texto)

            Next
            regsAfectados = consultaACT("UPDATE " & rutaBD & ".log SET visto = 'S' WHERE visto = 'P'")
            ContarLOG()
        End If
        leyendoLog = False
        revisarLog.Enabled = True
    End Sub

    Private Sub sinEventos_Tick(sender As Object, e As EventArgs) Handles sinEventos.Tick
        agregarSolo("No se ha generado información durante los ultimos 5 minutos")
    End Sub

    Private Sub escalamiento_Tick(sender As Object, e As EventArgs) Handles escalamiento.Tick
        If procesandoEscalamientos Or Not estadoPrograma Then Exit Sub
        escalamiento.Enabled = False
        escalamientos()
        escalamiento.Enabled = True
    End Sub

    Private Sub reportes_Tick(sender As Object, e As EventArgs) Handles reportes.Tick
        If enviandoReportes Or Not estadoPrograma Then Exit Sub
        If Format(Now, "mm") >= "05" Then
            entroReportes = False
            Exit Sub
        ElseIf Not entroReportes Then
            entroReportes = True
        Else
            Exit Sub

        End If
        reportes.Enabled = False
        enviandoReportes = True

        Try

            Shell(Application.StartupPath & "\reportes.exe " & Chr(34) & cadenaConexion & Chr(34), AppWinStyle.MinimizedNoFocus)
            agregarSolo("Se inicia la aplicación de Envío de reportes por correo")
        Catch ex As Exception
            agregarLOG("Error en la ejecución de la aplicación de envío de reportes por correos. Error: " & ex.Message, 7, 0)
        End Try
        enviandoReportes = False
        reportes.Enabled = True
    End Sub

    Private Sub arduino_Tick(sender As Object, e As EventArgs) Handles arduino.Tick
        If Not estadoPrograma Then Exit Sub

        Dim cadSQL As String = "SELECT ruta_audios, ruta_sms, be_alarmas_llamadas, be_alarmas_sms FROM " & rutaBD & ".configuracion"
        Dim reader As DataSet = consultaSEL(cadSQL)
        Dim regsAfectados = 0
        Dim rutaAudios
        Dim rutaSMS
        Dim be_alarmas_llamadas As Boolean = False
        Dim be_alarmas_sms As Boolean = False

        If errorBD.Length > 0 Then
            agregarLOG("No se logró la conexión con MySQL. Error: " + errorBD, 9, 0)

        Else
            rutaAudios = ValNull(reader.Tables(0).Rows(0)!ruta_audios, "A")
            rutaSMS = ValNull(reader.Tables(0).Rows(0)!ruta_sms, "A")
            be_alarmas_llamadas = ValNull(reader.Tables(0).Rows(0)!be_alarmas_llamadas, "A") = "S"
            be_alarmas_sms = ValNull(reader.Tables(0).Rows(0)!be_alarmas_sms, "A") = "S"
        End If
        Dim llamarPrograma As Boolean = False
        If be_alarmas_llamadas Or be_alarmas_sms Then
            If rutaSMS.Length = 0 Then
                rutaSMS = My.Computer.FileSystem.SpecialDirectories.MyDocuments
            Else
                rutaSMS = Strings.Replace(rutaSMS, "/", "\")
            End If
            If Not My.Computer.FileSystem.DirectoryExists(rutaSMS) Then
                rutaSMS = My.Computer.FileSystem.SpecialDirectories.MyDocuments
            End If

            If rutaAudios.Length = 0 Then
                rutaAudios = My.Computer.FileSystem.SpecialDirectories.MyDocuments
            Else
                rutaAudios = Strings.Replace(rutaAudios, "/", "\")
            End If
            If Not My.Computer.FileSystem.DirectoryExists(rutaAudios) Then
                rutaAudios = My.Computer.FileSystem.SpecialDirectories.MyDocuments
            End If

            If Not My.Computer.FileSystem.DirectoryExists(rutaAudios) Then
                rutaAudios = My.Computer.FileSystem.SpecialDirectories.MyDocuments
            End If
            Dim LlamadasPendientes = 0
            For Each FoundFile As String In My.Computer.FileSystem.GetFiles(
          rutaAudios, Microsoft.VisualBasic.FileIO.SearchOption.SearchTopLevelOnly, "*.wav")
                Dim Numero = Microsoft.VisualBasic.Strings.Left(Path.GetFileName(FoundFile), 10)
                Dim nombreArchivo = Path.GetFileName(FoundFile)
                If IsNumeric(Numero) Then LlamadasPendientes = LlamadasPendientes + 1
            Next
            If LlamadasPendientes > 0 And be_alarmas_llamadas Then
                llamarPrograma = True
            End If
            If Not llamarPrograma Then
                For Each FoundFile As String In My.Computer.FileSystem.GetFiles(
          rutaSMS, Microsoft.VisualBasic.FileIO.SearchOption.SearchTopLevelOnly, "*.txt")
                    Dim Numero = Microsoft.VisualBasic.Strings.Left(Path.GetFileName(FoundFile), 10)
                    Dim nombreArchivo = Path.GetFileName(FoundFile)
                    If IsNumeric(Numero) Then LlamadasPendientes = LlamadasPendientes + 1
                Next
                If LlamadasPendientes > 0 And be_alarmas_llamadas Then
                    llamarPrograma = True
                End If
            End If
        End If
        If llamarPrograma Then
            Try
                Shell(Application.StartupPath & "\arduino.exe " & Chr(34) & cadenaConexion & Chr(34), AppWinStyle.MinimizedNoFocus)
                agregarSolo("Se inicia la aplicación de Interfaz telefónica")
            Catch ex As Exception
                agregarLOG("Error en la ejecución de la aplicación de Interfaz telefónica Error: " & ex.Message, 7, 0)
            End Try
        End If
    End Sub

    Sub validarLicencia()
        'Dim cadSQL = "SELECT licencia FROM " & rutabd & ".configuracion"
        ' Dim falla As DataSet = consultaSEL(cadSQL)
        'If falla.Tables(0).Rows.Count > 0 Then
        'If ValNull(falla.Tables(0).Rows(0)!volumen, "A") = "" Then
        'XtraMessageBox.Show("Su licencia no pudo ser validada. Introduzca una licencia en la aplicación web de andon e intente de nuevo o contacte con su proveedor de ANDON-SIGMA", "No es posible continuar", MessageBoxButtons.OK, MessageBoxIcon.Error)
        'Application.Exit()
        'Else
        'Dim miLicencia = ValNull(falla.Tables(0).Rows(0)!volumen, "A")
        'If Strings.Mid(miLicencia, 11, 1) = "V" Or Strings.Mid(miLicencia, 11, 1) <> "S" Then
        'XtraMessageBox.Show("Su licencia ha caducado. Contacte con su proveedor de ANDON-SIGMA", "No es posible continuar", MessageBoxButtons.OK, MessageBoxIcon.Error)
        'Application.Exit()
        'End If
        'End If
        'End If
    End Sub

    Private Sub reenviarMMCALL_Tick(sender As Object, e As EventArgs) Handles reenviarMMCALL.Tick

        If reenviar Then Exit Sub
        reenviar = True
        If Not estadoPrograma Then Exit Sub
        Try

            Shell(Application.StartupPath & "\repeticiones.exe " & Chr(34) & cadenaConexion & Chr(34), AppWinStyle.MinimizedNoFocus)
        Catch ex As Exception
            agregarLOG("Error en la ejecución de la aplicación de envío de repeticiones de MMCall. Error: " & ex.Message, 7, 0)
        End Try

        reenviar = False
    End Sub

    Function validarURI(ByVal cadena As String) As Boolean
        Dim validatedUri As System.Uri
        Return Uri.TryCreate(cadena, UriKind.RelativeOrAbsolute, validatedUri)
    End Function



    Sub crearMensajes()
        Dim idProceso = Process.GetCurrentProcess.Id
        Dim mensajesDS As DataSet
        Dim registroDS As DataSet
        Dim eMensaje = ""
        Dim transporte As String = ""
        Dim chofer As String = ""
        Dim area As String = ""
        Dim placas As String = ""
        Dim destino As String = ""
        Dim fecha
        Dim tiempo As String = ""
        Dim cadSQL As String = ""
        Dim nroReporte As Integer = 0
        Dim eTitulo = ""

        'Escalada 4
        Dim miError As String = ""
        Dim optimizar As Boolean = False
        Dim mantenerPrioridad As Boolean = False
        Dim regsAfectados = 0

        Dim maximo_largo_mmcall As Integer = 40



        regsAfectados = consultaACT("UPDATE " & rutaBD & ".mensajes SET estatus = '" & idProceso & "' WHERE estatus = 'A'")
        cadSQL = "SELECT a.id, a.canal, b.evento, a.prioridad FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_alertas b on a.alerta = b.id WHERE a.estatus = '" & idProceso & "' ORDER BY a.prioridad DESC, a.id"
        'Se preselecciona la voz
        mensajesDS = consultaSEL(cadSQL)
        Dim generarMensaje As Boolean
        If mensajesDS.Tables(0).Rows.Count > 0 Then
            For Each elmensaje In mensajesDS.Tables(0).Rows
                generarMensaje = False
                eMensaje = ""
                destino = ""
                transporte = ""
                chofer = ""
                area = ""
                placas = ""
                generarMensaje = True
                nroReporte = 0

                cadSQL = "SELECT a.*, e.nombre as ntransporte, f.nombre as nchofer, g.nombre as narea, h.nombre as placa, j.nombre as ndestino, c.id AS idalerta, c.acumular, c.mensaje_mmcall, c.mensaje, c.titulo, c.resolucion_mensaje, c.cancelacion_mensaje, d.desde, d.estado, i.repeticiones, i.fase, i.escalamientos1, i.escalamientos2, i.escalamientos3, i.escalamientos4, i.escalamientos5 FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".cat_distribucion b ON a.lista = b.id AND b.estatus = 'A' INNER JOIN " & rutaBD & ".cat_alertas c ON a.alerta = c.id INNER JOIN " & rutaBD & ".alarmas i ON a.alarma = i.id LEFT JOIN " & rutaBD & ".requesters d ON a.proceso = d.id LEFT JOIN " & rutaBD & ".cat_transportes e ON d.transporte = e.id LEFT JOIN " & rutaBD & ".cat_choferes f ON d.chofer = f.id LEFT JOIN " & rutaBD & ".cat_generales g ON d.area = g.id LEFT JOIN " & rutaBD & ".cat_vehiculos h ON d.vehiculo = h.id LEFT JOIN " & rutaBD & ".cat_destinos j ON d.destino = j.id WHERE a.id = " & elmensaje!id

                registroDS = consultaSEL(cadSQL)
                If registroDS.Tables(0).Rows.Count > 0 Then
                    nroReporte = registroDS.Tables(0).Rows(0)!proceso
                    If elmensaje!evento < 300 Then
                        transporte = ValNull(registroDS.Tables(0).Rows(0)!ntransporte, "A")
                        chofer = ValNull(registroDS.Tables(0).Rows(0)!nchofer, "A")
                        area = ValNull(registroDS.Tables(0).Rows(0)!narea, "A")
                        placas = ValNull(registroDS.Tables(0).Rows(0)!placa, "A")
                        destino = ValNull(registroDS.Tables(0).Rows(0)!ndestino, "A")
                    Else
                        transporte = ""
                        chofer = ""
                        area = ""
                        placas = ""
                        destino = ""
                    End If

                    If elmensaje!evento Then
                        If Not registroDS.Tables(0).Rows(0)!desde.Equals(System.DBNull.Value) Then
                            fecha = registroDS.Tables(0).Rows(0)!desde
                        Else
                            fecha = Now()
                        End If

                    End If
                    tiempo = calcularTiempoCad(DateAndTime.DateDiff(DateInterval.Second, fecha, Now))
                    Dim mIFormato = "dd-MMM-yyyy HH:mm:ss"


                    If registroDS.Tables(0).Rows(0)!tipo = 8 Then
                        eMensaje = ValNull(registroDS.Tables(0).Rows(0)!resolucion_mensaje, "A")
                    ElseIf registroDS.Tables(0).Rows(0)!tipo = 7 Then
                        eMensaje = ValNull(registroDS.Tables(0).Rows(0)!cancelacion_mensaje, "A")
                    Else
                        If elmensaje!canal = 3 Then
                            mIFormato = "dd/MM HH:mm"
                            eMensaje = ValNull(registroDS.Tables(0).Rows(0)!mensaje_mmcall, "A")
                        Else
                            eMensaje = ValNull(registroDS.Tables(0).Rows(0)!mensaje, "A")
                        End If
                    End If

                    If eMensaje.Length > 0 Then
                        eMensaje = Replace(eMensaje, "[1]", chofer)
                        eMensaje = Replace(eMensaje, "[2]", transporte)
                        eMensaje = Replace(eMensaje, "[3]", placas)
                        eMensaje = Replace(eMensaje, "[4]", destino)
                        eMensaje = Replace(eMensaje, "[5]", Format(fecha, mIFormato))
                        eMensaje = Replace(eMensaje, "[11]", tiempo)
                        If ValNull(registroDS.Tables(0).Rows(0)!repeticiones, "N") > 0 Then
                            eMensaje = Replace(eMensaje, "[20]", "R" & ValNull(registroDS.Tables(0).Rows(0)!repeticiones, "N"))
                        Else
                            eMensaje = Replace(eMensaje, "[20]", "")
                        End If
                        If ValNull(registroDS.Tables(0).Rows(0)!fase - 10, "N") > 0 Then
                            Dim escala = ValNull(registroDS.Tables(0).Rows(0)!fase, "N") - 10
                            If elmensaje!canal = 3 Then
                                eMensaje = Replace(eMensaje, "[30]", "E" & If(escala > 0, escala, 0))
                            Else
                                eMensaje = Replace(eMensaje, "[30]", "Escalado al Nivel " & If(escala > 0, escala, 0))
                            End If

                        Else
                            eMensaje = Replace(eMensaje, "[30]", "")
                        End If
                        If ValNull(registroDS.Tables(0).Rows(0)!escalamientos1, "N") > 0 Then
                            eMensaje = Replace(eMensaje, "[31]", "R" & ValNull(registroDS.Tables(0).Rows(0)!escalamientos1, "N"))
                        Else
                            eMensaje = Replace(eMensaje, "[31]", "")
                        End If
                        If ValNull(registroDS.Tables(0).Rows(0)!escalamientos2, "N") > 0 Then
                            eMensaje = Replace(eMensaje, "[32]", "R" & ValNull(registroDS.Tables(0).Rows(0)!escalamientos2, "N"))
                        Else
                            eMensaje = Replace(eMensaje, "[32]", "")
                        End If
                        If ValNull(registroDS.Tables(0).Rows(0)!escalamientos3, "N") > 0 Then
                            eMensaje = Replace(eMensaje, "[33]", "R" & ValNull(registroDS.Tables(0).Rows(0)!escalamientos3, "N"))
                        Else
                            eMensaje = Replace(eMensaje, "[33]", "")
                        End If
                        If ValNull(registroDS.Tables(0).Rows(0)!escalamientos4, "N") > 0 Then
                            eMensaje = Replace(eMensaje, "[34]", "R" & ValNull(registroDS.Tables(0).Rows(0)!escalamientos4, "N"))
                        Else
                            eMensaje = Replace(eMensaje, "[34]", "")
                        End If
                        If ValNull(registroDS.Tables(0).Rows(0)!escalamientos5, "N") > 0 Then
                            eMensaje = Replace(eMensaje, "[35]", "R" & ValNull(registroDS.Tables(0).Rows(0)!escalamientos5, "N"))
                        Else
                            eMensaje = Replace(eMensaje, "[35]", "")
                        End If


                    Else
                        If elmensaje!evento = 101 Or elmensaje!evento = 104 Then
                            eMensaje = "REPORTE " & nroReporte & " TIEMPO ESPERA EXCED"
                        ElseIf elmensaje!evento = 102 Then
                            eMensaje = "REPORTE " & nroReporte & " TIEMPO TRANSITO EXCED"
                        ElseIf elmensaje!evento = 103 Then
                            eMensaje = "REPORTE " & nroReporte & " TIEMPO DESCARGA EXCED"
                        End If
                    End If

                    If elmensaje!canal = 2 Then
                        If eTitulo.Length = 0 Then
                            eTitulo = ValNull(registroDS.Tables(0).Rows(0)!titulo, "A")
                        End If
                        If eTitulo.Length > 0 Then
                            eTitulo = Replace(eTitulo, "[1]", chofer)
                            eTitulo = Replace(eTitulo, "[2]", transporte)
                            eTitulo = Replace(eTitulo, "[3]", placas)
                            eTitulo = Replace(eTitulo, "[4]", destino)
                            eTitulo = Replace(eTitulo, "[5]", Format(fecha, mIFormato))
                            eTitulo = Replace(eTitulo, "[11]", tiempo)

                            If ValNull(registroDS.Tables(0).Rows(0)!repeticiones, "N") > 0 Then
                                eTitulo = Replace(eTitulo, "[20]", "Repetición " & ValNull(registroDS.Tables(0).Rows(0)!repeticiones, "N"))
                            Else
                                eTitulo = Replace(eTitulo, "[20]", "")
                            End If
                            If ValNull(registroDS.Tables(0).Rows(0)!fase - 10, "N") > 0 Then
                                Dim escala = ValNull(registroDS.Tables(0).Rows(0)!fase, "N") - 10
                                eTitulo = Replace(eTitulo, "[30]", "Escalado al Nivel " & If(escala > 0, escala, 0))
                            Else
                                eTitulo = Replace(eTitulo, "[30]", "")
                            End If
                            If ValNull(registroDS.Tables(0).Rows(0)!escalamientos1, "N") > 0 Then
                                eTitulo = Replace(eTitulo, "[31]", "Repetición " & ValNull(registroDS.Tables(0).Rows(0)!escalamientos1, "N"))
                            Else
                                eTitulo = Replace(eTitulo, "[31]", "")
                            End If
                            If ValNull(registroDS.Tables(0).Rows(0)!escalamientos2, "N") > 0 Then
                                eTitulo = Replace(eTitulo, "[32]", "Repetición " & ValNull(registroDS.Tables(0).Rows(0)!escalamientos2, "N"))
                            Else
                                eTitulo = Replace(eTitulo, "[32]", "")
                            End If
                            If ValNull(registroDS.Tables(0).Rows(0)!escalamientos3, "N") > 0 Then
                                eTitulo = Replace(eTitulo, "[33]", "Repetición " & ValNull(registroDS.Tables(0).Rows(0)!escalamientos3, "N"))
                            Else
                                eTitulo = Replace(eTitulo, "[33]", "")
                            End If
                            If ValNull(registroDS.Tables(0).Rows(0)!escalamientos4, "N") > 0 Then
                                eTitulo = Replace(eTitulo, "[34]", "Repetición " & ValNull(registroDS.Tables(0).Rows(0)!escalamientos4, "N"))
                            Else
                                eTitulo = Replace(eTitulo, "[34]", "")
                            End If
                            If ValNull(registroDS.Tables(0).Rows(0)!escalamientos5, "N") > 0 Then
                                eTitulo = Replace(eTitulo, "[35]", "Repetición " & ValNull(registroDS.Tables(0).Rows(0)!escalamientos5, "N"))
                            Else
                                eTitulo = Replace(eTitulo, "[35]", "")
                            End If
                            eTitulo = Replace(eTitulo, "[90]", "")
                            eTitulo = Replace(eTitulo, System.Environment.NewLine, " ")


                        End If

                    ElseIf elmensaje!canal = 3 Then
                        Dim antes As String = "ÃÀÁÄÂÈÉËÊÌÍÏÎÒÓÖÔÙÚÜÛãàáäâèéëêìíïîòóöôùúüûÑñÇç"
                        Dim ahora As String = "AAAAAEEEEIIIIOOOOUUUUaaaaaeeeeiiiioooouuuunncc"
                        For i = 0 To antes.Length - 1
                            eMensaje = Replace(eMensaje, antes(i), ahora(i))
                        Next
                        eMensaje = Replace(eMensaje, ";", " ")
                        eMensaje = Replace(eMensaje, "\", "-")
                        eMensaje = Replace(eMensaje, "/", "-")
                        eMensaje = Replace(eMensaje, "[90]", "")
                        eMensaje = Replace(eMensaje, System.Environment.NewLine, " ")
                        'Se cambian los caracteres especiales
                    End If
                    eMensaje = Strings.Left(eMensaje, 400)
                    eTitulo = Strings.Left(eTitulo, 100)
                End If
                Dim cadAdic = ""
                If eMensaje.Length > 0 Then
                    cadAdic = "INSERT INTO " & rutaBD & ".mensajes_procesados (texto, canal, titulo, prioridad, fecha, mensaje) VALUES ('" & eMensaje & "', " & elmensaje!canaL & ", '" & eTitulo & "', " & elmensaje!prioridad & ", NOW(), " & elmensaje!id & ");"
                End If
                regsAfectados = consultaACT(cadAdic & "UPDATE " & rutaBD & ".mensajes SET estatus = 'E', enviada = NOW() WHERE estatus = '" & idProceso & "'")
            Next
        End If
    End Sub


    Private Sub borrarArchivos_Tick(sender As Object, e As EventArgs) Handles borrarArchivos.Tick

        For i = 0 To filestoDelete.Length - 1
            If Not IsNothing(filestoDelete(i)) Then eliminarArchivo(filestoDelete(i))
        Next
        borrarArchivos.Enabled = False
    End Sub




    'LUEGO ELIMINAR EEMV
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

    Private Sub turno_Tick(sender As Object, e As EventArgs) Handles turno.Tick
        If Not estadoPrograma Then Exit Sub
        Dim cadSQL As String = "SELECT destino, id, fecha_recibo, desde, orden, IF(estado = 10, estimado + des_estimado, 0) AS espera FROM " & rutaBD & ".requesters WHERE estado = 10 AND estatus <> 2 ORDER BY destino, fecha_asignacion"

        Dim turnos As DataSet = consultaSEL(cadSQL)

        If turnos.Tables(0).Rows.Count > 0 Then
            Dim destino = turnos.Tables(0).Rows(0)!destino
            Dim tiempo = 0
            Dim orden = 0
            Dim sentencia = ""
            For Each eTurno In turnos.Tables(0).Rows
                If destino <> eTurno!destino Then
                    destino = eTurno!destino
                    tiempo = 0
                    orden = 0
                End If
                Dim actFecha = ""
                If orden > 0 Then
                    If eTurno!fecha_recibo.Equals(System.DBNull.Value) Then
                        actFecha = ", fecha_recibo = DATE_ADD(NOW(), INTERVAL " & tiempo & " SECOND)"
                    Else
                        Dim difTiempo = DateDiff(DateInterval.Second, eTurno!fecha_recibo, Now())
                        If difTiempo < 0 And Math.Abs(difTiempo) > tiempo Then
                            actFecha = ", fecha_recibo = DATE_ADD(NOW(), INTERVAL " & tiempo & " SECOND)"
                        End If
                    End If
                Else
                    If eTurno!fecha_recibo.Equals(System.DBNull.Value) Then
                        actFecha = ", fecha_recibo = NOW()"
                    End If
                    If eTurno!orden = 1 And eTurno!desde.Equals(System.DBNull.Value) Then
                        actFecha = ", desde = NOW()"
                    End If

                End If
                orden = orden + 1
                sentencia = sentencia & "UPDATE " & rutaBD & ".requesters SET orden = " & orden & " " & actFecha & " WHERE id = " & eTurno!id & ";"

                tiempo = tiempo + eTurno!espera

            Next
            Dim regsAfectados = consultaACT(sentencia & "UPDATE " & rutaBD & ".actualizaciones SET dispositivos = NOW()")
            agregarSolo("Se recalculan los turnos para tránsito y descarga")
        End If
    End Sub

    Sub resample(origen, destino)
        Dim reader = New AudioFileReader(origen)
        Dim resampler = New WdlResamplingSampleProvider(reader, 16000)
        WaveFileWriter.CreateWaveFile16(destino, resampler)
        reader.Close()
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        Dim regsAfectados = consultaACT("UPDATE " & rutaBD & ".configuracion SET ultima_actualizacion = NOW()")
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

    Sub actualizarBD()
        'Se valida que exista la BD
        Dim cadSQL = "USE " & rutaBD
        Dim reader As DataSet = consultaSEL(cadSQL)
        Dim regsAfectados = 0
        Dim huboProceso = False
        If reader Is Nothing Then
            huboProceso = True
            'Se crea la bases de datos
            regsAfectados = consultaACT("CREATE DATABASE /*!32312 IF NOT EXISTS*/`" & rutaBD & "` /*!40100 DEFAULT CHARACTER SET latin1 */;

USE `" & rutaBD & "`;

/*Table structure for table `actualizaciones` */

DROP TABLE IF EXISTS `actualizaciones`;

CREATE TABLE `actualizaciones` (
  `plantas` datetime DEFAULT NULL COMMENT 'Se actualizaron las plantas?',
  `lineas` datetime DEFAULT NULL COMMENT 'Se actualizaron las líneas?',
  `maquinas` datetime DEFAULT NULL COMMENT 'Se actualizaron las máquinas?',
  `procesos` datetime DEFAULT NULL COMMENT 'Se actualizaron las procesos?',
  `rutas` datetime DEFAULT NULL COMMENT 'Se actualizaron las rutas?',
  `det_rutas` datetime DEFAULT NULL COMMENT 'Se actualizaron las rutas detalle?',
  `det_procesos` datetime DEFAULT NULL COMMENT 'Se actualizaron los procesos detalle?',
  `partes` datetime DEFAULT NULL COMMENT 'Se actualizaron las partes?',
  `recipientes` datetime DEFAULT NULL COMMENT 'Se actualizaron las recipientes?',
  `alertas` datetime DEFAULT NULL COMMENT 'Se actualizaron las alertas?',
  `situaciones` datetime DEFAULT NULL COMMENT 'Se actualizaron las situaciones?',
  `horario` datetime DEFAULT NULL COMMENT 'Se actualizaron las horarios?',
  `planes` datetime DEFAULT NULL COMMENT 'Se actualizaron las planes?',
  `prioridades` datetime DEFAULT NULL COMMENT 'Se actualizaron las prioridades?',
  `areas` datetime DEFAULT NULL COMMENT 'Se actualizaron las areas?',
  `fallas` datetime DEFAULT NULL COMMENT 'Se actualizaron las fallas?',
  `generales` datetime DEFAULT NULL COMMENT 'Se actualizaron las tablas generales?',
  `distribucion` datetime DEFAULT NULL COMMENT 'Se actualizaron las distribuciones?',
  `correos` datetime DEFAULT NULL COMMENT 'Se actualizaron los correos?',
  `turnos` datetime DEFAULT NULL COMMENT 'Se actualizaron los turnos?',
  `usuarios` datetime DEFAULT NULL COMMENT 'Se actualizaron los usuarios?',
  `traducciones` datetime DEFAULT NULL COMMENT 'Se actualizaron las traducciones?',
  `politicas` datetime DEFAULT NULL COMMENT 'Se actualizaron las politicas?',
  `rates` datetime DEFAULT NULL COMMENT 'Se actualizaron las rates?',
  `estimados` datetime DEFAULT NULL COMMENT 'Se actualizaron las estimados?',
  `objetivos` datetime DEFAULT NULL COMMENT 'Se actualizaron las objetivos?',
  `sensores` datetime DEFAULT NULL COMMENT 'Se actualizaron las sensores?',
  `paros` datetime DEFAULT NULL COMMENT 'Se actualizaron los paros?',
  `dispositivos` datetime DEFAULT NULL COMMENT 'Se actualizaron los dispositivos?',
  `vehiculos` datetime DEFAULT NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Control de actualización';

/*Table structure for table `alarmas` */

DROP TABLE IF EXISTS `alarmas`;

CREATE TABLE `alarmas` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `alerta` bigint(20) DEFAULT '0' COMMENT 'ID de la alerta',
  `proceso` bigint(20) DEFAULT '0' COMMENT 'ID del proceso',
  `proceso_detalle` bigint(20) DEFAULT '0' COMMENT 'ID del detalle del proceso',
  `prioridad` varchar(1) DEFAULT NULL COMMENT 'Prioridad de la alerta',
  `inicio` datetime DEFAULT CURRENT_TIMESTAMP COMMENT 'Fecha de inicio de la alarma',
  `fin` datetime DEFAULT NULL COMMENT 'Fecha de fin de la alarma',
  `tiempo` bigint(8) DEFAULT '0' COMMENT 'Tiempo que duro la alarma activa (en segundos)',
  `activada` datetime DEFAULT NULL COMMENT 'Fecha de activación',
  `repetida` datetime DEFAULT NULL COMMENT 'Fecha de repetición',
  `escalada1` datetime DEFAULT NULL COMMENT 'Fecha de escalamiento (1)',
  `escalada2` datetime DEFAULT NULL COMMENT 'Fecha de escalamiento (2)',
  `escalada3` datetime DEFAULT NULL COMMENT 'Fecha de escalamiento (3)',
  `escalada4` datetime DEFAULT NULL COMMENT 'Fecha de escalamiento (4)',
  `escalada5` datetime DEFAULT NULL COMMENT 'Fecha de escalamiento (5)',
  `fase` int(2) DEFAULT '0' COMMENT 'Fase en que está la alarma',
  `estatus` int(1) DEFAULT '0' COMMENT 'Estatus de la alarma',
  `repeticiones` int(4) DEFAULT '0' COMMENT 'Total repeticiones',
  `escalamientos1` int(4) DEFAULT '0' COMMENT 'Total escalamientos (1)',
  `escalamientos2` int(4) DEFAULT '0' COMMENT 'Total escalamientos (2)',
  `escalamientos3` int(4) DEFAULT '0' COMMENT 'Total escalamientos (3)',
  `escalamientos4` int(4) DEFAULT '0' COMMENT 'Total escalamientos (4)',
  `escalamientos5` int(4) DEFAULT '0' COMMENT 'Total escalamientos (5)',
  `informado` char(1) DEFAULT 'N' COMMENT 'Al terminarse esta informada?',
  `acumulada` char(1) DEFAULT 'N' COMMENT 'Es una alarma acumulada?',
  `termino` bigint(20) DEFAULT '0' COMMENT 'Usuario que terminó la alerta',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`alerta`,`inicio`),
  KEY `NewIndex2` (`inicio`),
  KEY `NewIndex3` (`tiempo`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Detalle de alarmas';

/*Table structure for table `cat_alertas` */

DROP TABLE IF EXISTS `cat_alertas`;

CREATE TABLE `cat_alertas` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `referencia` varchar(50) DEFAULT NULL COMMENT 'Referencia',
  `nombre` varchar(60) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `solapar` char(1) DEFAULT 'N' COMMENT 'Solapar alertas',
  `evento` bigint(20) DEFAULT '0' COMMENT 'ID del evento a monitorear',
  `tipo` int(3) DEFAULT '0' COMMENT 'Tipo: 0=Normal, 9=Escape',
  `notas` varchar(300) DEFAULT NULL COMMENT 'Notas varias',
  `proceso` bigint(20) DEFAULT '0' COMMENT 'Proceso asociado a la alerta',
  `linea` char(1) DEFAULT 'S' COMMENT 'Línea asignada a la alerta',
  `maquina` bigint(20) DEFAULT '0' COMMENT 'Máquina asignada a la alerta',
  `area` bigint(20) DEFAULT '0' COMMENT 'Área asignada a la alerta',
  `falla` bigint(20) DEFAULT '0' COMMENT 'Falla asignada a la alerta',
  `prioridad` int(1) DEFAULT '0' COMMENT 'Prioridad (0: Normal, 1: Alta)',
  `transcurrido` bigint(12) DEFAULT '0' COMMENT 'Tiempo trnscurrido en segundos para la alarma',
  `acumular` char(1) DEFAULT 'N' COMMENT 'Acumular fallas antes de enviar',
  `acumular_veces` bigint(6) DEFAULT '0' COMMENT 'Número de veces a acumular',
  `acumular_tiempo` bigint(8) DEFAULT '0' COMMENT 'Tiempo de acumulación',
  `acumular_inicializar` char(1) DEFAULT 'N' COMMENT 'Inicializa el contador una vez alcanzada la frecuencia',
  `log` char(1) DEFAULT 'N' COMMENT 'Se generará LOG',
  `sms` char(1) DEFAULT 'N' COMMENT 'Se enviará SMS',
  `correo` char(1) DEFAULT 'N' COMMENT 'Se enviará correo',
  `llamada` char(1) DEFAULT 'N' COMMENT 'Se hará llamada',
  `mmcall` char(1) DEFAULT 'N' COMMENT 'Se enviará llamada a MMCall',
  `lista` bigint(20) DEFAULT '0' COMMENT 'Lista de  distribución',
  `veces` int(3) DEFAULT '0' COMMENT 'Veces a alarmar',
  `escalar1` char(1) DEFAULT 'N' COMMENT 'Escalar 1ro',
  `tiempo1` bigint(8) DEFAULT '0' COMMENT 'Tiempo de escalación (1)',
  `lista1` bigint(20) DEFAULT '0' COMMENT 'Lista de distribución (1)',
  `log1` char(1) DEFAULT 'N' COMMENT 'Generar LOG (1)',
  `sms1` char(1) DEFAULT 'N' COMMENT 'Enviar SMS (1)',
  `correo1` char(1) DEFAULT 'N' COMMENT 'Enviar correo (1)',
  `llamada1` char(1) DEFAULT 'N' COMMENT 'Generar Llamada (1)',
  `mmcall1` char(1) DEFAULT 'N' COMMENT 'Área de MMCall (1)',
  `repetir1` char(1) DEFAULT 'N' COMMENT 'Repetir el escalamiento (1)',
  `veces1` int(3) DEFAULT '0' COMMENT 'Veces a escalar (1)',
  `escalar2` char(1) DEFAULT 'N' COMMENT 'Escalar 2do',
  `tiempo2` bigint(8) DEFAULT '0' COMMENT 'Tiempo de escalación (2)',
  `lista2` bigint(20) DEFAULT '0' COMMENT 'Lista de distribución (2)',
  `log2` char(1) DEFAULT 'N' COMMENT 'Generar LOG (2)',
  `sms2` char(1) DEFAULT 'N' COMMENT 'Enviar SMS (2)',
  `correo2` char(1) DEFAULT 'N' COMMENT 'Enviar correo (2)',
  `llamada2` char(1) DEFAULT 'N' COMMENT 'Generar Llamada (2)',
  `mmcall2` char(1) DEFAULT 'N' COMMENT 'Área de MMCall (2)',
  `repetir2` char(1) DEFAULT 'N' COMMENT 'Repetir el escalamiento (2)',
  `veces2` int(3) DEFAULT '0' COMMENT 'Veces a escalar (2)',
  `escalar3` char(1) DEFAULT 'N' COMMENT 'Escalar 3ro',
  `tiempo3` bigint(8) DEFAULT '0' COMMENT 'Tiempo de escalación (3)',
  `lista3` bigint(20) DEFAULT '0' COMMENT 'Lista de distribución (3)',
  `log3` char(1) DEFAULT 'N' COMMENT 'Generar LOG (3)',
  `sms3` char(1) DEFAULT 'N' COMMENT 'Enviar SMS (3)',
  `correo3` char(1) DEFAULT 'N' COMMENT 'Enviar correo (3)',
  `llamada3` char(1) DEFAULT 'N' COMMENT 'Generar Llamada (3)',
  `mmcall3` char(1) DEFAULT 'N' COMMENT 'Área de MMCall (3)',
  `repetir3` char(1) DEFAULT 'N' COMMENT 'Repetir el escalamiento (3)',
  `veces3` int(3) DEFAULT '0' COMMENT 'Veces a escalar (3)',
  `escalar4` char(1) DEFAULT 'N' COMMENT 'Escalar 4to',
  `tiempo4` bigint(8) DEFAULT '0' COMMENT 'Tiempo de escalación (4)',
  `lista4` bigint(20) DEFAULT '0' COMMENT 'Lista de distribución (4)',
  `log4` char(1) DEFAULT 'N' COMMENT 'Generar LOG (4)',
  `sms4` char(1) DEFAULT 'N' COMMENT 'Enviar SMS (4)',
  `correo4` char(1) DEFAULT 'N' COMMENT 'Enviar correo (4)',
  `llamada4` char(1) DEFAULT 'N' COMMENT 'Generar Llamada (4)',
  `mmcall4` char(1) DEFAULT 'N' COMMENT 'Área de MMCall (4)',
  `repetir4` char(1) DEFAULT 'N' COMMENT 'Repetir el escalamiento (4)',
  `veces4` int(3) DEFAULT '0' COMMENT 'Veces a escalar (4)',
  `escalar5` char(1) DEFAULT 'N' COMMENT 'Escalar 5to',
  `tiempo5` bigint(8) DEFAULT '0' COMMENT 'Tiempo de escalación (5)',
  `lista5` bigint(20) DEFAULT '0' COMMENT 'Lista de distribución (5)',
  `log5` char(1) DEFAULT 'N' COMMENT 'Generar LOG (5)',
  `sms5` char(1) DEFAULT 'N' COMMENT 'Enviar SMS (5)',
  `correo5` char(1) DEFAULT 'N' COMMENT 'Enviar correo (5)',
  `llamada5` char(1) DEFAULT 'N' COMMENT 'Generar Llamada (5)',
  `mmcall5` char(1) DEFAULT 'N' COMMENT 'Área de MMCall (5)',
  `repetir5` char(1) DEFAULT 'N' COMMENT 'Repetir el escalamiento (5)',
  `veces5` int(3) DEFAULT '0' COMMENT 'Veces a escalar (5)',
  `repetir` char(1) DEFAULT 'N' COMMENT 'Repetir llamada',
  `repetir_tiempo` bigint(8) DEFAULT '0' COMMENT 'Repetir llamada (segundos)',
  `repetir_log` char(1) DEFAULT 'N' COMMENT 'Generar log en la repetición',
  `repetir_sms` char(1) DEFAULT 'N' COMMENT 'Enviar SMS en la repetición',
  `repetir_correo` char(1) DEFAULT 'N' COMMENT 'Enviar correo en la repetición',
  `repetir_llamada` char(1) DEFAULT 'N' COMMENT 'Generar llamada en la repetición',
  `repetir_mmcall` char(1) DEFAULT 'N' COMMENT 'Área de MMCall en la repetición',
  `repetir_veces` int(4) DEFAULT '0' COMMENT 'Número de veces a repetir',
  `estadistica` char(1) DEFAULT 'N' COMMENT 'Generar estadística',
  `escape_veces` int(2) DEFAULT '3' COMMENT 'Número de veces que se repetirá una llamada',
  `escape_accion` char(1) DEFAULT 'N' COMMENT 'Acción de Escape',
  `escape_mensaje` varchar(200) DEFAULT NULL COMMENT 'Mensaje a enviar si se agotan las llamadas',
  `escape_lista` bigint(20) DEFAULT '0' COMMENT 'Lista de distribución',
  `informar_resolucion` char(1) DEFAULT 'N' COMMENT 'Informar resolución',
  `cancelacion_mensaje` varchar(200) DEFAULT NULL COMMENT 'Mensaje de cancelación',
  `resolucion_mensaje` varchar(200) DEFAULT NULL COMMENT 'Mensaje de resolución',
  `tiempo0` bigint(8) DEFAULT '0' COMMENT 'Tiempo previo para alertas informativas',
  `mensaje` varchar(300) DEFAULT NULL,
  `titulo` varchar(100) DEFAULT NULL,
  `mensaje_mmcall` char(80) DEFAULT NULL,
  `mensaje1_mantener` char(1) DEFAULT 'N',
  `mensaje1` varchar(300) DEFAULT NULL,
  `titulo1` varchar(100) DEFAULT NULL,
  `mensaje1_mmcall` varchar(80) DEFAULT NULL,
  `mensaje2_mantener` char(1) DEFAULT 'N',
  `mensaje2` varchar(300) DEFAULT NULL,
  `titulo2` varchar(100) DEFAULT NULL,
  `mensaje2_mmcall` varchar(50) DEFAULT NULL,
  `mensaje3_mantener` char(1) DEFAULT 'N',
  `mensaje3` varchar(300) DEFAULT NULL,
  `titulo3` varchar(100) DEFAULT NULL,
  `mensaje3_mmcall` varchar(80) DEFAULT NULL,
  `mensaje4_mantener` char(1) DEFAULT 'N',
  `mensaje4` varchar(200) DEFAULT NULL,
  `titulo4` varchar(100) DEFAULT NULL,
  `mensaje4_mmcall` varchar(80) DEFAULT NULL,
  `mensaje5_mantener` char(1) DEFAULT 'N',
  `mensaje5` varchar(200) DEFAULT NULL,
  `titulo5` varchar(100) DEFAULT NULL,
  `mensaje5_mmcall` varchar(80) DEFAULT NULL,
  `repetir_mantener` char(1) DEFAULT 'N',
  `repetir_mensaje` varchar(200) DEFAULT NULL,
  `repetir_titulo` varchar(100) DEFAULT NULL,
  `repetir_mensaje_mmcall` varchar(80) DEFAULT NULL,
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(20) DEFAULT NULL COMMENT 'Creado por',
  `modificado` bigint(20) DEFAULT NULL COMMENT 'Modificado por',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`evento`,`proceso`,`estatus`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catálogo de alertas';

/*Table structure for table `cat_areas` */

DROP TABLE IF EXISTS `cat_areas`;

CREATE TABLE `cat_areas` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `planta` bigint(20) DEFAULT NULL COMMENT 'ID de la planta',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `referencia` varchar(30) DEFAULT NULL COMMENT 'Referencia con el sistema',
  `nombre` varchar(100) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `agrupador_1` bigint(20) DEFAULT '0' COMMENT 'Agrupador (1)',
  `agrupador_2` bigint(20) DEFAULT '0' COMMENT 'Agrupador (2)',
  `notas` varchar(300) DEFAULT NULL COMMENT 'Notas varias',
  `url_mmcall` varchar(1000) DEFAULT NULL COMMENT 'URL de MMCall',
  `audios_ruta` varchar(1000) DEFAULT NULL COMMENT 'Carpeta de audios',
  `audios_activar` char(1) DEFAULT 'N',
  `audios_prefijo` varchar(1000) DEFAULT NULL COMMENT 'Audio prefijo',
  `audios_general` char(1) DEFAULT 'S' COMMENT 'Grabar también en carpeta general',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(11) DEFAULT NULL COMMENT 'Creado por',
  `modificado` bigint(11) DEFAULT NULL COMMENT 'Modificado por',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catálogo de áreas';

/*Table structure for table `cat_choferes` */

DROP TABLE IF EXISTS `cat_choferes`;

CREATE TABLE `cat_choferes` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `nombre` varchar(100) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `agrupador_1` bigint(20) DEFAULT '0' COMMENT 'Agrupador (1)',
  `agrupador_2` bigint(20) DEFAULT '0' COMMENT 'Agrupador (2)',
  `notas` varchar(300) DEFAULT NULL COMMENT 'Notas varias',
  `correo` varchar(100) DEFAULT NULL COMMENT 'Correo electrónico',
  `referencia` varchar(30) DEFAULT NULL COMMENT 'URL de MMCall',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(11) DEFAULT NULL COMMENT 'Creado por',
  `modificado` bigint(11) DEFAULT NULL COMMENT 'Modificado por',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`nombre`),
  KEY `NewIndex2` (`referencia`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catálogo de áreas';

/*Table structure for table `cat_correos` */

DROP TABLE IF EXISTS `cat_correos`;

CREATE TABLE `cat_correos` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `nombre` varchar(60) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `para` varchar(2000) DEFAULT NULL COMMENT 'Lista de distribución',
  `copia` varchar(2000) DEFAULT NULL COMMENT 'Lista de distribución (con copia)',
  `oculta` varchar(2000) DEFAULT NULL COMMENT 'Lista de distribución (con copia oculta)',
  `titulo` varchar(200) DEFAULT NULL COMMENT 'Título del correo',
  `cuerpo` varchar(1000) DEFAULT NULL COMMENT 'Cuerpo del correo',
  `extraccion` varchar(40) DEFAULT NULL COMMENT 'Extracción del reporte',
  `ultimo_envio` datetime DEFAULT NULL COMMENT 'Fecha del último envío',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(20) DEFAULT '0' COMMENT 'Creado por',
  `modificado` bigint(20) DEFAULT '0' COMMENT 'Modificado por',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catálogo de correos';

/*Table structure for table `cat_defectos` */

DROP TABLE IF EXISTS `cat_defectos`;

CREATE TABLE `cat_defectos` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `referencia` varchar(20) DEFAULT NULL COMMENT 'Referencia',
  `nombre` varchar(100) DEFAULT NULL COMMENT 'Nombre/Descripción del defecto',
  `prefijo` varchar(30) DEFAULT NULL COMMENT 'Prefijo de la descripción',
  `agrupador1` bigint(20) DEFAULT NULL COMMENT 'ID del agrupador (1)',
  `agrupador2` bigint(20) DEFAULT NULL COMMENT 'ID del agrupador (2)',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `minimo` decimal(17,7) DEFAULT NULL COMMENT 'Minima cantidad a reportar',
  `maximo` decimal(17,7) DEFAULT NULL COMMENT 'Maxima cantidad a reportar',
  `defecto` char(1) DEFAULT NULL COMMENT 'Registro por defecto',
  `estatus` char(1) DEFAULT NULL COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(11) DEFAULT NULL COMMENT 'Creado por',
  `modificado` bigint(11) DEFAULT NULL COMMENT 'Modificado por',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Catálogo de defectos';

/*Table structure for table `cat_descargas` */

DROP TABLE IF EXISTS `cat_descargas`;

CREATE TABLE `cat_descargas` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `destino` bigint(20) DEFAULT '0' COMMENT 'ID del destino',
  `transporte` bigint(20) DEFAULT '0' COMMENT 'ID del transporte',
  `tipo` bigint(20) DEFAULT '0',
  `carga` bigint(20) DEFAULT '0' COMMENT 'ID del tipo de carga',
  `tiempo` bigint(10) DEFAULT '0' COMMENT 'Tiempo estimado',
  `monitorear` char(1) DEFAULT 'S',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(11) DEFAULT NULL COMMENT 'Creado por',
  `modificado` bigint(11) DEFAULT NULL COMMENT 'Modificado por',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`destino`,`transporte`,`tipo`,`carga`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;

/*Table structure for table `cat_destinos` */

DROP TABLE IF EXISTS `cat_destinos`;

CREATE TABLE `cat_destinos` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `planta` bigint(20) DEFAULT NULL COMMENT 'ID de la planta',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `referencia` varchar(30) DEFAULT NULL COMMENT 'Referencia con el sistema',
  `nombre` varchar(100) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `color` varchar(20) DEFAULT NULL COMMENT 'Color principal',
  `agrupador_2` bigint(20) DEFAULT '0' COMMENT 'Agrupador (2)',
  `notas` varchar(300) DEFAULT NULL COMMENT 'Notas varias',
  `url_mmcall` bigint(20) DEFAULT '0' COMMENT 'URL de MMCall',
  `audios_ruta` varchar(1000) DEFAULT NULL COMMENT 'Carpeta de audios',
  `audios_activar` char(1) DEFAULT 'N',
  `audios_prefijo` varchar(1000) DEFAULT NULL COMMENT 'Audio prefijo',
  `audios_general` char(1) DEFAULT 'S' COMMENT 'Grabar también en carpeta general',
  `inicial` char(1) DEFAULT 'N' COMMENT 'Destino inicial por defecto',
  `final` char(1) DEFAULT 'N' COMMENT 'Destino final por defecto',
  `patio_espera` char(1) DEFAULT 'N' COMMENT 'Patio de espera por defecto',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(11) DEFAULT NULL COMMENT 'Creado por',
  `modificado` bigint(11) DEFAULT NULL COMMENT 'Modificado por',
  PRIMARY KEY (`id`),
  KEY `inicial` (`inicial`,`estatus`),
  KEY `final` (`final`,`estatus`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catálogo de destinos';

/*Table structure for table `cat_distribucion` */

DROP TABLE IF EXISTS `cat_distribucion`;

CREATE TABLE `cat_distribucion` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `referencia` varchar(30) DEFAULT NULL COMMENT 'Referencia',
  `nombre` varchar(60) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `prefijo` varchar(30) DEFAULT NULL COMMENT 'Prefijo del registro',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `telefonos` varchar(2000) DEFAULT '' COMMENT 'Número de teléfono',
  `correos` varchar(2000) DEFAULT '' COMMENT 'Correo electrónico',
  `mmcall` varchar(2000) DEFAULT '' COMMENT 'Requesters de MMCall',
  `hora_desde` time DEFAULT NULL COMMENT 'Hora de inicio',
  `hora_hasta` time DEFAULT NULL COMMENT 'Hora de fin',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(20) DEFAULT NULL COMMENT 'Creado por',
  `modificado` bigint(20) DEFAULT NULL COMMENT 'Modificado por',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catálogo de listas de distribución';

/*Table structure for table `cat_equipos` */

DROP TABLE IF EXISTS `cat_equipos`;

CREATE TABLE `cat_equipos` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `planta` bigint(20) DEFAULT '0' COMMENT 'ID de la planta',
  `linea` bigint(20) DEFAULT '0' COMMENT 'ID de la linea',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `referencia` varchar(30) DEFAULT NULL COMMENT 'Referencia con el sistema',
  `nombre` varchar(100) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `prefijo` varchar(30) DEFAULT NULL COMMENT 'Prefijo del registro',
  `notas` varchar(300) DEFAULT NULL COMMENT 'Notas varias',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `agrupador1` bigint(11) DEFAULT NULL COMMENT 'ID del agrupador (1)',
  `agrupador2` bigint(11) DEFAULT NULL COMMENT 'ID del agrupador (2)',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(11) DEFAULT '0' COMMENT 'Creado por',
  `modificado` bigint(11) DEFAULT '0' COMMENT 'Modificado por',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Catálogo de equipos';

/*Table structure for table `cat_frases` */

DROP TABLE IF EXISTS `cat_frases`;

CREATE TABLE `cat_frases` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `linea` bigint(20) DEFAULT '0' COMMENT 'ID de la línea',
  `maquina` bigint(20) DEFAULT '0' COMMENT 'ID de la máquina',
  `area` bigint(20) DEFAULT '0' COMMENT 'ID del área',
  `falla` bigint(20) DEFAULT '0' COMMENT 'ID de la falla',
  `mensaje` varchar(200) DEFAULT '0' COMMENT 'Agrupador (2)',
  `largo` int(2) DEFAULT NULL COMMENT 'Largo del mensaje',
  `esperado` bigint(12) DEFAULT NULL COMMENT 'Tiempo esperado para reparación',
  `url_mmcall` varchar(250) DEFAULT NULL COMMENT 'URL de MMCall',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(11) DEFAULT NULL COMMENT 'Creado por',
  `modificado` bigint(11) DEFAULT NULL COMMENT 'Modificado por',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;

/*Table structure for table `cat_generales` */

DROP TABLE IF EXISTS `cat_generales`;

CREATE TABLE `cat_generales` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `tabla` int(6) DEFAULT NULL COMMENT 'ID de la tabla',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `nombre` varchar(100) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `prefijo` varchar(30) DEFAULT NULL COMMENT 'Prefijo del registro',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `id_relacionado` bigint(20) DEFAULT NULL COMMENT 'Relación entre registros',
  `url_mmcall` varchar(1000) DEFAULT NULL COMMENT 'URL de MMCall',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(20) DEFAULT '0' COMMENT 'Creado por',
  `modificado` bigint(20) DEFAULT '0' COMMENT 'Modificado por',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`id_relacionado`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Tablas generales';

/*Table structure for table `cat_grupos` */

DROP TABLE IF EXISTS `cat_grupos`;

CREATE TABLE `cat_grupos` (
  `id` bigint(11) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `referencia` varchar(20) DEFAULT NULL COMMENT 'Referencia',
  `nombre` varchar(100) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `prefijo` varchar(30) DEFAULT NULL COMMENT 'Prefijo del registro',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `agrupador1` bigint(11) DEFAULT NULL COMMENT 'ID del agrupador (1)',
  `agrupador2` bigint(11) DEFAULT NULL COMMENT 'ID del agrupador (2)',
  `estatus` char(1) DEFAULT NULL COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(11) DEFAULT NULL COMMENT 'Creado por',
  `modificado` bigint(11) DEFAULT NULL COMMENT 'Modificado por',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catálogo de grupos';

/*Table structure for table `cat_listas` */

DROP TABLE IF EXISTS `cat_listas`;

CREATE TABLE `cat_listas` (
  `id` bigint(11) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `referencia` varchar(20) DEFAULT NULL COMMENT 'Referencia',
  `nombre` varchar(100) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `prefijo` varchar(30) DEFAULT NULL COMMENT 'Prefijo del registro',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `clase` bigint(11) DEFAULT NULL COMMENT 'ID de la clase',
  `area` bigint(11) DEFAULT NULL COMMENT 'ID del área',
  `equipo` bigint(11) DEFAULT NULL COMMENT 'ID del equipo',
  `tiempo_llenado` bigint(6) DEFAULT NULL COMMENT 'Tiempo límite para el llenado (segundos)',
  `tiempo_alarma` char(1) DEFAULT NULL COMMENT 'Generar alarma al sobrepasar el límite por',
  `prioridad` int(2) DEFAULT NULL COMMENT 'Prioridad del registro',
  `horario` bigint(11) DEFAULT NULL COMMENT 'Calendario',
  `estatus` char(1) DEFAULT NULL COMMENT 'Estatus del registro',
  `creacion` timestamp NULL DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(11) DEFAULT NULL COMMENT 'Creado por',
  `modificado` bigint(11) DEFAULT NULL COMMENT 'Modificado por',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catálogo de listas de verificación';

/*Table structure for table `cat_medios` */

DROP TABLE IF EXISTS `cat_medios`;

CREATE TABLE `cat_medios` (
  `id` bigint(11) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `nombre` varchar(100) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `tipo` int(4) DEFAULT NULL COMMENT 'Tipo de comunicaión',
  `estatus` char(1) DEFAULT NULL COMMENT 'Estatus del registro',
  `creacion` timestamp NULL DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(11) DEFAULT NULL COMMENT 'Creado por',
  `modificado` bigint(11) DEFAULT NULL COMMENT 'Modificado por',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catálogo de medios de envío';

/*Table structure for table `cat_rutas` */

DROP TABLE IF EXISTS `cat_rutas`;

CREATE TABLE `cat_rutas` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `origen` bigint(20) DEFAULT '0' COMMENT 'ID del origen',
  `destino` bigint(20) DEFAULT '0' COMMENT 'ID del destino',
  `transporte` bigint(20) DEFAULT '0' COMMENT 'ID del transporte',
  `tipo` bigint(20) DEFAULT '0',
  `carga` bigint(20) DEFAULT '0' COMMENT 'ID del tipo de carga',
  `tiempo` bigint(10) DEFAULT '0' COMMENT 'Tiempo estimado',
  `monitorear` char(1) DEFAULT 'S',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(11) DEFAULT NULL COMMENT 'Creado por',
  `modificado` bigint(11) DEFAULT NULL COMMENT 'Modificado por',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`origen`,`destino`,`transporte`,`tipo`,`carga`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catálogo de rutas';

/*Table structure for table `cat_transportes` */

DROP TABLE IF EXISTS `cat_transportes`;

CREATE TABLE `cat_transportes` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `planta` bigint(20) DEFAULT NULL COMMENT 'ID de la planta',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `referencia` varchar(30) DEFAULT NULL COMMENT 'Referencia con el sistema',
  `nombre` varchar(100) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `agrupador_1` bigint(20) DEFAULT '0' COMMENT 'Agrupador (1)',
  `agrupador_2` bigint(20) DEFAULT '0' COMMENT 'Agrupador (2)',
  `url_mmcall` bigint(20) DEFAULT '0' COMMENT 'ID del recipiente',
  `notas` varchar(300) DEFAULT NULL COMMENT 'Notas varias',
  `disponibilidad` int(1) DEFAULT '0',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(20) DEFAULT '0' COMMENT 'Creado por',
  `modificado` bigint(20) DEFAULT '0' COMMENT 'Modificado por',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catálogo de líneas';

/*Table structure for table `cat_turnos` */

DROP TABLE IF EXISTS `cat_turnos`;

CREATE TABLE `cat_turnos` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `planta` bigint(20) DEFAULT '0' COMMENT 'ID de la planta',
  `nombre` varchar(100) CHARACTER SET latin1 DEFAULT NULL COMMENT 'Nombre del registro',
  `referencia` varchar(30) COLLATE utf8_bin DEFAULT NULL COMMENT 'Referencia',
  `inicia` time DEFAULT NULL COMMENT 'Hora de inicio',
  `termina` time DEFAULT NULL COMMENT 'Hora de Fin',
  `cambiodia` char(1) COLLATE utf8_bin DEFAULT 'N' COMMENT 'Pasa de un día a otro (S/N)',
  `especial` char(1) COLLATE utf8_bin DEFAULT 'N' COMMENT 'Turno especial (S/N)',
  `tipo` int(1) DEFAULT '0' COMMENT 'Tipo de turno (0=Diurno, 1=Matutino, 2=Nocturno, 3=Mixto)',
  `mover` int(1) DEFAULT '0' COMMENT 'Recorrer fecha de reporte (1=Dia anterior, 2=Dia posterior, 0=No recorrer)',
  `secuencia` int(2) DEFAULT NULL COMMENT 'Secuencia del turno',
  `estatus` char(1) COLLATE utf8_bin DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` timestamp NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(20) DEFAULT NULL COMMENT 'Usuario que creó el registro',
  `modificado` bigint(20) DEFAULT NULL COMMENT 'Usuario que modificó el registro',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Tabla de turnos';

/*Table structure for table `cat_usuarios` */

DROP TABLE IF EXISTS `cat_usuarios`;

CREATE TABLE `cat_usuarios` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `referencia` varchar(50) DEFAULT NULL COMMENT 'Referencia',
  `nombre` varchar(100) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `clave` varchar(255) DEFAULT NULL COMMENT 'Conraseña',
  `prefijo` varchar(50) DEFAULT NULL COMMENT 'Prefijo del registro',
  `notas` varchar(300) DEFAULT NULL COMMENT 'Notas varias',
  `rol` char(1) DEFAULT NULL COMMENT 'Rol de usuario',
  `politica` int(2) DEFAULT NULL COMMENT 'Política de contraseña',
  `operacion` char(1) DEFAULT 'N' COMMENT 'Ver todas las operaciones (S/N)',
  `linea` char(1) DEFAULT 'N' COMMENT 'Ver todas las líneas (S/N)',
  `maquina` char(1) DEFAULT 'N' COMMENT 'Ver todas las máquinas (S/N)',
  `area` char(1) DEFAULT 'N' COMMENT 'Ver todas las áreas (S/N)',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `admin` char(1) DEFAULT 'N' COMMENT 'Es administrador',
  `calidad` char(1) DEFAULT 'N' COMMENT 'Puede hacer inspecciones de calidad',
  `reversos` char(1) DEFAULT 'N' COMMENT 'Puede hacer reversos',
  `programacion` char(1) DEFAULT 'N' COMMENT 'Ver programación(lectura)',
  `inventario` char(1) DEFAULT 'N' COMMENT 'Ver inventario',
  `tecnico` char(1) DEFAULT 'N' COMMENT 'Rol de técnico',
  `planta` bigint(20) DEFAULT '0' COMMENT 'ID de la plant',
  `apagar_alertas` char(1) DEFAULT 'N' COMMENT 'El usuario puede cancelar alertas',
  `ver_alertas` char(1) DEFAULT 'N' COMMENT 'El usuario puede ver las alertas',
  `cerrar_al_ejecutar` char(1) DEFAULT 'N' COMMENT 'Cerrar menú al ajecutar',
  `vista_resumida_fallas` char(1) DEFAULT NULL COMMENT 'Vista resumida de las fallas',
  `confirmar_reparacion` char(1) DEFAULT 'N' COMMENT 'Puede confirmar una reparación',
  `ultima_pantalla` int(2) DEFAULT '0' COMMENT 'Última pantalla usada',
  `inicializada` char(1) DEFAULT 'S' COMMENT 'Contraseña inicializada',
  `preferencias_andon` varchar(50) DEFAULT '00000000000000000000000000000000000000000000000000' COMMENT 'Preferecia de Andon',
  `departamento` bigint(20) DEFAULT '0' COMMENT 'ID del Departamento',
  `compania` bigint(20) DEFAULT '0' COMMENT 'ID de la compañía',
  `tema` int(3) DEFAULT '0' COMMENT 'ID del tema',
  `turno` bigint(20) DEFAULT '0' COMMENT 'Turno asociado',
  `claveant1` varchar(255) DEFAULT NULL COMMENT 'Últimas claves usadas',
  `claveant2` varchar(255) DEFAULT NULL COMMENT 'Últimas claves usadas',
  `claveant3` varchar(255) DEFAULT NULL COMMENT 'Últimas claves usadas',
  `claveant4` varchar(255) DEFAULT NULL COMMENT 'Últimas claves usadas',
  `claveant5` varchar(255) DEFAULT NULL COMMENT 'Últimas claves usadas',
  `ucambio` date DEFAULT NULL COMMENT 'Fecha del último cambio',
  `entrada` datetime DEFAULT NULL COMMENT 'Fecha de la última entrada',
  `salida` datetime DEFAULT NULL COMMENT 'Fecha de la última salida',
  `ultimo_reporte` int(1) DEFAULT '0' COMMENT 'Último reporte consultado',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(11) DEFAULT '0' COMMENT 'Creado por',
  `modificado` bigint(11) DEFAULT '0' COMMENT 'Modificado por',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catalogo de usuarios';

/*Table structure for table `cat_vehiculos` */

DROP TABLE IF EXISTS `cat_vehiculos`;

CREATE TABLE `cat_vehiculos` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `linea` bigint(20) DEFAULT '0' COMMENT 'ID de la línea (0=Suelta)',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `referencia` varchar(30) DEFAULT NULL COMMENT 'Referencia con el sistema',
  `nombre` varchar(100) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `imagen` varchar(255) DEFAULT NULL COMMENT 'Imagen a mostrar',
  `tipo` bigint(20) DEFAULT '0' COMMENT 'Tipo de máquina',
  `notas` varchar(300) DEFAULT NULL COMMENT 'Notas',
  `agrupador_1` bigint(20) DEFAULT '0' COMMENT 'Agrupador (1)',
  `agrupador_2` bigint(20) DEFAULT '0' COMMENT 'Agrupador (2)',
  `url_mmcall` bigint(20) DEFAULT '0' COMMENT 'Recipiente',
  `chofer` bigint(20) DEFAULT '0',
  `carga` bigint(20) DEFAULT '0' COMMENT 'Carga acistumbrada',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(20) DEFAULT '0' COMMENT 'Creado por',
  `modificado` bigint(20) DEFAULT '0' COMMENT 'Modificado por',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`nombre`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catálogo de máquinas';

/*Table structure for table `configuracion` */

DROP TABLE IF EXISTS `configuracion`;

CREATE TABLE `configuracion` (
  `id` bigint(11) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `planta` varchar(100) DEFAULT NULL COMMENT 'Nombre de la planta',
  `rfc` varchar(30) DEFAULT NULL COMMENT 'RFC',
  `licencia` varchar(30) DEFAULT NULL COMMENT 'Número de licencia',
  `tiempo` bigint(8) DEFAULT NULL COMMENT 'Tiempo de revisión',
  `correo_cuenta` varchar(100) DEFAULT NULL COMMENT 'Perfil de correo',
  `correo_puerto` varchar(100) DEFAULT NULL COMMENT 'Puerto',
  `correo_ssl` char(1) DEFAULT NULL COMMENT 'Seguridad SSL',
  `correo_clave` varchar(100) DEFAULT NULL COMMENT 'Contraseña',
  `correo_host` varchar(100) DEFAULT NULL COMMENT 'Host',
  `flag_agregar` char(1) DEFAULT NULL COMMENT 'Flag de que se agregó una falla',
  `ejecutando_desde` datetime DEFAULT NULL COMMENT 'Ejecutando desde',
  `ultima_falla` bigint(20) DEFAULT NULL COMMENT 'Último ID de falla revisado',
  `ultima_revision` datetime DEFAULT NULL COMMENT 'Fecha de la última revisión',
  `revisar_cada` bigint(8) DEFAULT '0' COMMENT 'Revisar cada n segundos',
  `utilizar_arduino` char(1) DEFAULT NULL COMMENT 'Usar arduino?',
  `puerto_comm1` varchar(10) DEFAULT NULL COMMENT 'Puerto comm (1)',
  `puerto_comm1_par` varchar(100) DEFAULT NULL COMMENT 'Parámetros Puerto comm (1)',
  `puerto_comm2` varchar(10) DEFAULT NULL COMMENT 'Puerto comm (2)',
  `puerto_comm2_par` varchar(100) DEFAULT NULL COMMENT 'Parámetros Puerto comm (2)',
  `puerto_comm3` varchar(10) DEFAULT NULL COMMENT 'Puerto comm (3)',
  `puerto_comm3_par` varchar(100) DEFAULT NULL COMMENT 'Parámetros Puerto comm (3)',
  `puerto_comm4` varchar(10) NOT NULL COMMENT 'Puerto comm (4)',
  `puerto_comm4_par` varchar(100) DEFAULT NULL COMMENT 'Parámetros Puerto comm (4)',
  `puerto_comm5` varchar(10) DEFAULT NULL COMMENT 'Puerto comm (5)',
  `puerto_comm5_par` varchar(100) DEFAULT NULL COMMENT 'Parámetros Puerto comm (5)',
  `puerto_comm6` varchar(10) DEFAULT NULL COMMENT 'Puerto comm (6)',
  `puerto_comm6_par` varchar(100) DEFAULT NULL COMMENT 'Parámetros Puerto comm (6)',
  `ruta_sms` varchar(500) DEFAULT NULL COMMENT 'Ruta para los SMS',
  `ruta_audios` varchar(500) DEFAULT NULL COMMENT 'Ruta para las llamadas',
  `optimizar_llamada` char(1) DEFAULT NULL COMMENT 'Optimiza las llamadas',
  `optimizar_sms` char(1) DEFAULT NULL COMMENT 'Optimiza los SMS',
  `optimizar_correo` char(1) DEFAULT NULL COMMENT 'Optimiza los correos',
  `optimizar_mmcall` char(1) DEFAULT NULL COMMENT 'Optimiza las llamadas a MMCall',
  `mantener_prioridad` char(1) DEFAULT NULL COMMENT 'Mantener prioridad en la optimización',
  `voz_predeterminada` varchar(255) DEFAULT NULL COMMENT 'Voz predeterminada',
  `escape_mmcall` char(1) DEFAULT NULL COMMENT 'Escape para MMCall',
  `escape_mmcall_mensaje` varchar(200) DEFAULT NULL COMMENT 'Mensaje a enviar MMCall',
  `escape_mmcall_lista` bigint(11) DEFAULT NULL COMMENT 'Lista de distribución (requesters ocupados)',
  `escape_mmcall_cancelar` char(1) DEFAULT NULL COMMENT 'Cancelar el llamado a MMCall',
  `escape_llamadas` int(1) DEFAULT NULL COMMENT 'Número de veces a llamar',
  `escape_accion` char(1) DEFAULT NULL COMMENT 'Acción a tomar',
  `escape_lista` bigint(11) DEFAULT NULL COMMENT 'Lista de distribución',
  `escape_mensaje` varchar(200) DEFAULT NULL COMMENT 'Mensaje a enviar',
  `escape_mensaje_propio` char(1) DEFAULT NULL COMMENT 'Enviar mensaje al propio móvil',
  `veces_reproducir` int(1) DEFAULT '1' COMMENT 'Número de veces que se repeduce un audio',
  `gestion_log` char(6) DEFAULT NULL COMMENT 'Año y mes de la última gestión',
  `gestion_meses` int(4) DEFAULT NULL COMMENT 'Número de meses en línea',
  `correo_titulo_falla` char(1) DEFAULT NULL COMMENT 'Mantener el título de la falla',
  `correo_titulo` varchar(100) DEFAULT NULL COMMENT 'Título opcional del correo',
  `correo_cuerpo` varchar(200) DEFAULT NULL COMMENT 'Cuerpo del correo',
  `correo_firma` varchar(100) DEFAULT NULL COMMENT 'Firma del correo',
  `timeout_llamadas` int(4) DEFAULT NULL COMMENT 'Time Out para llamadas',
  `timeout_sms` int(4) DEFAULT NULL COMMENT 'Time Out para SMS',
  `traducir` char(1) DEFAULT NULL COMMENT 'Traducir mensajes de voz',
  `tiempo_corte` bigint(8) DEFAULT '0' COMMENT 'Tiempo del corte en minutos',
  `ultimo_corte` datetime DEFAULT NULL COMMENT 'Fecha y hora del último corte',
  `bajo_hasta` int(3) DEFAULT NULL,
  `bajo_color` varchar(20) DEFAULT NULL,
  `bajo_etiqueta` varchar(30) DEFAULT NULL,
  `medio_hasta` int(3) DEFAULT NULL,
  `medio_color` varchar(20) DEFAULT NULL,
  `medio_etiqueta` varchar(30) DEFAULT NULL,
  `alto_color` varchar(20) DEFAULT NULL,
  `alto_etiqueta` varchar(30) DEFAULT NULL,
  `noatendio_color` varchar(20) DEFAULT NULL,
  `noatendio_etiqueta` varchar(30) DEFAULT NULL,
  `escaladas_color` varchar(20) DEFAULT NULL,
  `escaladas_etiqueta` varchar(30) DEFAULT NULL,
  `flag_monitor` char(1) DEFAULT 'N' COMMENT 'Flag para leer desde el monitor',
  `ruta_archivos_enviar` varchar(500) DEFAULT NULL COMMENT 'Ruta de los archivos a enviar por correo',
  `server_mmcall` varchar(100) DEFAULT NULL COMMENT 'Server para MMCall',
  `cad_consolidado` varchar(20) DEFAULT NULL COMMENT 'Cadena de la consolidado',
  `ruta_imagenes` varchar(500) DEFAULT NULL COMMENT 'Ruta de imágenes',
  `tiempo_imagen` int(4) DEFAULT NULL COMMENT 'Tiempo entre imagenes',
  `graficas_seleccion` varchar(100) DEFAULT NULL COMMENT 'Gráficas a reportar',
  `graficas_duracion` varchar(100) DEFAULT NULL,
  `timeout_fallas` int(10) DEFAULT '0' COMMENT 'Timeout para crear alerta',
  `avisar_segundos` bigint(4) DEFAULT NULL COMMENT 'Avisar con tantos segundos antes',
  `color_aviso` varchar(20) DEFAULT NULL COMMENT 'Color del aviso',
  `contar_post` char(1) DEFAULT NULL COMMENT 'Contar luego de vencer el tiempo',
  `color_post` varchar(20) DEFAULT NULL COMMENT 'Color del post',
  `escaner_prefijo` varchar(10) DEFAULT NULL COMMENT 'Prefijo del escaner',
  `escaner_sufijo` varchar(10) DEFAULT NULL COMMENT 'Sufijo del escaner',
  `tiempo_holgura` int(4) DEFAULT '0',
  `tiempo_entre_lecturas` int(4) DEFAULT NULL COMMENT 'Tiempo entre lecturas (seg)',
  `tiempo_escaner` int(4) DEFAULT '0' COMMENT 'Tiempo de espera entre milesegundos',
  `largo_escaner` int(2) DEFAULT '0' COMMENT 'Largo mínimo de la frase del escaner',
  `lote_inspeccion_clave` char(1) DEFAULT NULL COMMENT 'Requiere clave el envío de lotes a calidad',
  `reverso_permitir` char(1) DEFAULT NULL COMMENT 'Permitir reverso? (S/N/C)',
  `reverso_referencia` varchar(20) DEFAULT NULL COMMENT 'Referencia para reversar',
  `dias_programacion` int(4) DEFAULT '0' COMMENT 'Días atras para la programación',
  `holgura_reprogramar` int(6) DEFAULT '0' COMMENT 'Holgura en segundos para reprogramar',
  `tipo_flujo` char(1) DEFAULT NULL COMMENT 'Tipo de flujo',
  `estimado_productividad` int(3) DEFAULT NULL COMMENT 'Estimado productividad',
  `confirmar_mensaje_mantto` char(1) DEFAULT 'S' COMMENT 'Confirmar mensaje de configuración',
  `url_mmcall` varchar(1000) DEFAULT NULL COMMENT 'URL de MMCall',
  `accion_mmcall` char(1) DEFAULT 'N' COMMENT 'Acumula MMCall',
  `tiempo_reporte` bigint(12) DEFAULT NULL COMMENT 'Tiempo para cerrar el reporte',
  `be_alarmas_correos` char(1) DEFAULT 'N' COMMENT 'Enviar alarmas por correo',
  `be_alarmas_mmcall` char(1) DEFAULT 'N' COMMENT 'Enviar alarmas por MMCall',
  `be_alarmas_llamadas` char(1) DEFAULT 'N' COMMENT 'Enviar alarmas por llamada',
  `be_alarmas_sms` char(1) DEFAULT 'N' COMMENT 'Enviar alarmas por SMS',
  `be_envio_reportes` char(1) DEFAULT 'N' COMMENT 'Enviar reportes',
  `be_revision_correos` int(6) DEFAULT '0' COMMENT 'Envíos de correos (segundos)',
  `be_revision_mmcall` int(6) DEFAULT '0' COMMENT 'Envíos de MMCall (segundos)',
  `be_revision_arduino` int(6) DEFAULT '0' COMMENT 'Envíos de Arduino (segundos)',
  `be_log_lineas` int(4) DEFAULT '0' COMMENT 'Líneas a visulizar en en log',
  `be_log_activar` char(1) DEFAULT 'N' COMMENT 'Activar el log?',
  `maximo_largo_mmcall` int(2) DEFAULT '0' COMMENT 'Máximo de caracteres para mensajes a MMCall',
  `separador_mail` char(1) DEFAULT ';' COMMENT 'Separador de correos',
  `limitar_inicio` int(4) DEFAULT '0' COMMENT 'Segundos a limitar el inicio de sesión',
  `limitar_respuestas` int(4) DEFAULT '0' COMMENT 'Segundos a limitar respuestas',
  `recuperar_sesion` char(1) DEFAULT 'N' COMMENT 'Recuperar sesión luego de téenico',
  `visor_revisar_cada` int(6) DEFAULT '0' COMMENT 'Visor: Segundos para revisar',
  `logo_arriba` int(6) DEFAULT '0' COMMENT 'Arriba del logo',
  `logo_izquierda` int(6) DEFAULT '0' COMMENT 'Izquierda del logo',
  `logo_ancho` int(6) DEFAULT '0' COMMENT 'Ancho del logo',
  `logo_alto` int(6) DEFAULT '0' COMMENT 'Alto del logo',
  `logo_ruta` varchar(500) DEFAULT NULL COMMENT 'Ruta del logo',
  `mapa_alineacion` varchar(30) DEFAULT NULL COMMENT 'Tipo de alineación del mapa',
  `mapa_fondo` varchar(20) DEFAULT NULL COMMENT 'Color de fondo del mapa',
  `confirmar_reparacion` char(1) DEFAULT 'N' COMMENT 'Confirmar reparación',
  `ruta_programa_mapa` varchar(1000) DEFAULT NULL COMMENT 'Programa para actualizar el mapa',
  `mapa_delay` int(4) DEFAULT NULL COMMENT 'Segundos a mostrar múltiples mapas',
  `mapa_rotacion` int(1) DEFAULT '0' COMMENT 'Muestra de mapas (0: Solo con falla, 1: todos)',
  `tema_principal` int(3) DEFAULT NULL COMMENT 'ID del tema principal',
  `mapa_ultimo` datetime DEFAULT NULL COMMENT 'Fecha de la última actualización del mapa',
  `mapa_solicitud` char(1) DEFAULT NULL COMMENT 'Solicitar actualización de mapa',
  `tema_permitir_crear` char(1) DEFAULT NULL COMMENT 'Permitir a los usuarios crear temas',
  `tema_permitir_cambio` char(1) DEFAULT NULL COMMENT 'Permitir a los usuarios cambiar temas',
  `turno_modo` int(1) DEFAULT NULL COMMENT 'Modo de cambiar de turno (0: Manual, 1: Sugerir, 2: Prompt, 3: Automático)',
  `ver_logo_cronos` char(1) DEFAULT 'S' COMMENT 'Visualizar el logo de cronos',
  `url_cronos` varchar(300) DEFAULT NULL COMMENT 'URL de la página de cronos',
  `audios_activar` char(1) DEFAULT 'N' COMMENT 'Activar generación de audios generales',
  `audios_ruta` varchar(1000) DEFAULT NULL COMMENT 'Ruta a guardar',
  `audios_prefijo` varchar(1000) DEFAULT NULL COMMENT 'Ruta del audio prefijo',
  `ver_ayuda` char(1) DEFAULT 'S' COMMENT 'Ver ayuda en andon',
  `ip_localhost` varchar(100) DEFAULT NULL COMMENT 'IP local (primaria)',
  `mensaje` varchar(300) DEFAULT NULL COMMENT 'Mensaje a enviar como audio',
  `ultimo_audio` datetime DEFAULT NULL COMMENT 'Fecha de la ultima generación de audios',
  `audios_escalamiento` int(1) DEFAULT '0' COMMENT 'Modo de escalamiento de los audios',
  `audios_resolucion` varchar(100) DEFAULT NULL COMMENT 'Informar la resolución',
  `modulo_produccion` char(1) DEFAULT 'N' COMMENT 'Ver hoja de Excel',
  `usar_clave_falla` char(1) DEFAULT NULL COMMENT 'Usar clave de falla',
  `mostrar_numero` char(1) DEFAULT NULL COMMENT 'Mostrar el número de reporte',
  `turno_actual` bigint(20) DEFAULT '0' COMMENT 'ID del turno',
  `dimension` varchar(20) DEFAULT NULL COMMENT 'Dimension de la aplicación',
  `tiempo_andon` int(6) DEFAULT '0' COMMENT 'Tiempo de espera etre repeticionesANDOn',
  `lectura_pendiente` int(1) DEFAULT '0' COMMENT 'Lectura pendiente?',
  `modulo_oee` char(1) DEFAULT NULL COMMENT 'Modulo de OEE',
  `ver_nombre_planta` char(1) DEFAULT NULL COMMENT 'Ver titulo del mapa',
  `oee_mostrar_paro` char(1) DEFAULT 'N' COMMENT 'Mostrar mensaje de paros',
  `carrusel_oee` int(1) DEFAULT '0' COMMENT 'Tipo de carrusel OEE (0=Todas las máquinas, 1=Sólo maquinas produciendo)',
  `carrusel_tiempo` bigint(6) DEFAULT '0' COMMENT 'Tiempo de visualización de la máquina en segundos',
  `kanban` char(1) DEFAULT 'N' COMMENT 'Activar módulo de kanban',
  `smed` char(1) DEFAULT 'N' COMMENT 'Activar opción de SMED',
  `esperado_oee` decimal(6,2) DEFAULT '0.00',
  `esperado_ftq` decimal(6,2) DEFAULT '0.00',
  `esperado_efi` decimal(6,2) DEFAULT '0.00',
  `esperado_dis` decimal(6,2) DEFAULT '0.00',
  `esperado_mttr` decimal(15,5) DEFAULT '0.00000',
  `esperado_mtbf` decimal(15,5) DEFAULT '0.00000',
  `modulo_wip` char(1) DEFAULT 'N' COMMENT 'Módulo WIP',
  `reportes_inicial` int(1) DEFAULT '0' COMMENT 'Automaticamente ver todos',
  `turno_oee` bigint(20) DEFAULT '0',
  `mensaje_mmcall` varchar(60) DEFAULT NULL,
  `andon_repeticiones` int(3) DEFAULT NULL,
  `agregar_movil` char(1) DEFAULT 'S',
  `adicionales` char(1) DEFAULT 'N',
  `pagers_val` char(1) DEFAULT 'S',
  `asignar_destino` char(1) DEFAULT 'S',
  `asignar_caseta` char(1) DEFAULT 'N',
  `asignar_automatico` char(1) DEFAULT 'N',
  `visor_mostrar` int(1) NOT NULL DEFAULT '0',
  `agregar_transporte` char(1) NOT NULL DEFAULT 'N',
  `ultima_actualizacion` datetime DEFAULT NULL,
  PRIMARY KEY (`id`,`puerto_comm4`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 MIN_ROWS=1 MAX_ROWS=1;


/*Table structure for table `consultas_cab` */

DROP TABLE IF EXISTS `consultas_cab`;

CREATE TABLE `consultas_cab` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `nombre` varchar(50) COLLATE utf8_bin DEFAULT NULL COMMENT 'Nombre de la consulta',
  `usuario` bigint(20) DEFAULT NULL COMMENT 'ID del usuario',
  `publico` char(1) COLLATE utf8_bin DEFAULT NULL COMMENT 'Es una consulta pública?',
  `periodo` char(2) COLLATE utf8_bin DEFAULT NULL COMMENT 'Período a usar',
  `desde` datetime DEFAULT NULL COMMENT 'Fecha desde',
  `hasta` datetime DEFAULT NULL COMMENT 'Fecha de hasta',
  `defecto` char(1) COLLATE utf8_bin DEFAULT NULL COMMENT 'Consulta por defecto',
  `filtrooper` char(1) COLLATE utf8_bin DEFAULT 'S' COMMENT 'Total operaciones filtradas',
  `filtronpar` char(1) COLLATE utf8_bin DEFAULT 'S' COMMENT 'Total Números de parte filtradas',
  `filtrolin` char(1) COLLATE utf8_bin DEFAULT 'S' COMMENT 'Total Números de parte filtradas',
  `filtromaq` char(1) COLLATE utf8_bin DEFAULT 'S' COMMENT 'Total Números de parte filtradas',
  `filtroare` char(1) COLLATE utf8_bin DEFAULT 'S' COMMENT 'Total Números de parte filtradas',
  `filtrofal` char(1) COLLATE utf8_bin DEFAULT 'S' COMMENT 'Total Números de parte filtradas',
  `filtromq1` char(10) COLLATE utf8_bin DEFAULT 'S' COMMENT 'Total Números de parte filtradas',
  `filtromq2` char(10) COLLATE utf8_bin DEFAULT 'S' COMMENT 'Total Números de parte filtradas',
  `filtromq3` char(10) COLLATE utf8_bin DEFAULT 'S' COMMENT 'Total Números de parte filtradas',
  `filtrofa1` char(10) COLLATE utf8_bin DEFAULT 'S' COMMENT 'Total Números de parte filtradas',
  `filtrofa2` char(10) COLLATE utf8_bin DEFAULT 'S' COMMENT 'Total Números de parte filtradas',
  `filtrotec` char(10) COLLATE utf8_bin DEFAULT 'S' COMMENT 'Total Números de parte filtradas',
  `filtrotur` char(10) COLLATE utf8_bin DEFAULT 'S',
  `filtroord` char(10) COLLATE utf8_bin DEFAULT 'S',
  `visualizar` char(1) COLLATE utf8_bin DEFAULT 'N' COMMENT 'Visualizar todo',
  `general` char(1) COLLATE utf8_bin DEFAULT 'N' COMMENT 'Consulta general',
  `actualizacion` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`usuario`),
  KEY `NewIndex2` (`usuario`,`general`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Tabla de consultas';

/*Table structure for table `consultas_det` */

DROP TABLE IF EXISTS `consultas_det`;

CREATE TABLE `consultas_det` (
  `consulta` bigint(20) DEFAULT NULL COMMENT 'ID de la consulta',
  `tabla` int(6) DEFAULT NULL COMMENT 'ID de la tabla',
  `valor` bigint(20) DEFAULT NULL COMMENT 'ID del valor',
  KEY `NewIndex1` (`consulta`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Detalle de la consulta';

/*Table structure for table `control` */

DROP TABLE IF EXISTS `control`;

CREATE TABLE `control` (
  `fecha` varchar(10) NOT NULL COMMENT 'Fecha  y hora del envío',
  `mensajes` int(8) DEFAULT NULL COMMENT 'Mensajes enviados',
  PRIMARY KEY (`fecha`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Control de mensajes enviados';

/*Table structure for table `det_correo` */

DROP TABLE IF EXISTS `det_correo`;

CREATE TABLE `det_correo` (
  `correo` bigint(20) DEFAULT NULL COMMENT 'ID del correo',
  `reporte` bigint(20) DEFAULT NULL COMMENT 'ID del reporte'
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Detalle de los coreos a enviar';

/*Table structure for table `det_disponibilidad` */

DROP TABLE IF EXISTS `det_disponibilidad`;

CREATE TABLE `det_disponibilidad` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `calendario` bigint(20) DEFAULT NULL COMMENT 'ID del calendario',
  `equipo` bigint(20) DEFAULT NULL COMMENT 'ID del equipo',
  `fecha` date DEFAULT NULL COMMENT 'Fecha especifica',
  `disponibilidad` bigint(5) DEFAULT NULL COMMENT 'Disponibilidad en segundos',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`calendario`,`equipo`,`fecha`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Disponibilidad por equipo y fecha';

/*Table structure for table `det_distribucion` */

DROP TABLE IF EXISTS `det_distribucion`;

CREATE TABLE `det_distribucion` (
  `id` bigint(11) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `distribucion` bigint(11) NOT NULL COMMENT 'ID de la lista de distribución',
  `orden` int(4) NOT NULL COMMENT 'Línea en la lista',
  `tipo` int(2) DEFAULT NULL COMMENT 'Tipo de lista (0: Móvil-Llamada, 10: Móvil-SMS, 30: Móvil-LLamada y SMS, 40: Correo electrónico, 50: Ärea de MMCall)',
  `cadena` varchar(255) DEFAULT NULL COMMENT 'Cadena',
  `alias` varchar(30) DEFAULT NULL COMMENT 'Alias',
  `estatus` char(1) DEFAULT NULL COMMENT 'Estatus',
  PRIMARY KEY (`id`,`distribucion`,`orden`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Detalle de la lista de distribucón';

/*Table structure for table `det_estacion` */

DROP TABLE IF EXISTS `det_estacion`;

CREATE TABLE `det_estacion` (
  `estacion` bigint(11) NOT NULL COMMENT 'ID de la estación',
  `secuencia` int(6) DEFAULT NULL COMMENT 'Secuencia en la pantalla',
  `equipo` bigint(11) NOT NULL COMMENT 'ID del equipo',
  PRIMARY KEY (`estacion`,`equipo`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Detalle de las estaciones';

/*Table structure for table `det_lista` */

DROP TABLE IF EXISTS `det_lista`;

CREATE TABLE `det_lista` (
  `id` bigint(11) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `lista` bigint(11) NOT NULL COMMENT 'ID de la lista',
  `variable` bigint(11) NOT NULL COMMENT 'ID de la variable',
  `orden` int(4) DEFAULT NULL COMMENT 'Orden en la lista',
  `deorigen` char(1) DEFAULT NULL COMMENT 'Tomar los datos de origen',
  `requerido` char(1) DEFAULT NULL COMMENT 'Campo requerido',
  `notas` varchar(500) DEFAULT NULL COMMENT 'Notas de la variable',
  `tipo` int(2) DEFAULT NULL COMMENT 'Tipo de valor (10=numérico, 20=Si/NO, 30=Tabla)',
  `tabla` char(1) DEFAULT NULL COMMENT 'Tomar valor de una tabla',
  `idtabla` bigint(11) DEFAULT NULL COMMENT 'ID de la tabla',
  `unidad` bigint(11) DEFAULT NULL COMMENT 'ID de la unidad de medida',
  `permitido_min` decimal(30,10) DEFAULT NULL COMMENT 'Valor mínimo',
  `permitido_max` decimal(30,10) DEFAULT NULL COMMENT 'Valor máximo',
  `alarma_min` decimal(30,10) DEFAULT NULL COMMENT 'Valor mínimo para generar alarma',
  `alarma_max` decimal(30,10) DEFAULT NULL COMMENT 'Valor máximo para generar alarma',
  `alarma_supervision` char(1) DEFAULT NULL COMMENT 'Requiere supervisión',
  `alarma_regla` char(1) DEFAULT NULL COMMENT 'Requiere regla',
  `alarma_sino` char(1) DEFAULT NULL COMMENT 'Alarma Si/No',
  `color` varchar(20) DEFAULT NULL COMMENT 'Color de fondo',
  `resaltada` char(1) DEFAULT NULL COMMENT 'Resaltar variable',
  `mostrar_rango` char(1) DEFAULT NULL COMMENT 'Mostrar rango en pantalla',
  `confirmar_respuesta` char(1) DEFAULT NULL COMMENT 'Confirmar la respuesta',
  `estatus` char(1) DEFAULT NULL COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha en que se agregó',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha en que se modificó',
  `creado` bigint(11) DEFAULT NULL COMMENT 'Usuario que agregó',
  `modificado` bigint(11) DEFAULT NULL COMMENT 'Usuario que modificó',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`lista`,`variable`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Detalle de listas de verificación';

/*Table structure for table `dias` */

DROP TABLE IF EXISTS `dias`;

CREATE TABLE `dias` (
  `fecha` date NOT NULL COMMENT 'Fecha del día',
  `dia` int(1) NOT NULL,
  PRIMARY KEY (`fecha`,`dia`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Tabla de dias';

/*Table structure for table `figuras` */

DROP TABLE IF EXISTS `figuras`;

CREATE TABLE `figuras` (
  `id` varchar(50) NOT NULL,
  `mapa_id` int(11) NOT NULL,
  `objeto_id` varchar(50) NOT NULL,
  `x` float NOT NULL DEFAULT '0',
  `y` float NOT NULL DEFAULT '0',
  `ancho` float NOT NULL DEFAULT '0',
  `largo` float NOT NULL DEFAULT '0',
  `rotacion` float NOT NULL DEFAULT '0',
  `idx` float NOT NULL DEFAULT '0',
  `idy` float NOT NULL DEFAULT '0',
  `rotacion_texto` float NOT NULL DEFAULT '0',
  `tipo_id` int(11) NOT NULL,
  `color_borde` varchar(20) NOT NULL DEFAULT '#FFF',
  `alfa_borde` float DEFAULT '1',
  `color_fondo` varchar(20) NOT NULL DEFAULT '#FFF',
  `alfa_fondo` float DEFAULT '1',
  `color_texto` varchar(20) NOT NULL DEFAULT '#000',
  `fuente` varchar(50) NOT NULL DEFAULT 'Sans-serif',
  `tamano_fuente` float NOT NULL DEFAULT '0',
  `fuente_italica` tinyint(4) NOT NULL DEFAULT '0',
  `fuente_negrita` tinyint(4) NOT NULL DEFAULT '0',
  `archivo` varchar(300) NOT NULL DEFAULT '',
  `status_asociado` int(11) NOT NULL,
  `ultima_actualizacion` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `mensaje` varchar(300) NOT NULL DEFAULT '',
  `efecto` varchar(2) NOT NULL DEFAULT '',
  `ancho_borde` float NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`,`mapa_id`),
  KEY `mapa_id` (`mapa_id`,`objeto_id`),
  KEY `tipo_id` (`tipo_id`),
  KEY `status_asociado` (`status_asociado`),
  KEY `figuras_ibfk_3` (`status_asociado`,`mapa_id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

/*Table structure for table `horarios` */

DROP TABLE IF EXISTS `horarios`;

CREATE TABLE `horarios` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `clase` int(1) DEFAULT '0' COMMENT 'Clase de horario (0=Disponibilidad de producción, 1=Mantenimiento)',
  `tipo` char(1) DEFAULT 'S' COMMENT 'Tipo de horario (S/N)',
  `proceso` bigint(20) DEFAULT '0' COMMENT 'ID del proceso',
  `maquina` bigint(20) DEFAULT '0' COMMENT 'ID de la maquina',
  `dia` int(1) DEFAULT '0' COMMENT 'Dia de semana (9=por fecha)',
  `fecha` date DEFAULT NULL COMMENT 'Fecha a revisar',
  `desde` time DEFAULT NULL COMMENT 'Hora desde',
  `hasta` time DEFAULT NULL COMMENT 'Hora hasta',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Horarios';

/*Table structure for table `int_eventos` */

DROP TABLE IF EXISTS `int_eventos`;

CREATE TABLE `int_eventos` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `planta` bigint(20) DEFAULT '0' COMMENT 'ID de la planta',
  `nombre` varchar(100) DEFAULT NULL COMMENT 'Nombre/Descripción del registro',
  `monitor` char(1) DEFAULT NULL COMMENT 'Monitorear (S/N)',
  `alerta` bigint(20) DEFAULT '0' COMMENT 'ID de la alerta asociado',
  `revision` int(6) DEFAULT '0' COMMENT 'Tiempo de revisión',
  `revisado` datetime DEFAULT NULL COMMENT 'Fecha y hora la última revisión',
  `prioridad` int(3) DEFAULT NULL COMMENT 'Prioridad de la revisión',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` datetime DEFAULT NULL COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(20) DEFAULT '0' COMMENT 'Creado por',
  `modificado` bigint(20) DEFAULT '0' COMMENT 'Modificado por',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Catálogo de eventos';

/*Table structure for table `int_listados` */

DROP TABLE IF EXISTS `int_listados`;

CREATE TABLE `int_listados` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del correo',
  `nombre` varchar(50) DEFAULT NULL COMMENT 'Reporte',
  `datos` char(1) DEFAULT 'S' COMMENT 'Permite datos en csv',
  `grafica` char(1) DEFAULT 'S' COMMENT 'Permite gráficas',
  `orden` int(3) DEFAULT '0' COMMENT 'Orden en la vista',
  `file_name` varchar(200) DEFAULT NULL COMMENT 'Nombre del archivo',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Activo?',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Reportes para el negocio';

/*Table structure for table `int_opciones` */

DROP TABLE IF EXISTS `int_opciones`;

CREATE TABLE `int_opciones` (
  `id` int(6) DEFAULT NULL COMMENT 'ID de la opción',
  `rol` char(1) DEFAULT NULL COMMENT 'ID del rol',
  `nombre` varchar(60) DEFAULT NULL COMMENT 'Descripción de la opción',
  `orden` int(4) DEFAULT NULL COMMENT 'Orden en la pantalla de usuarios',
  `visualizar` char(1) DEFAULT 'S' COMMENT 'Visualizar en el sistema',
  `acciones` varchar(10) DEFAULT 'SSSSS' COMMENT 'Acciones (Visualizar, Crear, Editar, Inactivar/Reactivar, Eliminar)',
  `opcion_app` int(4) DEFAULT '0' COMMENT 'Opción de la app',
  `url` varchar(40) DEFAULT NULL COMMENT 'URL a ejecutar',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus de la opción'
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

/*Table structure for table `licencias` */

DROP TABLE IF EXISTS `licencias`;

CREATE TABLE `licencias` (
  `tipo` char(1) DEFAULT NULL COMMENT 'Tipo de dispositivo (B=Botonera, R=Reloj)',
  `mmcall` varchar(50) DEFAULT NULL COMMENT 'Valor MMCall',
  `cronos` varchar(50) DEFAULT NULL COMMENT 'Clave de Cronos',
  `inicio` datetime DEFAULT NULL COMMENT 'Fecha de inicio de la licencia',
  `licenciado` date DEFAULT NULL COMMENT 'Fecha de licencia',
  `vencimiento` datetime DEFAULT NULL COMMENT 'Fecha de vencimiento',
  `estatus` char(1) DEFAULT NULL COMMENT 'Estatus de la licencia',
  KEY `NewIndex1` (`tipo`,`mmcall`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Licenciamiento de equipos';

/*Table structure for table `log` */

DROP TABLE IF EXISTS `log`;

CREATE TABLE `log` (
  `id` bigint(11) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `fecha` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'Fecha y hora del registro',
  `aplicacion` int(6) DEFAULT NULL COMMENT 'ID de la aplicación',
  `tipo` int(1) DEFAULT '0' COMMENT 'Tipo de mensaje',
  `proceso` bigint(20) DEFAULT '0' COMMENT 'Número del proceso',
  `texto` varchar(250) DEFAULT NULL COMMENT 'Mensaje descriptivo (hasta 250 caracteres)',
  `visto` char(1) DEFAULT 'N' COMMENT 'Ya se vió en el visor?',
  `visto_pc` char(1) DEFAULT 'N' COMMENT 'Ya se vió en el log del PC?',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`fecha`),
  KEY `NewIndex2` (`visto_pc`),
  KEY `NewIndex3` (`visto`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;

/*Table structure for table `mapas` */

DROP TABLE IF EXISTS `mapas`;

CREATE TABLE `mapas` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `planta` bigint(20) DEFAULT NULL COMMENT 'ID de la planta',
  `descripcion` varchar(200) NOT NULL,
  `ancho` float NOT NULL DEFAULT '0',
  `alto` float NOT NULL DEFAULT '0',
  `tasa_actualizacion` int(11) NOT NULL DEFAULT '5',
  `tasa_refresco` int(11) NOT NULL DEFAULT '600',
  `activo` tinyint(4) NOT NULL DEFAULT '0',
  `nombre` varchar(50) NOT NULL DEFAULT '',
  PRIMARY KEY (`id`),
  UNIQUE KEY `descripcion` (`descripcion`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=utf8;

/*Table structure for table `mensajes` */

DROP TABLE IF EXISTS `mensajes`;

CREATE TABLE `mensajes` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `alerta` bigint(20) DEFAULT '0' COMMENT 'ID de la alerta',
  `canal` int(2) DEFAULT '0' COMMENT 'Canal de la alerta (0=Llamada, 1=SMS, 2=Correo, 3=MMcall)',
  `tipo` int(2) DEFAULT '0' COMMENT '0=Inicio, 1-5=Escalación1, 9=Repetición, 11-15=Repetición de escalamiento',
  `proceso` bigint(20) DEFAULT '0' COMMENT 'ID del proceso',
  `alarma` bigint(20) DEFAULT '0' COMMENT 'ID de la alarma',
  `prioridad` int(1) DEFAULT '0' COMMENT 'Prioridad del mensaje',
  `estatus` varchar(20) DEFAULT 'A' COMMENT 'Estatus del mensaje',
  `lista` bigint(20) DEFAULT '0' COMMENT 'ID del Recipiente',
  `creada` timestamp NULL DEFAULT CURRENT_TIMESTAMP COMMENT 'Creación del mensaje',
  `enviada` datetime DEFAULT NULL COMMENT 'Fecha de envío del mensaje',
  `texto` varchar(20) DEFAULT '' COMMENT 'Mensaje',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`prioridad`,`estatus`),
  KEY `NewIndex2` (`estatus`),
  KEY `NewIndex3` (`canal`,`estatus`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;

/*Table structure for table `mensajes_procesados` */

DROP TABLE IF EXISTS `mensajes_procesados`;

CREATE TABLE `mensajes_procesados` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del mensaje',
  `texto` varchar(400) DEFAULT NULL COMMENT 'Texto a enviar o dictar',
  `canal` int(2) DEFAULT '0' COMMENT 'Canal de difusión',
  `titulo` varchar(100) DEFAULT '0' COMMENT '(Para correo)',
  `prioridad` int(2) DEFAULT '0' COMMENT 'Proridad del mensaje',
  `fecha` date DEFAULT NULL COMMENT 'Fecha del mensaje (solo dura un día)',
  `mensaje` decimal(20,0) DEFAULT '0' COMMENT 'ID del mensaje',
  `estatus` char(20) DEFAULT 'A' COMMENT 'Estatus del envío',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`id`,`canal`,`prioridad`),
  KEY `NewIndex2` (`fecha`),
  KEY `NewIndex3` (`mensaje`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Mensajes a enviar o llamar';

/*Table structure for table `movimientos_cab` */

DROP TABLE IF EXISTS `movimientos_cab`;

CREATE TABLE `movimientos_cab` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `requester` bigint(20) DEFAULT NULL COMMENT 'ID del requester',
  `pager` int(3) DEFAULT NULL COMMENT 'ID del pager',
  `transporte` bigint(20) DEFAULT '0',
  `chofer` bigint(20) DEFAULT '0',
  `vehiculo` bigint(20) DEFAULT '0',
  `carga` bigint(20) DEFAULT '0' COMMENT 'Carga',
  `origen` bigint(20) DEFAULT '0',
  `destino` bigint(20) DEFAULT '0',
  `actual_fecha` datetime DEFAULT NULL,
  `inicio` datetime DEFAULT NULL,
  `fin` datetime DEFAULT NULL,
  `tiempo` bigint(14) DEFAULT '0',
  `estado` int(2) DEFAULT '0' COMMENT 'Estado del movimiento',
  `alarmado` char(1) DEFAULT 'N',
  `alarmado_desde` datetime DEFAULT NULL,
  `transacciones` int(4) DEFAULT '0' COMMENT 'Número de movimientos del viaje',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`requester`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Cabecera de movimientos';

/*Table structure for table `movimientos_det` */

DROP TABLE IF EXISTS `movimientos_det`;

CREATE TABLE `movimientos_det` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `cabecera` bigint(20) DEFAULT '0' COMMENT 'ID del movimiento cabecera',
  `requester` bigint(20) DEFAULT '0' COMMENT 'ID del requester asociado',
  `origen` bigint(20) DEFAULT '0',
  `destino` bigint(20) DEFAULT '0',
  `des_inicio` datetime DEFAULT NULL,
  `des_fin` datetime DEFAULT NULL,
  `des_tiempo` bigint(10) DEFAULT '0',
  `des_estimado` bigint(10) DEFAULT '0',
  `des_alarmado` char(1) DEFAULT 'N',
  `des_alarmado_desde` datetime DEFAULT NULL,
  `inicio` datetime DEFAULT NULL,
  `fin` datetime DEFAULT NULL,
  `tiempo` bigint(10) DEFAULT '0',
  `estimado` bigint(10) DEFAULT '0',
  `alarmado` char(1) DEFAULT 'N',
  `alarmado_desde` datetime DEFAULT NULL,
  `estatus` int(1) DEFAULT '0',
  `estado` int(2) DEFAULT '0',
  `viaje` int(3) DEFAULT '0',
  `transporte` bigint(20) DEFAULT '0',
  `vehiculo` bigint(20) DEFAULT '0',
  `area` bigint(20) DEFAULT '0',
  `carga` bigint(20) DEFAULT '0',
  `chofer` bigint(20) DEFAULT '0',
  `espera` bigint(10) DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`cabecera`),
  KEY `NewIndex2` (`requester`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1;

/*Table structure for table `objetos` */

DROP TABLE IF EXISTS `objetos`;

CREATE TABLE `objetos` (
  `mapa_id` int(11) NOT NULL,
  `id` varchar(50) NOT NULL,
  `descripcion` varchar(300) NOT NULL,
  `ultima_actualizacion` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`mapa_id`,`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

/*Table structure for table `politicas` */

DROP TABLE IF EXISTS `politicas`;

CREATE TABLE `politicas` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `nombre` varchar(30) CHARACTER SET latin1 DEFAULT NULL COMMENT 'Nombre corto',
  `deunsolouso` char(1) COLLATE utf8_bin DEFAULT NULL COMMENT 'De un sólo uso',
  `obligatoria` char(1) COLLATE utf8_bin DEFAULT NULL COMMENT 'Se requiere contraseña?',
  `vence` char(1) CHARACTER SET latin1 DEFAULT NULL COMMENT 'La contraseña vence?',
  `diasvencimiento` bigint(7) DEFAULT NULL COMMENT 'Días para vencerse',
  `aviso` bigint(7) DEFAULT NULL COMMENT 'Días de anticipación para avisar',
  `complejidad` char(10) COLLATE utf8_bin DEFAULT NULL COMMENT 'Complejidad de la contraseña (1-2=largo,3=especiales,4=numeros,5=mayus/minus)',
  `usadas` int(2) DEFAULT NULL COMMENT 'Últimas contraseñas usadas',
  `caducidad` int(3) DEFAULT NULL COMMENT 'Días de gracia',
  `estatus` char(1) CHARACTER SET latin1 DEFAULT 'A' COMMENT 'Estatus del registro',
  `creacion` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP COMMENT 'Fecha de creación',
  `modificacion` datetime DEFAULT NULL COMMENT 'Fecha de modificación',
  `creado` bigint(20) DEFAULT NULL COMMENT 'Usuario que creó el registro',
  `modificado` bigint(20) DEFAULT NULL COMMENT 'Usuario que modificó el registro',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=utf8 COLLATE=utf8_bin COMMENT='Tabla de políticas';

/*Table structure for table `pu_colores` */

DROP TABLE IF EXISTS `pu_colores`;

CREATE TABLE `pu_colores` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del tema',
  `planta` bigint(20) DEFAULT '0' COMMENT 'ID de la planta',
  `usuario` bigint(20) DEFAULT '0' COMMENT 'ID del usuario',
  `nombre` varchar(50) DEFAULT NULL COMMENT 'Nomre del tema',
  `personalizada` char(1) DEFAULT 'N' COMMENT 'Skin personalizada',
  `obligatorio` char(1) DEFAULT 'N' COMMENT 'Skin obligatorio',
  `seleccionado` char(1) DEFAULT 'N' COMMENT 'Skin seleccionado',
  `fondo_total` varchar(20) DEFAULT NULL COMMENT 'Color de fondo total',
  `fondo_barra_superior` varchar(20) DEFAULT NULL COMMENT 'Color de la barra superior',
  `fondo_barra_inferior` varchar(20) DEFAULT NULL COMMENT 'Color de la barra inferior',
  `fondo_aplicacion` varchar(20) DEFAULT NULL COMMENT 'Color de fondo',
  `fondo_seleccion` varchar(20) DEFAULT NULL COMMENT 'Color de fondo selección',
  `fondo_boton` varchar(20) DEFAULT NULL COMMENT 'Color de fondo botón ',
  `fondo_slider` varchar(20) DEFAULT NULL COMMENT 'Color de fondo slider',
  `fondo_tarjeta` varchar(20) DEFAULT NULL COMMENT 'Color de fondo de la tarjeta',
  `fondo_boton_inactivo` varchar(20) DEFAULT NULL COMMENT 'Color de fondo botón inactivo',
  `fondo_boton_positivo` varchar(20) DEFAULT NULL COMMENT 'Color de fondo botón positivo',
  `fondo_boton_negativo` varchar(20) DEFAULT NULL COMMENT 'Color de fondo botón negativo',
  `fondo_boton_barra` varchar(20) DEFAULT NULL COMMENT 'Color de fondo botón en barra',
  `fondo_tiptool` varchar(20) DEFAULT NULL COMMENT 'Color de fondo tiptool',
  `fondo_logo` varchar(20) DEFAULT NULL COMMENT 'Color de fondo logo',
  `fondo_snack_normal` varchar(20) DEFAULT NULL COMMENT 'Color de fondo snack normal',
  `fondo_snack_error` varchar(20) DEFAULT NULL COMMENT 'Color de fondo snack error',
  `borde_total` varchar(20) DEFAULT NULL COMMENT 'Color de borde',
  `borde_seleccion` varchar(20) DEFAULT NULL COMMENT 'Color de borde en selección',
  `borde_hover` varchar(20) DEFAULT NULL COMMENT 'Color de borde en Hover',
  `borde_boton` varchar(20) DEFAULT NULL COMMENT 'Color de borde de botones',
  `borde_boton_inactivo` varchar(20) DEFAULT NULL COMMENT 'Color de borde de inactivo',
  `borde_tarjeta` varchar(20) DEFAULT NULL COMMENT 'Color de borde de la tarjeta',
  `borde_tiptool` varchar(20) DEFAULT NULL COMMENT 'Color de borde del tiptool',
  `color_impar` varchar(20) DEFAULT NULL COMMENT 'Color impar',
  `color_par` varchar(20) DEFAULT NULL COMMENT 'Color par',
  `texto_tarjeta` varchar(20) DEFAULT NULL COMMENT 'Texto tarjeta',
  `texto_tarjeta_resalte` varchar(20) DEFAULT NULL COMMENT 'Texto tarjeta resaltado',
  `texto_barra_superior` varchar(20) DEFAULT NULL COMMENT 'Texto barra superior',
  `texto_barra_inferior` varchar(20) DEFAULT NULL COMMENT 'Texto barra inferior',
  `texto_boton` varchar(20) DEFAULT NULL COMMENT 'Texto botón',
  `texto_boton_inactivo` varchar(20) DEFAULT NULL COMMENT 'Texto botón inactivo',
  `texto_boton_positivo` varchar(20) DEFAULT NULL COMMENT 'Texto botón positivo',
  `texto_boton_negativo` varchar(20) DEFAULT NULL COMMENT 'Texto botón negativo',
  `texto_boton_barra` varchar(20) DEFAULT NULL COMMENT 'Texto botón barra',
  `texto_seleccion` varchar(20) DEFAULT NULL COMMENT 'Texto selección',
  `texto_tiptool` varchar(20) DEFAULT NULL COMMENT 'Texto tiptool',
  `texto_snack_normal` varchar(20) DEFAULT NULL COMMENT 'Texto snack normal',
  `texto_snack_error` varchar(20) DEFAULT NULL COMMENT 'Texto snack error',
  `texto_solo_texto` varchar(20) DEFAULT NULL COMMENT 'Texto solo lectura',
  `estatus` char(1) DEFAULT 'A' COMMENT 'Estatus',
  `eliminable` char(1) DEFAULT 'S',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Colores';

/*Table structure for table `pu_graficos` */

DROP TABLE IF EXISTS `pu_graficos`;

CREATE TABLE `pu_graficos` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `usuario` bigint(11) DEFAULT NULL COMMENT 'ID del usuario',
  `visualizar` char(1) DEFAULT 'S' COMMENT 'Visualizar gráfico',
  `grafico` int(4) DEFAULT NULL COMMENT 'Número del gráfico',
  `titulo` varchar(100) DEFAULT NULL COMMENT 'Título del gráfico',
  `titulo_fuente` int(2) DEFAULT NULL COMMENT 'Fuente del título',
  `sub_titulo` varchar(100) DEFAULT NULL COMMENT 'Subtítulo del gráfico',
  `subtitulo_fuente` int(2) DEFAULT NULL COMMENT 'Fuente del subtítulo',
  `texto_x` varchar(100) DEFAULT NULL COMMENT 'Texto eje X',
  `texto_x_fuente` int(2) DEFAULT NULL COMMENT 'Fuente del eje X',
  `texto_y` varchar(100) DEFAULT '0' COMMENT 'Texto eje Y',
  `texto_y_fuente` int(2) DEFAULT NULL COMMENT 'Fuente del eje Y',
  `texto_z` varchar(100) DEFAULT '0' COMMENT 'Texto eje Z',
  `texto_z_fuente` int(2) DEFAULT NULL COMMENT 'Fuente del eje Z',
  `etiqueta_mostrar` char(1) DEFAULT NULL COMMENT 'Mostrar etiquetas',
  `etiqueta_fuente` int(2) DEFAULT NULL COMMENT 'Fuente de la etiqueta',
  `etiqueta_leyenda` varchar(30) DEFAULT NULL COMMENT 'Título de la leyenda',
  `etiqueta_color` varchar(20) DEFAULT NULL COMMENT 'Color de la etiqueta',
  `etiqueta_fondo` varchar(20) DEFAULT NULL COMMENT 'Color del fondo',
  `ancho` int(6) DEFAULT NULL COMMENT 'Ancho de la pantalla',
  `alto` int(6) DEFAULT '0' COMMENT 'Alto de la pantalla',
  `margen_arriba` int(4) DEFAULT NULL COMMENT 'Margen arriba',
  `margen_abajo` int(4) DEFAULT NULL COMMENT 'Margen abajo',
  `margen_izquierda` int(4) DEFAULT NULL COMMENT 'Margen izquierda',
  `margen_derecha` int(4) DEFAULT NULL COMMENT 'Margen derecha',
  `maximo_barras` int(2) DEFAULT '0' COMMENT 'Máximo de barras',
  `maximo_barraspct` int(3) DEFAULT '0' COMMENT 'PCT de máximo de barras',
  `agrupar` char(1) DEFAULT NULL COMMENT 'Agrupar el resto',
  `agrupar_posicion` char(1) DEFAULT NULL COMMENT 'P=Principio, F=Final, N=Ordenado',
  `agrupar_texto` varchar(30) DEFAULT NULL COMMENT 'Texto de la barra agrupada',
  `fecha` datetime DEFAULT CURRENT_TIMESTAMP COMMENT 'Fecha de actualización',
  `periodo_tipo` int(2) DEFAULT NULL COMMENT 'Tipo de período (0: segundos, 1: minutos, 2: horas, 3: días, 4: semanas, 5: meses, 6: años, 10: DTD, 11: WTD, 12: MTD, 13: YTD)',
  `periodo_atras` bigint(8) DEFAULT NULL COMMENT 'Tiempo a recorrer hacía atrás',
  `mostrar_tabla` char(1) DEFAULT NULL COMMENT 'Mostrar tabla',
  `orden` char(1) DEFAULT NULL COMMENT 'Orden de la gráfica',
  `color_fondo_barras` varchar(20) DEFAULT NULL COMMENT 'Color de fondo de las barras',
  `color_letras` varchar(20) DEFAULT NULL COMMENT 'Color de las letras',
  `color_fondo` varchar(20) DEFAULT NULL COMMENT 'Color del fondo',
  `color_leyenda_fondo` varchar(20) DEFAULT NULL COMMENT 'Color del fondo de la leyenda',
  `color_leyenda` varchar(20) DEFAULT NULL COMMENT 'Color del texto de la leyenda',
  `ver_esperado` char(1) DEFAULT 'N' COMMENT 'Ver esperado',
  `grueso_esperado` int(2) DEFAULT NULL COMMENT 'Pixls del esperado',
  `color_esperado` varchar(20) DEFAULT NULL COMMENT 'Color del esperado',
  `texto_esperado` varchar(30) DEFAULT NULL COMMENT 'Texto para el valor esperado',
  `valor_esperado` decimal(20,3) DEFAULT '0.000' COMMENT 'Valor esperado',
  `incluir_ceros` char(1) DEFAULT 'N' COMMENT 'Incluir valores cero',
  `orden_grafica` char(1) DEFAULT 'N' COMMENT 'Orden de la gráfica (M=Mayor a menor, N= Menor a mayor, A=Alfabético)',
  `mostrarpct` char(1) DEFAULT 'N' COMMENT 'Mostrar frecuencia',
  `color_barra` varchar(20) DEFAULT NULL COMMENT 'Color de la barra',
  `color_barra_borde` varchar(20) DEFAULT NULL COMMENT 'Color de la barra borde',
  `color_barra_o` varchar(20) DEFAULT NULL COMMENT 'Color de la barra opcional',
  `color_barra_borde_o` varchar(20) DEFAULT NULL COMMENT 'Color de la barra borde opcional',
  `ver_leyenda` char(1) DEFAULT 'S' COMMENT 'Ver leyenda',
  `overlap` char(1) DEFAULT 'S' COMMENT 'Tipo de overlap',
  `adicionales` char(13) DEFAULT '0;0;0;0;0;0;0' COMMENT 'YTD, MTD, Año anterior, Mes anterior, Mismo mes Año anterior, Aplicar Filtro a los acumulados',
  `adicionales_colores` varchar(130) DEFAULT NULL COMMENT 'Colores',
  `adicionales_titulos` varchar(130) DEFAULT NULL,
  `oee` char(6) DEFAULT 'NNNSSS',
  `oee_colores` varchar(80) DEFAULT NULL,
  `oee_tipo` char(3) DEFAULT 'BBB',
  `oee_nombre` varchar(80) DEFAULT ';;',
  `tipo_principal` char(1) DEFAULT 'B',
  `colores_multiples` char(1) DEFAULT NULL COMMENT 'Usar colores diferentes',
  `color_spiline` varchar(20) DEFAULT NULL COMMENT 'Color del spline',
  `grueso_spiline` int(2) DEFAULT '0' COMMENT 'Pixls del spline',
  `mostrar_argumentos` char(1) DEFAULT 'S' COMMENT 'Mostrar argumentos',
  `activo` char(1) DEFAULT 'S' COMMENT 'Gráfico activo para el sistema?',
  `esperado_esquema` varchar(50) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`grafico`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Preferencias de usuario (Gráficos)';

/*Table structure for table `relacion_usuarios_opciones` */

DROP TABLE IF EXISTS `relacion_usuarios_opciones`;

CREATE TABLE `relacion_usuarios_opciones` (
  `usuario` bigint(20) DEFAULT NULL COMMENT 'ID del número de parte',
  `opcion` bigint(6) DEFAULT NULL COMMENT 'ID de la ruta',
  KEY `NewIndex1` (`usuario`,`opcion`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Relación usuarios y opciones del sistema';

/*Table structure for table `relacion_usuarios_operaciones` */

DROP TABLE IF EXISTS `relacion_usuarios_operaciones`;

CREATE TABLE `relacion_usuarios_operaciones` (
  `usuario` bigint(20) DEFAULT NULL COMMENT 'ID del número de parte',
  `proceso` bigint(20) DEFAULT NULL COMMENT 'ID de la ruta',
  `tipo` int(2) DEFAULT '0' COMMENT 'Catálogo asociado',
  KEY `NewIndex1` (`usuario`,`proceso`,`tipo`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Relación entre usuarios y operaciones';

/*Table structure for table `relaciones` */

DROP TABLE IF EXISTS `relaciones`;

CREATE TABLE `relaciones` (
  `operacion` bigint(4) DEFAULT NULL COMMENT '1=Alertas',
  `indice` bigint(20) DEFAULT NULL COMMENT 'ID del registro principal a evaluar',
  `detalle` bigint(20) DEFAULT NULL COMMENT 'ID del registro detalle',
  KEY `NewIndex1` (`operacion`,`indice`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;

/*Table structure for table `relacionparos` */

DROP TABLE IF EXISTS `relacionparos`;

CREATE TABLE `relacionparos` (
  `paro` bigint(11) NOT NULL COMMENT 'ID del paro',
  `grupo` bigint(11) NOT NULL COMMENT 'ID del grupo',
  `equipo` bigint(11) NOT NULL COMMENT 'ID del equipo',
  PRIMARY KEY (`paro`,`grupo`,`equipo`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Relacion paros versus equips';

/*Table structure for table `requesters` */

DROP TABLE IF EXISTS `requesters`;

CREATE TABLE `requesters` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `pager` int(3) DEFAULT '0' COMMENT 'ID del pager en MMCall',
  `nombre` varchar(30) DEFAULT NULL COMMENT 'Nombre asociado al requester',
  `area` bigint(20) DEFAULT '0',
  `color` varchar(20) DEFAULT NULL,
  `origen` bigint(20) DEFAULT '0',
  `destino` bigint(20) DEFAULT '0' COMMENT 'ID del área asignada',
  `estimado` bigint(10) DEFAULT '0',
  `chofer` bigint(20) DEFAULT '0',
  `vehiculo` bigint(20) DEFAULT '0',
  `transporte` bigint(20) DEFAULT '0',
  `movimiento` bigint(20) DEFAULT '0' COMMENT 'Registro actual',
  `estado` int(2) DEFAULT '0' COMMENT 'Estado',
  `mensaje` varchar(300) DEFAULT NULL,
  `mensaje_mmcall` varchar(40) DEFAULT NULL,
  `desde` datetime DEFAULT NULL,
  `hasta` datetime DEFAULT NULL,
  `alarmado` char(1) DEFAULT 'N',
  `alarmado_desde` datetime DEFAULT NULL,
  `carga` bigint(20) DEFAULT NULL,
  `estatus` int(1) DEFAULT '0',
  `repeticiones` int(3) DEFAULT '0' COMMENT 'Número de repeticiones',
  `ultima_repeticion` datetime DEFAULT NULL,
  `repeticiones_audio` int(3) DEFAULT '0',
  `ultima_repeticion_audio` datetime DEFAULT NULL,
  `viajes` int(4) DEFAULT '0' COMMENT 'Total viajes',
  `alarmas` int(4) DEFAULT '0' COMMENT 'Total alarmas',
  `tipo` int(1) DEFAULT '0' COMMENT 'Tipo: 0=Requester 1=Movil',
  `movil` varchar(30) DEFAULT NULL COMMENT 'Movil',
  `monitorear` char(1) DEFAULT 'N',
  `des_estimado` bigint(10) DEFAULT '0',
  `des_monitorear` char(1) DEFAULT 'N',
  `preasignado` char(1) DEFAULT 'N',
  `orden` int(6) DEFAULT '0',
  `fecha_recibo` datetime DEFAULT NULL,
  `espera_temporal` bigint(8) DEFAULT '0',
  `fecha_asignacion` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`pager`),
  KEY `NewIndex2` (`movil`),
  KEY `NewIndex3` (`estatus`,`estado`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=utf16 COMMENT='Catálogo de requesters';

/*Table structure for table `status_objetos` */

DROP TABLE IF EXISTS `status_objetos`;

CREATE TABLE `status_objetos` (
  `id` int(11) NOT NULL,
  `descripcion` varchar(200) NOT NULL,
  `color` varchar(10) NOT NULL,
  `normal` tinyint(4) NOT NULL DEFAULT '0',
  `mapa_Id` int(11) NOT NULL,
  PRIMARY KEY (`id`,`mapa_Id`),
  KEY `fk_status_objetos_mapas_idx` (`mapa_Id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

/*Table structure for table `tablas` */

DROP TABLE IF EXISTS `tablas`;

CREATE TABLE `tablas` (
  `id` int(6) NOT NULL COMMENT 'ID de la tabla',
  `nombre` varchar(50) DEFAULT NULL COMMENT 'Nombre de la tabla',
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 COMMENT='Catálogo de tablas';

/*Table structure for table `tipos_figuras` */

DROP TABLE IF EXISTS `tipos_figuras`;

CREATE TABLE `tipos_figuras` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `descripcion` varchar(100) NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=utf8;

/*Table structure for table `traduccion` */

DROP TABLE IF EXISTS `traduccion`;

CREATE TABLE `traduccion` (
  `id` bigint(11) NOT NULL AUTO_INCREMENT COMMENT 'ID del registro',
  `literal` varchar(100) DEFAULT NULL COMMENT 'Literal a buscar',
  `idioma` varchar(5) DEFAULT NULL COMMENT 'Idioma',
  `traduccion` varchar(100) DEFAULT NULL COMMENT 'Traducción',
  PRIMARY KEY (`id`),
  KEY `NewIndex1` (`literal`,`idioma`)
) ENGINE=MyISAM AUTO_INCREMENT=1 DEFAULT CHARSET=latin1 COMMENT='Tabla de traducción';

DROP TABLE IF EXISTS `sentencias`;

CREATE TABLE `sentencias` (
  `id` BIGINT(10) NOT NULL AUTO_INCREMENT,
  `sentencia` VARCHAR(5000) DEFAULT NULL,
  `estatus` CHAR(1) DEFAULT 'N',
  `fecha` TIMESTAMP NULL DEFAULT CURRENT_TIMESTAMP,
  `elvis` BIGINT(10) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=INNODB DEFAULT CHARSET=latin1
")

            Dim reader2 = consultaSEL(cadSQL)
            If Not reader2 Is Nothing Then
                XtraMessageBox.Show("La base de datos de Logisticar ha sido creada satisfactoriamente", "Logisticar v1.0", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)

                regsAfectados = consultaACT("insert  into `actualizaciones`(`plantas`,`lineas`,`maquinas`,`procesos`,`rutas`,`det_rutas`,`det_procesos`,`partes`,`recipientes`,`alertas`,`situaciones`,`horario`,`planes`,`prioridades`,`areas`,`fallas`,`generales`,`distribucion`,`correos`,`turnos`,`usuarios`,`traducciones`,`politicas`,`rates`,`estimados`,`objetivos`,`sensores`,`paros`,`dispositivos`,`vehiculos`) values ('2020-01-01 00:00:00','2020-04-22 01:19:36','2020-04-10 15:27:28',NULL,NULL,NULL,NULL,NULL,NULL,'2020-04-13 14:26:27',NULL,NULL,NULL,NULL,'2020-04-13 11:01:14',NULL,'2020-04-13 11:02:30','2020-04-13 12:37:28','2020-04-22 02:49:04',NULL,'2020-04-14 14:33:06','2020-04-13 11:55:59','2020-03-28 13:33:25',NULL,NULL,NULL,NULL,NULL,'2020-04-22 15:07:05','2020-04-22 02:49:25');

/*Data for the table `cat_usuarios` */

insert  into `cat_usuarios`(`id`,`secuencia`,`referencia`,`nombre`,`clave`,`prefijo`,`notas`,`rol`,`politica`,`operacion`,`linea`,`maquina`,`area`,`imagen`,`admin`,`calidad`,`reversos`,`programacion`,`inventario`,`tecnico`,`planta`,`apagar_alertas`,`ver_alertas`,`cerrar_al_ejecutar`,`vista_resumida_fallas`,`confirmar_reparacion`,`ultima_pantalla`,`inicializada`,`preferencias_andon`,`departamento`,`compania`,`tema`,`turno`,`claveant1`,`claveant2`,`claveant3`,`claveant4`,`claveant5`,`ucambio`,`entrada`,`salida`,`ultimo_reporte`,`estatus`,`creacion`,`modificacion`,`creado`,`modificado`) values (1,1,'ADMIN','ADMINISTRADOR DEL SISTEMA','Š@HÅÇxIåD§%îÖìì','ž¾€\\—ÎeJÛy¥~Î-T^','','A',0,'S','S','S','S','','S','S','S','N','N','N',39,'S','S','N','S','N',4,'N','10101001111100111101011111011010101111111010111111',41,40,3,0,'ØbÖÏçx:OÜÝ1_ñ‹','ž¾€\\—ÎeJÛy¥~Î-T^','WæxP<ÂÀÏ\'û','±ç–Ä¨å‘ ÂzTB','ž¾€\\—ÎeJÛy¥~Î-T^','2020-03-27','2020-06-22 14:07:39','2020-04-14 14:33:09',0,'A','2019-06-30 23:52:20','2020-03-28 13:28:32',1,1),(2,2,'error','OPERADOR','Š@HÅÇxIåD§%îÖìì',NULL,'','O',0,'S','N','N','S','','N','N','N','N','N','N',39,'N','N','N','N','S',12,'N','11111111111111111111111',41,40,2,0,'Š@HÅÇxIåD§%îÖìì',NULL,NULL,NULL,NULL,'2020-04-09','2020-04-09 11:32:41','2020-04-09 11:37:55',0,'A','2019-07-08 09:37:25','2020-04-09 11:35:37',1,1),(3,3,'GESTION','GESTOR','Š@HÅÇxIåD§%îÖìì',NULL,'','G',0,'S','N','S','S','','N','N','N','N','N','N',18,'N','N','S',NULL,'N',NULL,NULL,'11111111111111111111',8,17,3,0,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,0,'A','2019-07-08 09:37:25','2020-01-12 13:54:51',1,1);



/*Data for the table `configuracion` */

insert  into `configuracion`(`id`,`planta`,`rfc`,`licencia`,`tiempo`,`correo_cuenta`,`correo_puerto`,`correo_ssl`,`correo_clave`,`correo_host`,`flag_agregar`,`ejecutando_desde`,`ultima_falla`,`ultima_revision`,`revisar_cada`,`utilizar_arduino`,`puerto_comm1`,`puerto_comm1_par`,`puerto_comm2`,`puerto_comm2_par`,`puerto_comm3`,`puerto_comm3_par`,`puerto_comm4`,`puerto_comm4_par`,`puerto_comm5`,`puerto_comm5_par`,`puerto_comm6`,`puerto_comm6_par`,`ruta_sms`,`ruta_audios`,`optimizar_llamada`,`optimizar_sms`,`optimizar_correo`,`optimizar_mmcall`,`mantener_prioridad`,`voz_predeterminada`,`escape_mmcall`,`escape_mmcall_mensaje`,`escape_mmcall_lista`,`escape_mmcall_cancelar`,`escape_llamadas`,`escape_accion`,`escape_lista`,`escape_mensaje`,`escape_mensaje_propio`,`veces_reproducir`,`gestion_log`,`gestion_meses`,`correo_titulo_falla`,`correo_titulo`,`correo_cuerpo`,`correo_firma`,`timeout_llamadas`,`timeout_sms`,`traducir`,`tiempo_corte`,`ultimo_corte`,`bajo_hasta`,`bajo_color`,`bajo_etiqueta`,`medio_hasta`,`medio_color`,`medio_etiqueta`,`alto_color`,`alto_etiqueta`,`noatendio_color`,`noatendio_etiqueta`,`escaladas_color`,`escaladas_etiqueta`,`flag_monitor`,`ruta_archivos_enviar`,`server_mmcall`,`cad_consolidado`,`ruta_imagenes`,`tiempo_imagen`,`graficas_seleccion`,`graficas_duracion`,`timeout_fallas`,`avisar_segundos`,`color_aviso`,`contar_post`,`color_post`,`escaner_prefijo`,`escaner_sufijo`,`tiempo_holgura`,`tiempo_entre_lecturas`,`tiempo_escaner`,`largo_escaner`,`lote_inspeccion_clave`,`reverso_permitir`,`reverso_referencia`,`dias_programacion`,`holgura_reprogramar`,`tipo_flujo`,`estimado_productividad`,`confirmar_mensaje_mantto`,`url_mmcall`,`accion_mmcall`,`tiempo_reporte`,`be_alarmas_correos`,`be_alarmas_mmcall`,`be_alarmas_llamadas`,`be_alarmas_sms`,`be_envio_reportes`,`be_revision_correos`,`be_revision_mmcall`,`be_revision_arduino`,`be_log_lineas`,`be_log_activar`,`maximo_largo_mmcall`,`separador_mail`,`limitar_inicio`,`limitar_respuestas`,`recuperar_sesion`,`visor_revisar_cada`,`logo_arriba`,`logo_izquierda`,`logo_ancho`,`logo_alto`,`logo_ruta`,`mapa_alineacion`,`mapa_fondo`,`confirmar_reparacion`,`ruta_programa_mapa`,`mapa_delay`,`mapa_rotacion`,`tema_principal`,`mapa_ultimo`,`mapa_solicitud`,`tema_permitir_crear`,`tema_permitir_cambio`,`turno_modo`,`ver_logo_cronos`,`url_cronos`,`audios_activar`,`audios_ruta`,`audios_prefijo`,`ver_ayuda`,`ip_localhost`,`mensaje`,`ultimo_audio`,`audios_escalamiento`,`audios_resolucion`,`modulo_produccion`,`usar_clave_falla`,`mostrar_numero`,`turno_actual`,`dimension`,`tiempo_andon`,`lectura_pendiente`,`modulo_oee`,`ver_nombre_planta`,`oee_mostrar_paro`,`carrusel_oee`,`carrusel_tiempo`,`kanban`,`smed`,`esperado_oee`,`esperado_ftq`,`esperado_efi`,`esperado_dis`,`esperado_mttr`,`esperado_mtbf`,`modulo_wip`,`reportes_inicial`,`turno_oee`,`mensaje_mmcall`,`andon_repeticiones`,`agregar_movil`,`adicionales`,`pagers_val`,`asignar_destino`,`asignar_caseta`,`asignar_automatico`,`visor_mostrar`,`agregar_transporte`) values (1,'MERCADO LIBRE','AAA900101A1A','299409911927952196991911974914',1,'elvismontezuma@hotmail.com','587','S','Montezum@74','smtp.live.com','N','2020-04-22 02:50:22',19365,'2019-06-19 22:04:46',2,'N','COM4','19200,8,0,1,2,S','COM3','null','null','null','','null','null','null','null','','C:/Users/Sistema Logistica/Documents/Audios/SMS','C:/Users/Sistema Logistica/Documents/Audios/Llamadas','N','N','S','N','N','Microsoft Sabina','S','TODOS LOS REQUESTERS OCUPADOS',0,'S',1,'N',12,'SE GENERARON 3 LLAMADAS SIN CONTESTAR AL NUMERO ','N',2,'201911',0,'S','Monitor de WIP v1.0','Este mensaje se envía desde una aplicación automática, por favor no lo responda ya que esta cuenta de correos no se revisa.','Gracias por su soporte!',30,10,'S',NULL,'2019-07-12 16:54:53',67,'E74C3C','BAJO',85,'F5B041','REGULAR','229954','En tiempo','tomato','Abiertas','orange','Escaladas','S','C:/Users/Sistema Logistica/Documents/vw monitorenviar','http://localhost:8081','NAVE VW 102','C:\\Users\\lap\\Documents\\vw\\carrusel',10,'S;S;S;S;N;N','2;2;2;2;2;2',10,300,'tomato','S','red','','',900,300,3000,0,'S','C','REV1234',15,1800,'C',75,'S','http://localhost:8081/locations/integration/page/number=1','N',300,'N','S','N','N','S',0,0,0,NULL,'S',0,';',0,10,'S',10,12,0,197,80,'/logisticar/assets/imagenes/logo.png','xMidYMid meet','FFFFFF','S','',10,1,3,'2019-12-13 11:46:01','A','S','S',3,'S','https://mmcallmexico.mx','N','C:/Users/Sistema Logistica/Documents/Audios/Carpeta General','','S','localhost:8081','[1] DEL TRANSPORTE [2] FAVOR DESPLAZARSE AL AREA [4]',NULL,1,'El reporte [0] ha sido resuelto','S','S','I',0,NULL,60,NULL,'S','S','S',0,10,'N','S','0.00','0.00','0.00','0.00','2.00000','24.00000','N',0,0,'FAVOR DIRIGIRSE A [4]',3,'N','N','S','S','S','N',0,'S');


/*Data for the table `dias` */

insert  into `dias`(`fecha`,`dia`) values ('2018-12-15',7),('2018-12-16',1),('2018-12-17',2),('2018-12-18',3),('2018-12-19',4),('2018-12-20',5),('2018-12-21',6),('2018-12-22',7),('2018-12-23',1),('2018-12-24',2),('2018-12-25',3),('2018-12-26',4),('2018-12-27',5),('2018-12-28',6),('2018-12-29',7),('2018-12-30',1),('2018-12-31',2),('2019-01-01',3),('2019-01-02',4),('2019-01-03',5),('2019-01-04',6),('2019-01-05',7),('2019-01-06',1),('2019-01-07',2),('2019-01-08',3),('2019-01-09',4),('2019-01-10',5),('2019-01-11',6),('2019-01-12',7),('2019-01-13',1),('2019-01-14',2),('2019-01-15',3),('2019-01-16',4),('2019-01-17',5),('2019-01-18',6),('2019-01-19',7),('2019-01-20',1),('2019-01-21',2),('2019-01-22',3),('2019-01-23',4),('2019-01-24',5),('2019-01-25',6),('2019-01-26',7),('2019-01-27',1),('2019-01-28',2),('2019-01-29',3),('2019-01-30',4),('2019-01-31',5),('2019-02-01',6),('2019-02-02',7),('2019-02-03',1),('2019-02-04',2),('2019-02-05',3),('2019-02-06',4),('2019-02-07',5),('2019-02-08',6),('2019-02-09',7),('2019-02-10',1),('2019-02-11',2),('2019-02-12',3),('2019-02-13',4),('2019-02-14',5),('2019-02-15',6),('2019-02-16',7),('2019-02-17',1),('2019-02-18',2),('2019-02-19',3),('2019-02-20',4),('2019-02-21',5),('2019-02-22',6),('2019-02-23',7),('2019-02-24',1),('2019-02-25',2),('2019-02-26',3),('2019-02-27',4),('2019-02-28',5),('2019-03-01',6),('2019-03-02',7),('2019-03-03',1),('2019-03-04',2),('2019-03-05',3),('2019-03-06',4),('2019-03-07',5),('2019-03-08',6),('2019-03-09',7),('2019-03-10',1),('2019-03-11',2),('2019-03-12',3),('2019-03-13',4),('2019-03-14',5),('2019-03-15',6),('2019-03-16',7),('2019-03-17',1),('2019-03-18',2),('2019-03-19',3),('2019-03-20',4),('2019-03-21',5),('2019-03-22',6),('2019-03-23',7),('2019-03-24',1),('2019-03-25',2),('2019-03-26',3),('2019-03-27',4),('2019-03-28',5),('2019-03-29',6),('2019-03-30',7),('2019-03-31',1),('2019-04-01',2),('2019-04-02',3),('2019-04-03',4),('2019-04-04',5),('2019-04-05',6),('2019-04-06',7),('2019-04-07',1),('2019-04-08',2),('2019-04-09',3),('2019-04-10',4),('2019-04-11',5),('2019-04-12',6),('2019-04-13',7),('2019-04-14',1),('2019-04-15',2),('2019-04-16',3),('2019-04-17',4),('2019-04-18',5),('2019-04-19',6),('2019-04-20',7),('2019-04-21',1),('2019-04-22',2),('2019-04-23',3),('2019-04-24',4),('2019-04-25',5),('2019-04-26',6),('2019-04-27',7),('2019-04-28',1),('2019-04-29',2),('2019-04-30',3),('2019-05-01',4),('2019-05-02',5),('2019-05-03',6),('2019-05-04',7),('2019-05-05',1),('2019-05-06',2),('2019-05-07',3),('2019-05-08',4),('2019-05-09',5),('2019-05-10',6),('2019-05-11',7),('2019-05-12',1),('2019-05-13',2),('2019-05-14',3),('2019-05-15',4),('2019-05-16',5),('2019-05-17',6),('2019-05-18',7),('2019-05-19',1),('2019-05-20',2),('2019-05-21',3),('2019-05-22',4),('2019-05-23',5),('2019-05-24',6),('2019-05-25',7),('2019-05-26',1),('2019-05-27',2),('2019-05-28',3),('2019-05-29',4),('2019-05-30',5),('2019-05-31',6),('2019-06-01',7),('2019-06-02',1),('2019-06-03',2),('2019-06-04',3),('2019-06-05',4),('2019-06-06',5),('2019-06-07',6),('2019-06-08',7),('2019-06-09',1),('2019-06-10',2),('2019-06-11',3),('2019-06-12',4),('2019-06-13',5),('2019-06-14',6),('2019-06-15',7),('2019-06-16',1),('2019-06-17',2),('2019-06-18',3),('2019-06-19',4),('2019-06-20',5),('2019-06-21',6),('2019-06-22',7),('2019-06-23',1),('2019-06-24',2),('2019-06-25',3),('2019-06-26',4),('2019-06-27',5),('2019-06-28',6),('2019-06-29',7),('2019-06-30',1),('2019-07-01',2),('2019-07-02',3),('2019-07-03',4),('2019-07-04',5),('2019-07-05',6),('2019-07-06',7),('2019-07-07',1),('2019-07-08',2),('2019-07-09',3),('2019-07-10',4),('2019-07-11',5),('2019-07-12',6),('2019-07-13',7),('2019-07-14',1),('2019-07-15',2),('2019-07-16',3),('2019-07-17',4),('2019-07-18',5),('2019-07-19',6),('2019-07-20',7),('2019-07-21',1),('2019-07-22',2),('2019-07-23',3),('2019-07-24',4),('2019-07-25',5),('2019-07-26',6),('2019-07-27',7),('2019-07-28',1),('2019-07-29',2),('2019-07-30',3),('2019-07-31',4),('2019-08-01',5),('2019-08-02',6),('2019-08-03',7),('2019-08-04',1),('2019-08-05',2),('2019-08-06',3),('2019-08-07',4),('2019-08-08',5),('2019-08-09',6),('2019-08-10',7),('2019-08-11',1),('2019-08-12',2),('2019-08-13',3),('2019-08-14',4),('2019-08-15',5),('2019-08-16',6),('2019-08-17',7),('2019-08-18',1),('2019-08-19',2),('2019-08-20',3),('2019-08-21',4),('2019-08-22',5),('2019-08-23',6),('2019-08-24',7),('2019-08-25',1),('2019-08-26',2),('2019-08-27',3),('2019-08-28',4),('2019-08-29',5),('2019-08-30',6),('2019-08-31',7),('2019-09-01',1),('2019-09-02',2),('2019-09-03',3),('2019-09-04',4),('2019-09-05',5),('2019-09-06',6),('2019-09-07',7),('2019-09-08',1),('2019-09-09',2),('2019-09-10',3),('2019-09-11',4),('2019-09-12',5),('2019-09-13',6),('2019-09-14',7),('2019-09-15',1),('2019-09-16',2),('2019-09-17',3),('2019-09-18',4),('2019-09-19',5),('2019-09-20',6),('2019-09-21',7),('2019-09-22',1),('2019-09-23',2),('2019-09-24',3),('2019-09-25',4),('2019-09-26',5),('2019-09-27',6),('2019-09-28',7),('2019-09-29',1),('2019-09-30',2),('2019-10-01',3),('2019-10-02',4),('2019-10-03',5),('2019-10-04',6),('2019-10-05',7),('2019-10-06',1),('2019-10-07',2),('2019-10-08',3),('2019-10-09',4),('2019-10-10',5),('2019-10-11',6),('2019-10-12',7),('2019-10-13',1),('2019-10-14',2),('2019-10-15',3),('2019-10-16',4),('2019-10-17',5),('2019-10-18',6),('2019-10-19',7),('2019-10-20',1),('2019-10-21',2),('2019-10-22',3),('2019-10-23',4),('2019-10-24',5),('2019-10-25',6),('2019-10-26',7),('2019-10-27',1),('2019-10-28',2),('2019-10-29',3),('2019-10-30',4),('2019-10-31',5),('2019-11-01',6),('2019-11-02',7),('2019-11-03',1),('2019-11-04',2),('2019-11-05',3),('2019-11-06',4),('2019-11-07',5),('2019-11-08',6),('2019-11-09',7),('2019-11-10',1),('2019-11-11',2),('2019-11-12',3),('2019-11-13',4),('2019-11-14',5),('2019-11-15',6),('2019-11-16',7),('2019-11-17',1),('2019-11-18',2),('2019-11-19',3),('2019-11-20',4),('2019-11-21',5),('2019-11-22',6),('2019-11-23',7),('2019-11-24',1),('2019-11-25',2),('2019-11-26',3),('2019-11-27',4),('2019-11-28',5),('2019-11-29',6),('2019-11-30',7),('2019-12-01',1),('2019-12-02',2),('2019-12-03',3),('2019-12-04',4),('2019-12-05',5),('2019-12-06',6),('2019-12-07',7),('2019-12-08',1),('2019-12-09',2),('2019-12-10',3),('2019-12-11',4),('2019-12-12',5),('2019-12-13',6),('2019-12-14',7),('2019-12-15',1),('2019-12-16',2),('2019-12-17',3),('2019-12-18',4),('2019-12-19',5),('2019-12-20',6),('2019-12-21',7),('2019-12-22',1),('2019-12-23',2),('2019-12-24',3),('2019-12-25',4),('2019-12-26',5),('2019-12-27',6),('2019-12-28',7),('2019-12-29',1),('2019-12-30',2),('2019-12-31',3),('2020-01-01',4),('2020-01-02',5),('2020-01-03',6),('2020-01-04',7),('2020-01-05',1),('2020-01-06',2),('2020-01-07',3),('2020-01-08',4),('2020-01-09',5),('2020-01-10',6),('2020-01-11',7),('2020-01-12',1),('2020-01-13',2),('2020-01-14',3),('2020-01-15',4),('2020-01-16',5),('2020-01-17',6),('2020-01-18',7),('2020-01-19',1),('2020-01-20',2),('2020-01-21',3),('2020-01-22',4),('2020-01-23',5),('2020-01-24',6),('2020-01-25',7),('2020-01-26',1),('2020-01-27',2),('2020-01-28',3),('2020-01-29',4),('2020-01-30',5),('2020-01-31',6),('2020-02-01',7),('2020-02-02',1),('2020-02-03',2),('2020-02-04',3),('2020-02-05',4),('2020-02-06',5),('2020-02-07',6),('2020-02-08',7),('2020-02-09',1),('2020-02-10',2),('2020-02-11',3),('2020-02-12',4),('2020-02-13',5),('2020-02-14',6),('2020-02-15',7),('2020-02-16',1),('2020-02-17',2),('2020-02-18',3),('2020-02-19',4),('2020-02-20',5),('2020-02-21',6),('2020-02-22',7),('2020-02-23',1),('2020-02-24',2),('2020-02-25',3),('2020-02-26',4),('2020-02-27',5),('2020-02-28',6),('2020-02-29',7),('2020-03-01',1),('2020-03-02',2),('2020-03-03',3),('2020-03-04',4),('2020-03-05',5),('2020-03-06',6),('2020-03-07',7),('2020-03-08',1),('2020-03-09',2),('2020-03-10',3),('2020-03-11',4),('2020-03-12',5),('2020-03-13',6),('2020-03-14',7),('2020-03-15',1),('2020-03-16',2),('2020-03-17',3),('2020-03-18',4),('2020-03-19',5),('2020-03-20',6),('2020-03-21',7),('2020-03-22',1),('2020-03-23',2),('2020-03-24',3),('2020-03-25',4),('2020-03-26',5),('2020-03-27',6),('2020-03-28',7),('2020-03-29',1),('2020-03-30',2),('2020-03-31',3),('2020-04-01',4),('2020-04-02',5),('2020-04-03',6),('2020-04-04',7),('2020-04-05',1),('2020-04-06',2),('2020-04-07',3),('2020-04-08',4),('2020-04-09',5),('2020-04-10',6),('2020-04-11',7),('2020-04-12',1),('2020-04-13',2),('2020-04-14',3),('2020-04-15',4),('2020-04-16',5),('2020-04-17',6),('2020-04-18',7),('2020-04-19',1),('2020-04-20',2),('2020-04-21',3),('2020-04-22',4),('2020-04-23',5),('2020-04-24',6),('2020-04-25',7),('2020-04-26',1),('2020-04-27',2),('2020-04-28',3),('2020-04-29',4),('2020-04-30',5),('2020-05-01',6),('2020-05-02',7),('2020-05-03',1),('2020-05-04',2),('2020-05-05',3),('2020-05-06',4),('2020-05-07',5),('2020-05-08',6),('2020-05-09',7),('2020-05-10',1),('2020-05-11',2),('2020-05-12',3),('2020-05-13',4),('2020-05-14',5),('2020-05-15',6),('2020-05-16',7),('2020-05-17',1),('2020-05-18',2),('2020-05-19',3),('2020-05-20',4),('2020-05-21',5),('2020-05-22',6),('2020-05-23',7),('2020-05-24',1),('2020-05-25',2),('2020-05-26',3),('2020-05-27',4),('2020-05-28',5),('2020-05-29',6),('2020-05-30',7),('2020-05-31',1),('2020-06-01',2),('2020-06-02',3),('2020-06-03',4),('2020-06-04',5),('2020-06-05',6),('2020-06-06',7),('2020-06-07',1),('2020-06-08',2),('2020-06-09',3),('2020-06-10',4),('2020-06-11',5),('2020-06-12',6),('2020-06-13',7),('2020-06-14',1),('2020-06-15',2),('2020-06-16',3),('2020-06-17',4),('2020-06-18',5),('2020-06-19',6),('2020-06-20',7),('2020-06-21',1),('2020-06-22',2),('2020-06-23',3),('2020-06-24',4),('2020-06-25',5),('2020-06-26',6),('2020-06-27',7),('2020-06-28',1),('2020-06-29',2),('2020-06-30',3),('2020-07-01',4),('2020-07-02',5),('2020-07-03',6),('2020-07-04',7),('2020-07-05',1),('2020-07-06',2),('2020-07-07',3),('2020-07-08',4),('2020-07-09',5),('2020-07-10',6),('2020-07-11',7),('2020-07-12',1),('2020-07-13',2),('2020-07-14',3),('2020-07-15',4),('2020-07-16',5),('2020-07-17',6),('2020-07-18',7),('2020-07-19',1),('2020-07-20',2),('2020-07-21',3),('2020-07-22',4),('2020-07-23',5),('2020-07-24',6),('2020-07-25',7),('2020-07-26',1),('2020-07-27',2),('2020-07-28',3),('2020-07-29',4),('2020-07-30',5),('2020-07-31',6),('2020-08-01',7),('2020-08-02',1),('2020-08-03',2),('2020-08-04',3),('2020-08-05',4),('2020-08-06',5),('2020-08-07',6),('2020-08-08',7),('2020-08-09',1),('2020-08-10',2),('2020-08-11',3),('2020-08-12',4),('2020-08-13',5),('2020-08-14',6),('2020-08-15',7),('2020-08-16',1),('2020-08-17',2),('2020-08-18',3),('2020-08-19',4),('2020-08-20',5),('2020-08-21',6),('2020-08-22',7),('2020-08-23',1),('2020-08-24',2),('2020-08-25',3),('2020-08-26',4),('2020-08-27',5),('2020-08-28',6),('2020-08-29',7),('2020-08-30',1),('2020-08-31',2),('2020-09-01',3),('2020-09-02',4),('2020-09-03',5),('2020-09-04',6),('2020-09-05',7),('2020-09-06',1),('2020-09-07',2),('2020-09-08',3),('2020-09-09',4),('2020-09-10',5),('2020-09-11',6),('2020-09-12',7),('2020-09-13',1),('2020-09-14',2),('2020-09-15',3),('2020-09-16',4),('2020-09-17',5),('2020-09-18',6),('2020-09-19',7),('2020-09-20',1),('2020-09-21',2),('2020-09-22',3),('2020-09-23',4),('2020-09-24',5),('2020-09-25',6),('2020-09-26',7),('2020-09-27',1),('2020-09-28',2),('2020-09-29',3),('2020-09-30',4),('2020-10-01',5),('2020-10-02',6),('2020-10-03',7),('2020-10-04',1),('2020-10-05',2),('2020-10-06',3),('2020-10-07',4),('2020-10-08',5),('2020-10-09',6),('2020-10-10',7),('2020-10-11',1),('2020-10-12',2),('2020-10-13',3),('2020-10-14',4),('2020-10-15',5),('2020-10-16',6),('2020-10-17',7),('2020-10-18',1),('2020-10-19',2),('2020-10-20',3),('2020-10-21',4),('2020-10-22',5),('2020-10-23',6),('2020-10-24',7),('2020-10-25',1),('2020-10-26',2),('2020-10-27',3),('2020-10-28',4),('2020-10-29',5),('2020-10-30',6),('2020-10-31',7),('2020-11-01',1),('2020-11-02',2),('2020-11-03',3),('2020-11-04',4),('2020-11-05',5),('2020-11-06',6),('2020-11-07',7),('2020-11-08',1),('2020-11-09',2),('2020-11-10',3),('2020-11-11',4),('2020-11-12',5),('2020-11-13',6),('2020-11-14',7),('2020-11-15',1),('2020-11-16',2),('2020-11-17',3),('2020-11-18',4),('2020-11-19',5),('2020-11-20',6),('2020-11-21',7),('2020-11-22',1),('2020-11-23',2),('2020-11-24',3),('2020-11-25',4),('2020-11-26',5),('2020-11-27',6),('2020-11-28',7),('2020-11-29',1),('2020-11-30',2),('2020-12-01',3),('2020-12-02',4),('2020-12-03',5),('2020-12-04',6),('2020-12-05',7),('2020-12-06',1),('2020-12-07',2),('2020-12-08',3),('2020-12-09',4),('2020-12-10',5),('2020-12-11',6),('2020-12-12',7),('2020-12-13',1),('2020-12-14',2),('2020-12-15',3),('2020-12-16',4),('2020-12-17',5),('2020-12-18',6),('2020-12-19',7),('2020-12-20',1),('2020-12-21',2),('2020-12-22',3),('2020-12-23',4),('2020-12-24',5),('2020-12-25',6),('2020-12-26',7),('2020-12-27',1),('2020-12-28',2),('2020-12-29',3),('2020-12-30',4),('2020-12-31',5),('2021-01-01',6),('2021-01-02',7),('2021-01-03',1),('2021-01-04',2),('2021-01-05',3),('2021-01-06',4),('2021-01-07',5),('2021-01-08',6),('2021-01-09',7),('2021-01-10',1),('2021-01-11',2),('2021-01-12',3),('2021-01-13',4),('2021-01-14',5),('2021-01-15',6),('2021-01-16',7),('2021-01-17',1),('2021-01-18',2),('2021-01-19',3),('2021-01-20',4),('2021-01-21',5),('2021-01-22',6),('2021-01-23',7),('2021-01-24',1),('2021-01-25',2),('2021-01-26',3),('2021-01-27',4),('2021-01-28',5),('2021-01-29',6),('2021-01-30',7),('2021-01-31',1),('2021-02-01',2),('2021-02-02',3),('2021-02-03',4),('2021-02-04',5),('2021-02-05',6),('2021-02-06',7),('2021-02-07',1),('2021-02-08',2),('2021-02-09',3),('2021-02-10',4),('2021-02-11',5),('2021-02-12',6),('2021-02-13',7),('2021-02-14',1),('2021-02-15',2),('2021-02-16',3),('2021-02-17',4),('2021-02-18',5),('2021-02-19',6),('2021-02-20',7),('2021-02-21',1),('2021-02-22',2),('2021-02-23',3),('2021-02-24',4),('2021-02-25',5),('2021-02-26',6),('2021-02-27',7),('2021-02-28',1),('2021-03-01',2),('2021-03-02',3),('2021-03-03',4),('2021-03-04',5),('2021-03-05',6),('2021-03-06',7),('2021-03-07',1),('2021-03-08',2),('2021-03-09',3),('2021-03-10',4),('2021-03-11',5),('2021-03-12',6),('2021-03-13',7),('2021-03-14',1),('2021-03-15',2),('2021-03-16',3),('2021-03-17',4),('2021-03-18',5),('2021-03-19',6),('2021-03-20',7),('2021-03-21',1),('2021-03-22',2),('2021-03-23',3),('2021-03-24',4),('2021-03-25',5),('2021-03-26',6),('2021-03-27',7),('2021-03-28',1),('2021-03-29',2),('2021-03-30',3),('2021-03-31',4),('2021-04-01',5),('2021-04-02',6),('2021-04-03',7),('2021-04-04',1),('2021-04-05',2),('2021-04-06',3),('2021-04-07',4),('2021-04-08',5),('2021-04-09',6),('2021-04-10',7),('2021-04-11',1),('2021-04-12',2),('2021-04-13',3),('2021-04-14',4),('2021-04-15',5),('2021-04-16',6),('2021-04-17',7),('2021-04-18',1),('2021-04-19',2),('2021-04-20',3),('2021-04-21',4),('2021-04-22',5),('2021-04-23',6),('2021-04-24',7),('2021-04-25',1),('2021-04-26',2),('2021-04-27',3),('2021-04-28',4),('2021-04-29',5),('2021-04-30',6),('2021-05-01',7),('2021-05-02',1),('2021-05-03',2),('2021-05-04',3),('2021-05-05',4),('2021-05-06',5),('2021-05-07',6),('2021-05-08',7),('2021-05-09',1),('2021-05-10',2),('2021-05-11',3),('2021-05-12',4),('2021-05-13',5),('2021-05-14',6),('2021-05-15',7),('2021-05-16',1),('2021-05-17',2),('2021-05-18',3),('2021-05-19',4),('2021-05-20',5),('2021-05-21',6),('2021-05-22',7),('2021-05-23',1),('2021-05-24',2),('2021-05-25',3),('2021-05-26',4),('2021-05-27',5),('2021-05-28',6),('2021-05-29',7),('2021-05-30',1),('2021-05-31',2),('2021-06-01',3),('2021-06-02',4),('2021-06-03',5),('2021-06-04',6),('2021-06-05',7),('2021-06-06',1),('2021-06-07',2),('2021-06-08',3),('2021-06-09',4),('2021-06-10',5),('2021-06-11',6),('2021-06-12',7),('2021-06-13',1),('2021-06-14',2),('2021-06-15',3),('2021-06-16',4),('2021-06-17',5),('2021-06-18',6),('2021-06-19',7),('2021-06-20',1),('2021-06-21',2),('2021-06-22',3),('2021-06-23',4),('2021-06-24',5),('2021-06-25',6),('2021-06-26',7),('2021-06-27',1),('2021-06-28',2),('2021-06-29',3),('2021-06-30',4),('2021-07-01',5),('2021-07-02',6),('2021-07-03',7),('2021-07-04',1),('2021-07-05',2),('2021-07-06',3),('2021-07-07',4),('2021-07-08',5),('2021-07-09',6),('2021-07-10',7),('2021-07-11',1),('2021-07-12',2),('2021-07-13',3),('2021-07-14',4),('2021-07-15',5),('2021-07-16',6),('2021-07-17',7),('2021-07-18',1),('2021-07-19',2),('2021-07-20',3),('2021-07-21',4),('2021-07-22',5),('2021-07-23',6),('2021-07-24',7),('2021-07-25',1),('2021-07-26',2),('2021-07-27',3),('2021-07-28',4),('2021-07-29',5),('2021-07-30',6),('2021-07-31',7),('2021-08-01',1),('2021-08-02',2),('2021-08-03',3),('2021-08-04',4),('2021-08-05',5),('2021-08-06',6),('2021-08-07',7),('2021-08-08',1),('2021-08-09',2),('2021-08-10',3),('2021-08-11',4),('2021-08-12',5),('2021-08-13',6),('2021-08-14',7),('2021-08-15',1),('2021-08-16',2),('2021-08-17',3),('2021-08-18',4),('2021-08-19',5),('2021-08-20',6),('2021-08-21',7),('2021-08-22',1),('2021-08-23',2),('2021-08-24',3),('2021-08-25',4),('2021-08-26',5),('2021-08-27',6),('2021-08-28',7),('2021-08-29',1),('2021-08-30',2),('2021-08-31',3),('2021-09-01',4),('2021-09-02',5),('2021-09-03',6),('2021-09-04',7),('2021-09-05',1),('2021-09-06',2),('2021-09-07',3),('2021-09-08',4),('2021-09-09',5),('2021-09-10',6),('2021-09-11',7),('2021-09-12',1),('2021-09-13',2),('2021-09-14',3),('2021-09-15',4),('2021-09-16',5),('2021-09-17',6),('2021-09-18',7),('2021-09-19',1),('2021-09-20',2),('2021-09-21',3),('2021-09-22',4),('2021-09-23',5),('2021-09-24',6),('2021-09-25',7),('2021-09-26',1),('2021-09-27',2),('2021-09-28',3),('2021-09-29',4),('2021-09-30',5),('2021-10-01',6),('2021-10-02',7),('2021-10-03',1),('2021-10-04',2),('2021-10-05',3),('2021-10-06',4),('2021-10-07',5),('2021-10-08',6),('2021-10-09',7),('2021-10-10',1),('2021-10-11',2),('2021-10-12',3),('2021-10-13',4),('2021-10-14',5),('2021-10-15',6),('2021-10-16',7),('2021-10-17',1),('2021-10-18',2),('2021-10-19',3),('2021-10-20',4),('2021-10-21',5),('2021-10-22',6),('2021-10-23',7),('2021-10-24',1),('2021-10-25',2),('2021-10-26',3),('2021-10-27',4),('2021-10-28',5),('2021-10-29',6),('2021-10-30',7),('2021-10-31',1),('2021-11-01',2),('2021-11-02',3),('2021-11-03',4),('2021-11-04',5),('2021-11-05',6),('2021-11-06',7),('2021-11-07',1),('2021-11-08',2),('2021-11-09',3),('2021-11-10',4),('2021-11-11',5),('2021-11-12',6),('2021-11-13',7),('2021-11-14',1),('2021-11-15',2),('2021-11-16',3),('2021-11-17',4),('2021-11-18',5),('2021-11-19',6),('2021-11-20',7),('2021-11-21',1),('2021-11-22',2),('2021-11-23',3),('2021-11-24',4),('2021-11-25',5),('2021-11-26',6),('2021-11-27',7),('2021-11-28',1),('2021-11-29',2),('2021-11-30',3),('2021-12-01',4),('2021-12-02',5),('2021-12-03',6),('2021-12-04',7),('2021-12-05',1),('2021-12-06',2),('2021-12-07',3),('2021-12-08',4),('2021-12-09',5),('2021-12-10',6),('2021-12-11',7),('2021-12-12',1),('2021-12-13',2),('2021-12-14',3),('2021-12-15',4),('2021-12-16',5),('2021-12-17',6),('2021-12-18',7),('2021-12-19',1),('2021-12-20',2),('2021-12-21',3),('2021-12-22',4),('2021-12-23',5),('2021-12-24',6),('2021-12-25',7),('2021-12-26',1),('2021-12-27',2),('2021-12-28',3),('2021-12-29',4),('2021-12-30',5),('2021-12-31',6),('2022-01-01',7),('2022-01-02',1),('2022-01-03',2),('2022-01-04',3),('2022-01-05',4),('2022-01-06',5),('2022-01-07',6),('2022-01-08',7),('2022-01-09',1),('2022-01-10',2),('2022-01-11',3),('2022-01-12',4),('2022-01-13',5),('2022-01-14',6),('2022-01-15',7),('2022-01-16',1),('2022-01-17',2),('2022-01-18',3),('2022-01-19',4),('2022-01-20',5),('2022-01-21',6),('2022-01-22',7),('2022-01-23',1),('2022-01-24',2),('2022-01-25',3),('2022-01-26',4),('2022-01-27',5),('2022-01-28',6),('2022-01-29',7),('2022-01-30',1),('2022-01-31',2),('2022-02-01',3),('2022-02-02',4),('2022-02-03',5),('2022-02-04',6),('2022-02-05',7),('2022-02-06',1),('2022-02-07',2),('2022-02-08',3),('2022-02-09',4),('2022-02-10',5),('2022-02-11',6),('2022-02-12',7),('2022-02-13',1),('2022-02-14',2),('2022-02-15',3),('2022-02-16',4),('2022-02-17',5),('2022-02-18',6),('2022-02-19',7),('2022-02-20',1),('2022-02-21',2),('2022-02-22',3),('2022-02-23',4),('2022-02-24',5),('2022-02-25',6),('2022-02-26',7),('2022-02-27',1),('2022-02-28',2),('2022-03-01',3),('2022-03-02',4),('2022-03-03',5),('2022-03-04',6),('2022-03-05',7),('2022-03-06',1),('2022-03-07',2),('2022-03-08',3),('2022-03-09',4),('2022-03-10',5),('2022-03-11',6),('2022-03-12',7),('2022-03-13',1),('2022-03-14',2),('2022-03-15',3),('2022-03-16',4),('2022-03-17',5),('2022-03-18',6),('2022-03-19',7),('2022-03-20',1),('2022-03-21',2),('2022-03-22',3),('2022-03-23',4),('2022-03-24',5),('2022-03-25',6),('2022-03-26',7),('2022-03-27',1),('2022-03-28',2),('2022-03-29',3),('2022-03-30',4),('2022-03-31',5),('2022-04-01',6),('2022-04-02',7),('2022-04-03',1),('2022-04-04',2),('2022-04-05',3),('2022-04-06',4),('2022-04-07',5),('2022-04-08',6),('2022-04-09',7),('2022-04-10',1),('2022-04-11',2),('2022-04-12',3),('2022-04-13',4),('2022-04-14',5),('2022-04-15',6),('2022-04-16',7),('2022-04-17',1),('2022-04-18',2),('2022-04-19',3),('2022-04-20',4),('2022-04-21',5),('2022-04-22',6),('2022-04-23',7),('2022-04-24',1),('2022-04-25',2),('2022-04-26',3),('2022-04-27',4),('2022-04-28',5),('2022-04-29',6),('2022-04-30',7),('2022-05-01',1),('2022-05-02',2),('2022-05-03',3),('2022-05-04',4),('2022-05-05',5),('2022-05-06',6),('2022-05-07',7),('2022-05-08',1),('2022-05-09',2),('2022-05-10',3),('2022-05-11',4),('2022-05-12',5),('2022-05-13',6),('2022-05-14',7),('2022-05-15',1),('2022-05-16',2),('2022-05-17',3),('2022-05-18',4),('2022-05-19',5),('2022-05-20',6),('2022-05-21',7),('2022-05-22',1),('2022-05-23',2),('2022-05-24',3),('2022-05-25',4),('2022-05-26',5),('2022-05-27',6),('2022-05-28',7),('2022-05-29',1),('2022-05-30',2),('2022-05-31',3),('2022-06-01',4),('2022-06-02',5),('2022-06-03',6),('2022-06-04',7),('2022-06-05',1),('2022-06-06',2),('2022-06-07',3),('2022-06-08',4),('2022-06-09',5),('2022-06-10',6),('2022-06-11',7),('2022-06-12',1),('2022-06-13',2),('2022-06-14',3),('2022-06-15',4),('2022-06-16',5),('2022-06-17',6),('2022-06-18',7),('2022-06-19',1),('2022-06-20',2),('2022-06-21',3),('2022-06-22',4),('2022-06-23',5),('2022-06-24',6),('2022-06-25',7),('2022-06-26',1),('2022-06-27',2),('2022-06-28',3),('2022-06-29',4),('2022-06-30',5),('2022-07-01',6),('2022-07-02',7),('2022-07-03',1),('2022-07-04',2),('2022-07-05',3),('2022-07-06',4),('2022-07-07',5),('2022-07-08',6),('2022-07-09',7),('2022-07-10',1),('2022-07-11',2),('2022-07-12',3),('2022-07-13',4),('2022-07-14',5),('2022-07-15',6),('2022-07-16',7),('2022-07-17',1),('2022-07-18',2),('2022-07-19',3),('2022-07-20',4),('2022-07-21',5),('2022-07-22',6),('2022-07-23',7),('2022-07-24',1),('2022-07-25',2),('2022-07-26',3),('2022-07-27',4),('2022-07-28',5),('2022-07-29',6),('2022-07-30',7),('2022-07-31',1),('2022-08-01',2),('2022-08-02',3),('2022-08-03',4),('2022-08-04',5),('2022-08-05',6),('2022-08-06',7),('2022-08-07',1),('2022-08-08',2),('2022-08-09',3),('2022-08-10',4),('2022-08-11',5),('2022-08-12',6),('2022-08-13',7),('2022-08-14',1),('2022-08-15',2),('2022-08-16',3),('2022-08-17',4),('2022-08-18',5),('2022-08-19',6),('2022-08-20',7),('2022-08-21',1),('2022-08-22',2),('2022-08-23',3),('2022-08-24',4),('2022-08-25',5),('2022-08-26',6),('2022-08-27',7),('2022-08-28',1),('2022-08-29',2),('2022-08-30',3),('2022-08-31',4),('2022-09-01',5),('2022-09-02',6),('2022-09-03',7),('2022-09-04',1),('2022-09-05',2),('2022-09-06',3),('2022-09-07',4),('2022-09-08',5),('2022-09-09',6),('2022-09-10',7),('2022-09-11',1),('2022-09-12',2),('2022-09-13',3),('2022-09-14',4),('2022-09-15',5),('2022-09-16',6),('2022-09-17',7),('2022-09-18',1),('2022-09-19',2),('2022-09-20',3),('2022-09-21',4),('2022-09-22',5),('2022-09-23',6),('2022-09-24',7),('2022-09-25',1),('2022-09-26',2),('2022-09-27',3),('2022-09-28',4),('2022-09-29',5),('2022-09-30',6),('2022-10-01',7),('2022-10-02',1),('2022-10-03',2),('2022-10-04',3),('2022-10-05',4),('2022-10-06',5),('2022-10-07',6),('2022-10-08',7),('2022-10-09',1),('2022-10-10',2),('2022-10-11',3),('2022-10-12',4),('2022-10-13',5),('2022-10-14',6),('2022-10-15',7),('2022-10-16',1),('2022-10-17',2),('2022-10-18',3),('2022-10-19',4),('2022-10-20',5),('2022-10-21',6),('2022-10-22',7),('2022-10-23',1),('2022-10-24',2),('2022-10-25',3),('2022-10-26',4),('2022-10-27',5),('2022-10-28',6),('2022-10-29',7),('2022-10-30',1),('2022-10-31',2),('2022-11-01',3),('2022-11-02',4),('2022-11-03',5),('2022-11-04',6),('2022-11-05',7),('2022-11-06',1),('2022-11-07',2),('2022-11-08',3),('2022-11-09',4),('2022-11-10',5),('2022-11-11',6),('2022-11-12',7),('2022-11-13',1),('2022-11-14',2),('2022-11-15',3),('2022-11-16',4),('2022-11-17',5),('2022-11-18',6),('2022-11-19',7),('2022-11-20',1),('2022-11-21',2),('2022-11-22',3),('2022-11-23',4),('2022-11-24',5),('2022-11-25',6),('2022-11-26',7),('2022-11-27',1),('2022-11-28',2),('2022-11-29',3),('2022-11-30',4),('2022-12-01',5),('2022-12-02',6),('2022-12-03',7),('2022-12-04',1),('2022-12-05',2),('2022-12-06',3),('2022-12-07',4),('2022-12-08',5),('2022-12-09',6),('2022-12-10',7),('2022-12-11',1),('2022-12-12',2),('2022-12-13',3),('2022-12-14',4),('2022-12-15',5),('2022-12-16',6),('2022-12-17',7),('2022-12-18',1),('2022-12-19',2),('2022-12-20',3),('2022-12-21',4),('2022-12-22',5),('2022-12-23',6),('2022-12-24',7),('2022-12-25',1),('2022-12-26',2),('2022-12-27',3),('2022-12-28',4),('2022-12-29',5),('2022-12-30',6),('2022-12-31',7),('2023-01-01',1),('2023-01-02',2),('2023-01-03',3),('2023-01-04',4),('2023-01-05',5),('2023-01-06',6),('2023-01-07',7),('2023-01-08',1),('2023-01-09',2),('2023-01-10',3),('2023-01-11',4),('2023-01-12',5),('2023-01-13',6),('2023-01-14',7),('2023-01-15',1),('2023-01-16',2),('2023-01-17',3),('2023-01-18',4),('2023-01-19',5),('2023-01-20',6),('2023-01-21',7),('2023-01-22',1),('2023-01-23',2),('2023-01-24',3),('2023-01-25',4),('2023-01-26',5),('2023-01-27',6),('2023-01-28',7),('2023-01-29',1),('2023-01-30',2),('2023-01-31',3),('2023-02-01',4),('2023-02-02',5),('2023-02-03',6),('2023-02-04',7),('2023-02-05',1),('2023-02-06',2),('2023-02-07',3),('2023-02-08',4),('2023-02-09',5),('2023-02-10',6),('2023-02-11',7),('2023-02-12',1),('2023-02-13',2),('2023-02-14',3),('2023-02-15',4),('2023-02-16',5),('2023-02-17',6),('2023-02-18',7),('2023-02-19',1),('2023-02-20',2),('2023-02-21',3),('2023-02-22',4),('2023-02-23',5),('2023-02-24',6),('2023-02-25',7),('2023-02-26',1),('2023-02-27',2),('2023-02-28',3),('2023-03-01',4),('2023-03-02',5),('2023-03-03',6),('2023-03-04',7),('2023-03-05',1),('2023-03-06',2),('2023-03-07',3),('2023-03-08',4),('2023-03-09',5),('2023-03-10',6),('2023-03-11',7),('2023-03-12',1),('2023-03-13',2),('2023-03-14',3),('2023-03-15',4),('2023-03-16',5),('2023-03-17',6),('2023-03-18',7),('2023-03-19',1),('2023-03-20',2),('2023-03-21',3),('2023-03-22',4),('2023-03-23',5),('2023-03-24',6),('2023-03-25',7),('2023-03-26',1),('2023-03-27',2),('2023-03-28',3),('2023-03-29',4),('2023-03-30',5),('2023-03-31',6),('2023-04-01',7),('2023-04-02',1),('2023-04-03',2),('2023-04-04',3),('2023-04-05',4),('2023-04-06',5),('2023-04-07',6),('2023-04-08',7),('2023-04-09',1),('2023-04-10',2),('2023-04-11',3),('2023-04-12',4),('2023-04-13',5),('2023-04-14',6),('2023-04-15',7),('2023-04-16',1),('2023-04-17',2),('2023-04-18',3),('2023-04-19',4),('2023-04-20',5),('2023-04-21',6),('2023-04-22',7),('2023-04-23',1),('2023-04-24',2),('2023-04-25',3),('2023-04-26',4),('2023-04-27',5),('2023-04-28',6),('2023-04-29',7),('2023-04-30',1),('2023-05-01',2),('2023-05-02',3),('2023-05-03',4),('2023-05-04',5),('2023-05-05',6),('2023-05-06',7),('2023-05-07',1),('2023-05-08',2),('2023-05-09',3),('2023-05-10',4),('2023-05-11',5),('2023-05-12',6),('2023-05-13',7),('2023-05-14',1),('2023-05-15',2),('2023-05-16',3),('2023-05-17',4),('2023-05-18',5),('2023-05-19',6),('2023-05-20',7),('2023-05-21',1),('2023-05-22',2),('2023-05-23',3),('2023-05-24',4),('2023-05-25',5),('2023-05-26',6),('2023-05-27',7),('2023-05-28',1),('2023-05-29',2),('2023-05-30',3),('2023-05-31',4),('2023-06-01',5),('2023-06-02',6),('2023-06-03',7),('2023-06-04',1),('2023-06-05',2),('2023-06-06',3),('2023-06-07',4),('2023-06-08',5),('2023-06-09',6),('2023-06-10',7),('2023-06-11',1),('2023-06-12',2),('2023-06-13',3),('2023-06-14',4),('2023-06-15',5),('2023-06-16',6),('2023-06-17',7),('2023-06-18',1),('2023-06-19',2),('2023-06-20',3),('2023-06-21',4),('2023-06-22',5),('2023-06-23',6),('2023-06-24',7),('2023-06-25',1),('2023-06-26',2),('2023-06-27',3),('2023-06-28',4),('2023-06-29',5),('2023-06-30',6),('2023-07-01',7),('2023-07-02',1),('2023-07-03',2),('2023-07-04',3),('2023-07-05',4),('2023-07-06',5),('2023-07-07',6),('2023-07-08',7),('2023-07-09',1),('2023-07-10',2),('2023-07-11',3),('2023-07-12',4),('2023-07-13',5),('2023-07-14',6),('2023-07-15',7),('2023-07-16',1),('2023-07-17',2),('2023-07-18',3),('2023-07-19',4),('2023-07-20',5),('2023-07-21',6),('2023-07-22',7),('2023-07-23',1),('2023-07-24',2),('2023-07-25',3),('2023-07-26',4),('2023-07-27',5),('2023-07-28',6),('2023-07-29',7),('2023-07-30',1),('2023-07-31',2),('2023-08-01',3),('2023-08-02',4),('2023-08-03',5),('2023-08-04',6),('2023-08-05',7),('2023-08-06',1),('2023-08-07',2),('2023-08-08',3),('2023-08-09',4),('2023-08-10',5),('2023-08-11',6),('2023-08-12',7),('2023-08-13',1),('2023-08-14',2),('2023-08-15',3),('2023-08-16',4),('2023-08-17',5),('2023-08-18',6),('2023-08-19',7),('2023-08-20',1),('2023-08-21',2),('2023-08-22',3),('2023-08-23',4),('2023-08-24',5),('2023-08-25',6),('2023-08-26',7),('2023-08-27',1),('2023-08-28',2),('2023-08-29',3),('2023-08-30',4),('2023-08-31',5),('2023-09-01',6),('2023-09-02',7),('2023-09-03',1),('2023-09-04',2),('2023-09-05',3),('2023-09-06',4),('2023-09-07',5),('2023-09-08',6),('2023-09-09',7),('2023-09-10',1),('2023-09-11',2),('2023-09-12',3),('2023-09-13',4),('2023-09-14',5),('2023-09-15',6),('2023-09-16',7),('2023-09-17',1),('2023-09-18',2),('2023-09-19',3),('2023-09-20',4),('2023-09-21',5),('2023-09-22',6),('2023-09-23',7),('2023-09-24',1),('2023-09-25',2),('2023-09-26',3),('2023-09-27',4),('2023-09-28',5),('2023-09-29',6),('2023-09-30',7),('2023-10-01',1),('2023-10-02',2),('2023-10-03',3),('2023-10-04',4),('2023-10-05',5),('2023-10-06',6),('2023-10-07',7),('2023-10-08',1),('2023-10-09',2),('2023-10-10',3),('2023-10-11',4),('2023-10-12',5),('2023-10-13',6),('2023-10-14',7),('2023-10-15',1),('2023-10-16',2),('2023-10-17',3),('2023-10-18',4),('2023-10-19',5),('2023-10-20',6),('2023-10-21',7),('2023-10-22',1),('2023-10-23',2),('2023-10-24',3),('2023-10-25',4),('2023-10-26',5),('2023-10-27',6),('2023-10-28',7),('2023-10-29',1),('2023-10-30',2),('2023-10-31',3),('2023-11-01',4),('2023-11-02',5),('2023-11-03',6),('2023-11-04',7),('2023-11-05',1),('2023-11-06',2),('2023-11-07',3),('2023-11-08',4),('2023-11-09',5),('2023-11-10',6),('2023-11-11',7),('2023-11-12',1),('2023-11-13',2),('2023-11-14',3),('2023-11-15',4),('2023-11-16',5),('2023-11-17',6),('2023-11-18',7),('2023-11-19',1),('2023-11-20',2),('2023-11-21',3),('2023-11-22',4),('2023-11-23',5),('2023-11-24',6),('2023-11-25',7),('2023-11-26',1),('2023-11-27',2),('2023-11-28',3),('2023-11-29',4),('2023-11-30',5),('2023-12-01',6),('2023-12-02',7),('2023-12-03',1),('2023-12-04',2),('2023-12-05',3),('2023-12-06',4),('2023-12-07',5),('2023-12-08',6),('2023-12-09',7),('2023-12-10',1),('2023-12-11',2),('2023-12-12',3),('2023-12-13',4),('2023-12-14',5),('2023-12-15',6),('2023-12-16',7),('2023-12-17',1),('2023-12-18',2),('2023-12-19',3),('2023-12-20',4),('2023-12-21',5),('2023-12-22',6),('2023-12-23',7),('2023-12-24',1),('2023-12-25',2),('2023-12-26',3),('2023-12-27',4),('2023-12-28',5),('2023-12-29',6),('2023-12-30',7),('2023-12-31',1),('2024-01-01',2),('2024-01-02',3),('2024-01-03',4),('2024-01-04',5),('2024-01-05',6),('2024-01-06',7),('2024-01-07',1),('2024-01-08',2),('2024-01-09',3),('2024-01-10',4),('2024-01-11',5),('2024-01-12',6),('2024-01-13',7),('2024-01-14',1),('2024-01-15',2),('2024-01-16',3),('2024-01-17',4),('2024-01-18',5),('2024-01-19',6),('2024-01-20',7),('2024-01-21',1),('2024-01-22',2),('2024-01-23',3),('2024-01-24',4),('2024-01-25',5),('2024-01-26',6),('2024-01-27',7),('2024-01-28',1),('2024-01-29',2),('2024-01-30',3),('2024-01-31',4),('2024-02-01',5),('2024-02-02',6),('2024-02-03',7),('2024-02-04',1),('2024-02-05',2),('2024-02-06',3),('2024-02-07',4),('2024-02-08',5),('2024-02-09',6),('2024-02-10',7),('2024-02-11',1),('2024-02-12',2),('2024-02-13',3),('2024-02-14',4),('2024-02-15',5),('2024-02-16',6),('2024-02-17',7),('2024-02-18',1),('2024-02-19',2),('2024-02-20',3),('2024-02-21',4),('2024-02-22',5),('2024-02-23',6),('2024-02-24',7),('2024-02-25',1),('2024-02-26',2),('2024-02-27',3),('2024-02-28',4),('2024-02-29',5),('2024-03-01',6),('2024-03-02',7),('2024-03-03',1),('2024-03-04',2),('2024-03-05',3),('2024-03-06',4),('2024-03-07',5),('2024-03-08',6),('2024-03-09',7),('2024-03-10',1),('2024-03-11',2),('2024-03-12',3),('2024-03-13',4),('2024-03-14',5),('2024-03-15',6),('2024-03-16',7),('2024-03-17',1),('2024-03-18',2),('2024-03-19',3),('2024-03-20',4),('2024-03-21',5),('2024-03-22',6),('2024-03-23',7),('2024-03-24',1),('2024-03-25',2),('2024-03-26',3),('2024-03-27',4),('2024-03-28',5),('2024-03-29',6),('2024-03-30',7),('2024-03-31',1),('2024-04-01',2),('2024-04-02',3),('2024-04-03',4),('2024-04-04',5),('2024-04-05',6),('2024-04-06',7),('2024-04-07',1),('2024-04-08',2),('2024-04-09',3),('2024-04-10',4),('2024-04-11',5),('2024-04-12',6),('2024-04-13',7),('2024-04-14',1),('2024-04-15',2),('2024-04-16',3),('2024-04-17',4),('2024-04-18',5),('2024-04-19',6),('2024-04-20',7),('2024-04-21',1),('2024-04-22',2),('2024-04-23',3),('2024-04-24',4),('2024-04-25',5),('2024-04-26',6),('2024-04-27',7),('2024-04-28',1),('2024-04-29',2),('2024-04-30',3),('2024-05-01',4),('2024-05-02',5),('2024-05-03',6),('2024-05-04',7),('2024-05-05',1),('2024-05-06',2),('2024-05-07',3),('2024-05-08',4),('2024-05-09',5),('2024-05-10',6),('2024-05-11',7),('2024-05-12',1),('2024-05-13',2),('2024-05-14',3),('2024-05-15',4),('2024-05-16',5),('2024-05-17',6),('2024-05-18',7),('2024-05-19',1),('2024-05-20',2),('2024-05-21',3),('2024-05-22',4),('2024-05-23',5),('2024-05-24',6),('2024-05-25',7),('2024-05-26',1),('2024-05-27',2),('2024-05-28',3),('2024-05-29',4),('2024-05-30',5),('2024-05-31',6),('2024-06-01',7),('2024-06-02',1),('2024-06-03',2),('2024-06-04',3),('2024-06-05',4),('2024-06-06',5),('2024-06-07',6),('2024-06-08',7),('2024-06-09',1),('2024-06-10',2),('2024-06-11',3),('2024-06-12',4),('2024-06-13',5),('2024-06-14',6),('2024-06-15',7),('2024-06-16',1),('2024-06-17',2),('2024-06-18',3),('2024-06-19',4),('2024-06-20',5),('2024-06-21',6),('2024-06-22',7),('2024-06-23',1),('2024-06-24',2),('2024-06-25',3),('2024-06-26',4),('2024-06-27',5),('2024-06-28',6),('2024-06-29',7),('2024-06-30',1),('2024-07-01',2),('2024-07-02',3),('2024-07-03',4),('2024-07-04',5),('2024-07-05',6),('2024-07-06',7),('2024-07-07',1),('2024-07-08',2),('2024-07-09',3),('2024-07-10',4),('2024-07-11',5),('2024-07-12',6),('2024-07-13',7),('2024-07-14',1),('2024-07-15',2),('2024-07-16',3),('2024-07-17',4),('2024-07-18',5),('2024-07-19',6),('2024-07-20',7),('2024-07-21',1),('2024-07-22',2),('2024-07-23',3),('2024-07-24',4),('2024-07-25',5),('2024-07-26',6),('2024-07-27',7),('2024-07-28',1),('2024-07-29',2),('2024-07-30',3),('2024-07-31',4),('2024-08-01',5),('2024-08-02',6),('2024-08-03',7),('2024-08-04',1),('2024-08-05',2),('2024-08-06',3),('2024-08-07',4),('2024-08-08',5),('2024-08-09',6),('2024-08-10',7),('2024-08-11',1),('2024-08-12',2),('2024-08-13',3),('2024-08-14',4),('2024-08-15',5),('2024-08-16',6),('2024-08-17',7),('2024-08-18',1),('2024-08-19',2),('2024-08-20',3),('2024-08-21',4),('2024-08-22',5),('2024-08-23',6),('2024-08-24',7),('2024-08-25',1),('2024-08-26',2),('2024-08-27',3),('2024-08-28',4),('2024-08-29',5),('2024-08-30',6),('2024-08-31',7),('2024-09-01',1),('2024-09-02',2),('2024-09-03',3),('2024-09-04',4),('2024-09-05',5),('2024-09-06',6),('2024-09-07',7),('2024-09-08',1),('2024-09-09',2),('2024-09-10',3),('2024-09-11',4),('2024-09-12',5),('2024-09-13',6),('2024-09-14',7),('2024-09-15',1),('2024-09-16',2),('2024-09-17',3),('2024-09-18',4),('2024-09-19',5),('2024-09-20',6),('2024-09-21',7),('2024-09-22',1),('2024-09-23',2),('2024-09-24',3),('2024-09-25',4),('2024-09-26',5),('2024-09-27',6),('2024-09-28',7),('2024-09-29',1),('2024-09-30',2),('2024-10-01',3),('2024-10-02',4),('2024-10-03',5),('2024-10-04',6),('2024-10-05',7),('2024-10-06',1),('2024-10-07',2),('2024-10-08',3),('2024-10-09',4),('2024-10-10',5),('2024-10-11',6),('2024-10-12',7),('2024-10-13',1),('2024-10-14',2),('2024-10-15',3),('2024-10-16',4),('2024-10-17',5),('2024-10-18',6),('2024-10-19',7),('2024-10-20',1),('2024-10-21',2),('2024-10-22',3),('2024-10-23',4),('2024-10-24',5),('2024-10-25',6),('2024-10-26',7),('2024-10-27',1),('2024-10-28',2),('2024-10-29',3),('2024-10-30',4),('2024-10-31',5),('2024-11-01',6),('2024-11-02',7),('2024-11-03',1),('2024-11-04',2),('2024-11-05',3),('2024-11-06',4),('2024-11-07',5),('2024-11-08',6),('2024-11-09',7),('2024-11-10',1),('2024-11-11',2),('2024-11-12',3),('2024-11-13',4),('2024-11-14',5),('2024-11-15',6),('2024-11-16',7),('2024-11-17',1),('2024-11-18',2),('2024-11-19',3),('2024-11-20',4),('2024-11-21',5),('2024-11-22',6),('2024-11-23',7),('2024-11-24',1),('2024-11-25',2),('2024-11-26',3),('2024-11-27',4),('2024-11-28',5),('2024-11-29',6),('2024-11-30',7),('2024-12-01',1),('2024-12-02',2),('2024-12-03',3),('2024-12-04',4),('2024-12-05',5),('2024-12-06',6),('2024-12-07',7),('2024-12-08',1),('2024-12-09',2),('2024-12-10',3),('2024-12-11',4),('2024-12-12',5),('2024-12-13',6),('2024-12-14',7),('2024-12-15',1),('2024-12-16',2),('2024-12-17',3),('2024-12-18',4),('2024-12-19',5),('2024-12-20',6),('2024-12-21',7),('2024-12-22',1),('2024-12-23',2),('2024-12-24',3),('2024-12-25',4),('2024-12-26',5),('2024-12-27',6),('2024-12-28',7),('2024-12-29',1),('2024-12-30',2),('2024-12-31',3),('2025-01-01',4),('2025-01-02',5),('2025-01-03',6),('2025-01-04',7),('2025-01-05',1),('2025-01-06',2),('2025-01-07',3),('2025-01-08',4),('2025-01-09',5),('2025-01-10',6),('2025-01-11',7),('2025-01-12',1),('2025-01-13',2),('2025-01-14',3),('2025-01-15',4),('2025-01-16',5),('2025-01-17',6),('2025-01-18',7),('2025-01-19',1),('2025-01-20',2),('2025-01-21',3),('2025-01-22',4),('2025-01-23',5),('2025-01-24',6),('2025-01-25',7),('2025-01-26',1),('2025-01-27',2),('2025-01-28',3),('2025-01-29',4),('2025-01-30',5),('2025-01-31',6),('2025-02-01',7),('2025-02-02',1),('2025-02-03',2),('2025-02-04',3),('2025-02-05',4),('2025-02-06',5),('2025-02-07',6),('2025-02-08',7),('2025-02-09',1),('2025-02-10',2),('2025-02-11',3),('2025-02-12',4),('2025-02-13',5),('2025-02-14',6),('2025-02-15',7),('2025-02-16',1),('2025-02-17',2),('2025-02-18',3),('2025-02-19',4),('2025-02-20',5),('2025-02-21',6),('2025-02-22',7),('2025-02-23',1),('2025-02-24',2),('2025-02-25',3),('2025-02-26',4),('2025-02-27',5),('2025-02-28',6),('2025-03-01',7),('2025-03-02',1),('2025-03-03',2),('2025-03-04',3),('2025-03-05',4),('2025-03-06',5),('2025-03-07',6),('2025-03-08',7),('2025-03-09',1),('2025-03-10',2),('2025-03-11',3),('2025-03-12',4),('2025-03-13',5),('2025-03-14',6),('2025-03-15',7),('2025-03-16',1),('2025-03-17',2),('2025-03-18',3),('2025-03-19',4),('2025-03-20',5),('2025-03-21',6),('2025-03-22',7),('2025-03-23',1),('2025-03-24',2),('2025-03-25',3),('2025-03-26',4),('2025-03-27',5),('2025-03-28',6),('2025-03-29',7),('2025-03-30',1),('2025-03-31',2),('2025-04-01',3),('2025-04-02',4),('2025-04-03',5),('2025-04-04',6),('2025-04-05',7),('2025-04-06',1),('2025-04-07',2),('2025-04-08',3),('2025-04-09',4),('2025-04-10',5),('2025-04-11',6),('2025-04-12',7),('2025-04-13',1),('2025-04-14',2),('2025-04-15',3),('2025-04-16',4),('2025-04-17',5),('2025-04-18',6),('2025-04-19',7),('2025-04-20',1),('2025-04-21',2),('2025-04-22',3),('2025-04-23',4),('2025-04-24',5),('2025-04-25',6),('2025-04-26',7),('2025-04-27',1),('2025-04-28',2),('2025-04-29',3),('2025-04-30',4),('2025-05-01',5),('2025-05-02',6),('2025-05-03',7),('2025-05-04',1),('2025-05-05',2),('2025-05-06',3),('2025-05-07',4),('2025-05-08',5),('2025-05-09',6),('2025-05-10',7),('2025-05-11',1),('2025-05-12',2),('2025-05-13',3),('2025-05-14',4),('2025-05-15',5),('2025-05-16',6),('2025-05-17',7),('2025-05-18',1),('2025-05-19',2),('2025-05-20',3),('2025-05-21',4),('2025-05-22',5),('2025-05-23',6),('2025-05-24',7),('2025-05-25',1),('2025-05-26',2),('2025-05-27',3),('2025-05-28',4),('2025-05-29',5),('2025-05-30',6),('2025-05-31',7),('2025-06-01',1),('2025-06-02',2),('2025-06-03',3),('2025-06-04',4),('2025-06-05',5),('2025-06-06',6),('2025-06-07',7),('2025-06-08',1),('2025-06-09',2),('2025-06-10',3),('2025-06-11',4),('2025-06-12',5),('2025-06-13',6),('2025-06-14',7),('2025-06-15',1),('2025-06-16',2),('2025-06-17',3),('2025-06-18',4),('2025-06-19',5),('2025-06-20',6),('2025-06-21',7),('2025-06-22',1),('2025-06-23',2),('2025-06-24',3),('2025-06-25',4),('2025-06-26',5),('2025-06-27',6),('2025-06-28',7),('2025-06-29',1),('2025-06-30',2),('2025-07-01',3),('2025-07-02',4),('2025-07-03',5),('2025-07-04',6),('2025-07-05',7),('2025-07-06',1),('2025-07-07',2),('2025-07-08',3),('2025-07-09',4),('2025-07-10',5),('2025-07-11',6),('2025-07-12',7),('2025-07-13',1),('2025-07-14',2),('2025-07-15',3),('2025-07-16',4),('2025-07-17',5),('2025-07-18',6),('2025-07-19',7),('2025-07-20',1),('2025-07-21',2),('2025-07-22',3),('2025-07-23',4),('2025-07-24',5),('2025-07-25',6),('2025-07-26',7),('2025-07-27',1),('2025-07-28',2),('2025-07-29',3),('2025-07-30',4),('2025-07-31',5),('2025-08-01',6),('2025-08-02',7),('2025-08-03',1),('2025-08-04',2),('2025-08-05',3),('2025-08-06',4),('2025-08-07',5),('2025-08-08',6),('2025-08-09',7),('2025-08-10',1),('2025-08-11',2),('2025-08-12',3),('2025-08-13',4),('2025-08-14',5),('2025-08-15',6),('2025-08-16',7),('2025-08-17',1),('2025-08-18',2),('2025-08-19',3),('2025-08-20',4),('2025-08-21',5),('2025-08-22',6),('2025-08-23',7),('2025-08-24',1),('2025-08-25',2),('2025-08-26',3),('2025-08-27',4),('2025-08-28',5),('2025-08-29',6),('2025-08-30',7),('2025-08-31',1),('2025-09-01',2),('2025-09-02',3),('2025-09-03',4),('2025-09-04',5),('2025-09-05',6),('2025-09-06',7),('2025-09-07',1),('2025-09-08',2),('2025-09-09',3),('2025-09-10',4),('2025-09-11',5),('2025-09-12',6),('2025-09-13',7),('2025-09-14',1),('2025-09-15',2),('2025-09-16',3),('2025-09-17',4),('2025-09-18',5),('2025-09-19',6),('2025-09-20',7),('2025-09-21',1),('2025-09-22',2),('2025-09-23',3),('2025-09-24',4),('2025-09-25',5),('2025-09-26',6),('2025-09-27',7),('2025-09-28',1),('2025-09-29',2),('2025-09-30',3),('2025-10-01',4),('2025-10-02',5),('2025-10-03',6),('2025-10-04',7),('2025-10-05',1),('2025-10-06',2),('2025-10-07',3),('2025-10-08',4),('2025-10-09',5),('2025-10-10',6),('2025-10-11',7),('2025-10-12',1),('2025-10-13',2),('2025-10-14',3),('2025-10-15',4),('2025-10-16',5),('2025-10-17',6),('2025-10-18',7),('2025-10-19',1),('2025-10-20',2),('2025-10-21',3),('2025-10-22',4),('2025-10-23',5),('2025-10-24',6),('2025-10-25',7),('2025-10-26',1),('2025-10-27',2),('2025-10-28',3),('2025-10-29',4),('2025-10-30',5),('2025-10-31',6),('2025-11-01',7),('2025-11-02',1),('2025-11-03',2),('2025-11-04',3),('2025-11-05',4),('2025-11-06',5),('2025-11-07',6),('2025-11-08',7),('2025-11-09',1),('2025-11-10',2),('2025-11-11',3),('2025-11-12',4),('2025-11-13',5),('2025-11-14',6),('2025-11-15',7),('2025-11-16',1),('2025-11-17',2),('2025-11-18',3),('2025-11-19',4),('2025-11-20',5),('2025-11-21',6),('2025-11-22',7),('2025-11-23',1),('2025-11-24',2),('2025-11-25',3),('2025-11-26',4),('2025-11-27',5),('2025-11-28',6),('2025-11-29',7),('2025-11-30',1),('2025-12-01',2),('2025-12-02',3),('2025-12-03',4),('2025-12-04',5),('2025-12-05',6),('2025-12-06',7),('2025-12-07',1),('2025-12-08',2),('2025-12-09',3),('2025-12-10',4),('2025-12-11',5),('2025-12-12',6),('2025-12-13',7),('2025-12-14',1),('2025-12-15',2),('2025-12-16',3),('2025-12-17',4),('2025-12-18',5),('2025-12-19',6),('2025-12-20',7),('2025-12-21',1),('2025-12-22',2),('2025-12-23',3),('2025-12-24',4),('2025-12-25',5),('2025-12-26',6),('2025-12-27',7),('2025-12-28',1),('2025-12-29',2),('2025-12-30',3),('2025-12-31',4),('2026-01-01',5),('2026-01-02',6),('2026-01-03',7),('2026-01-04',1),('2026-01-05',2),('2026-01-06',3),('2026-01-07',4),('2026-01-08',5),('2026-01-09',6),('2026-01-10',7),('2026-01-11',1),('2026-01-12',2),('2026-01-13',3),('2026-01-14',4),('2026-01-15',5),('2026-01-16',6),('2026-01-17',7),('2026-01-18',1),('2026-01-19',2),('2026-01-20',3),('2026-01-21',4),('2026-01-22',5),('2026-01-23',6),('2026-01-24',7),('2026-01-25',1),('2026-01-26',2),('2026-01-27',3),('2026-01-28',4),('2026-01-29',5),('2026-01-30',6),('2026-01-31',7),('2026-02-01',1),('2026-02-02',2),('2026-02-03',3),('2026-02-04',4),('2026-02-05',5),('2026-02-06',6),('2026-02-07',7),('2026-02-08',1),('2026-02-09',2),('2026-02-10',3),('2026-02-11',4),('2026-02-12',5),('2026-02-13',6),('2026-02-14',7),('2026-02-15',1),('2026-02-16',2),('2026-02-17',3),('2026-02-18',4),('2026-02-19',5),('2026-02-20',6),('2026-02-21',7),('2026-02-22',1),('2026-02-23',2),('2026-02-24',3),('2026-02-25',4),('2026-02-26',5),('2026-02-27',6),('2026-02-28',7),('2026-03-01',1),('2026-03-02',2),('2026-03-03',3),('2026-03-04',4),('2026-03-05',5),('2026-03-06',6),('2026-03-07',7),('2026-03-08',1),('2026-03-09',2),('2026-03-10',3),('2026-03-11',4),('2026-03-12',5),('2026-03-13',6),('2026-03-14',7),('2026-03-15',1),('2026-03-16',2),('2026-03-17',3),('2026-03-18',4),('2026-03-19',5),('2026-03-20',6),('2026-03-21',7),('2026-03-22',1),('2026-03-23',2),('2026-03-24',3),('2026-03-25',4),('2026-03-26',5),('2026-03-27',6),('2026-03-28',7),('2026-03-29',1),('2026-03-30',2),('2026-03-31',3),('2026-04-01',4),('2026-04-02',5),('2026-04-03',6),('2026-04-04',7),('2026-04-05',1),('2026-04-06',2),('2026-04-07',3),('2026-04-08',4),('2026-04-09',5),('2026-04-10',6),('2026-04-11',7),('2026-04-12',1),('2026-04-13',2),('2026-04-14',3),('2026-04-15',4),('2026-04-16',5),('2026-04-17',6),('2026-04-18',7),('2026-04-19',1),('2026-04-20',2),('2026-04-21',3),('2026-04-22',4),('2026-04-23',5),('2026-04-24',6),('2026-04-25',7),('2026-04-26',1),('2026-04-27',2),('2026-04-28',3),('2026-04-29',4),('2026-04-30',5),('2026-05-01',6),('2026-05-02',7),('2026-05-03',1),('2026-05-04',2),('2026-05-05',3),('2026-05-06',4),('2026-05-07',5),('2026-05-08',6),('2026-05-09',7),('2026-05-10',1),('2026-05-11',2),('2026-05-12',3),('2026-05-13',4),('2026-05-14',5),('2026-05-15',6),('2026-05-16',7),('2026-05-17',1),('2026-05-18',2),('2026-05-19',3),('2026-05-20',4),('2026-05-21',5),('2026-05-22',6),('2026-05-23',7),('2026-05-24',1),('2026-05-25',2),('2026-05-26',3),('2026-05-27',4),('2026-05-28',5),('2026-05-29',6),('2026-05-30',7),('2026-05-31',1),('2026-06-01',2),('2026-06-02',3),('2026-06-03',4),('2026-06-04',5),('2026-06-05',6),('2026-06-06',7),('2026-06-07',1),('2026-06-08',2),('2026-06-09',3),('2026-06-10',4),('2026-06-11',5),('2026-06-12',6),('2026-06-13',7),('2026-06-14',1),('2026-06-15',2),('2026-06-16',3),('2026-06-17',4),('2026-06-18',5),('2026-06-19',6),('2026-06-20',7),('2026-06-21',1),('2026-06-22',2),('2026-06-23',3),('2026-06-24',4),('2026-06-25',5),('2026-06-26',6),('2026-06-27',7),('2026-06-28',1),('2026-06-29',2),('2026-06-30',3),('2026-07-01',4),('2026-07-02',5),('2026-07-03',6),('2026-07-04',7),('2026-07-05',1),('2026-07-06',2),('2026-07-07',3),('2026-07-08',4),('2026-07-09',5),('2026-07-10',6),('2026-07-11',7),('2026-07-12',1),('2026-07-13',2),('2026-07-14',3),('2026-07-15',4),('2026-07-16',5),('2026-07-17',6),('2026-07-18',7),('2026-07-19',1),('2026-07-20',2),('2026-07-21',3),('2026-07-22',4),('2026-07-23',5),('2026-07-24',6),('2026-07-25',7),('2026-07-26',1),('2026-07-27',2),('2026-07-28',3),('2026-07-29',4),('2026-07-30',5),('2026-07-31',6),('2026-08-01',7),('2026-08-02',1),('2026-08-03',2),('2026-08-04',3),('2026-08-05',4),('2026-08-06',5),('2026-08-07',6),('2026-08-08',7),('2026-08-09',1),('2026-08-10',2),('2026-08-11',3),('2026-08-12',4),('2026-08-13',5),('2026-08-14',6),('2026-08-15',7),('2026-08-16',1),('2026-08-17',2),('2026-08-18',3),('2026-08-19',4),('2026-08-20',5),('2026-08-21',6),('2026-08-22',7),('2026-08-23',1),('2026-08-24',2),('2026-08-25',3),('2026-08-26',4),('2026-08-27',5),('2026-08-28',6),('2026-08-29',7),('2026-08-30',1),('2026-08-31',2),('2026-09-01',3),('2026-09-02',4),('2026-09-03',5),('2026-09-04',6),('2026-09-05',7),('2026-09-06',1),('2026-09-07',2),('2026-09-08',3),('2026-09-09',4),('2026-09-10',5),('2026-09-11',6),('2026-09-12',7),('2026-09-13',1),('2026-09-14',2),('2026-09-15',3),('2026-09-16',4),('2026-09-17',5),('2026-09-18',6),('2026-09-19',7),('2026-09-20',1),('2026-09-21',2),('2026-09-22',3),('2026-09-23',4),('2026-09-24',5),('2026-09-25',6),('2026-09-26',7),('2026-09-27',1),('2026-09-28',2),('2026-09-29',3),('2026-09-30',4),('2026-10-01',5),('2026-10-02',6),('2026-10-03',7),('2026-10-04',1),('2026-10-05',2),('2026-10-06',3),('2026-10-07',4),('2026-10-08',5),('2026-10-09',6),('2026-10-10',7),('2026-10-11',1),('2026-10-12',2),('2026-10-13',3),('2026-10-14',4),('2026-10-15',5),('2026-10-16',6),('2026-10-17',7),('2026-10-18',1),('2026-10-19',2),('2026-10-20',3),('2026-10-21',4),('2026-10-22',5),('2026-10-23',6),('2026-10-24',7),('2026-10-25',1),('2026-10-26',2),('2026-10-27',3),('2026-10-28',4),('2026-10-29',5),('2026-10-30',6),('2026-10-31',7),('2026-11-01',1),('2026-11-02',2),('2026-11-03',3),('2026-11-04',4),('2026-11-05',5),('2026-11-06',6),('2026-11-07',7),('2026-11-08',1),('2026-11-09',2),('2026-11-10',3),('2026-11-11',4),('2026-11-12',5),('2026-11-13',6),('2026-11-14',7),('2026-11-15',1),('2026-11-16',2),('2026-11-17',3),('2026-11-18',4),('2026-11-19',5),('2026-11-20',6),('2026-11-21',7),('2026-11-22',1),('2026-11-23',2),('2026-11-24',3),('2026-11-25',4),('2026-11-26',5),('2026-11-27',6),('2026-11-28',7),('2026-11-29',1),('2026-11-30',2),('2026-12-01',3),('2026-12-02',4),('2026-12-03',5),('2026-12-04',6),('2026-12-05',7),('2026-12-06',1),('2026-12-07',2),('2026-12-08',3),('2026-12-09',4),('2026-12-10',5),('2026-12-11',6),('2026-12-12',7),('2026-12-13',1),('2026-12-14',2),('2026-12-15',3),('2026-12-16',4),('2026-12-17',5),('2026-12-18',6),('2026-12-19',7),('2026-12-20',1),('2026-12-21',2),('2026-12-22',3),('2026-12-23',4),('2026-12-24',5),('2026-12-25',6),('2026-12-26',7),('2026-12-27',1),('2026-12-28',2),('2026-12-29',3),('2026-12-30',4),('2026-12-31',5),('2027-01-01',6),('2027-01-02',7),('2027-01-03',1),('2027-01-04',2),('2027-01-05',3),('2027-01-06',4),('2027-01-07',5),('2027-01-08',6),('2027-01-09',7),('2027-01-10',1),('2027-01-11',2),('2027-01-12',3),('2027-01-13',4),('2027-01-14',5),('2027-01-15',6),('2027-01-16',7),('2027-01-17',1),('2027-01-18',2),('2027-01-19',3),('2027-01-20',4),('2027-01-21',5),('2027-01-22',6),('2027-01-23',7),('2027-01-24',1),('2027-01-25',2),('2027-01-26',3),('2027-01-27',4),('2027-01-28',5),('2027-01-29',6),('2027-01-30',7),('2027-01-31',1),('2027-02-01',2),('2027-02-02',3),('2027-02-03',4),('2027-02-04',5),('2027-02-05',6),('2027-02-06',7),('2027-02-07',1),('2027-02-08',2),('2027-02-09',3),('2027-02-10',4),('2027-02-11',5),('2027-02-12',6),('2027-02-13',7),('2027-02-14',1),('2027-02-15',2),('2027-02-16',3),('2027-02-17',4),('2027-02-18',5),('2027-02-19',6),('2027-02-20',7),('2027-02-21',1),('2027-02-22',2),('2027-02-23',3),('2027-02-24',4),('2027-02-25',5),('2027-02-26',6),('2027-02-27',7),('2027-02-28',1),('2027-03-01',2),('2027-03-02',3),('2027-03-03',4),('2027-03-04',5),('2027-03-05',6),('2027-03-06',7),('2027-03-07',1),('2027-03-08',2),('2027-03-09',3),('2027-03-10',4),('2027-03-11',5),('2027-03-12',6),('2027-03-13',7),('2027-03-14',1),('2027-03-15',2),('2027-03-16',3),('2027-03-17',4),('2027-03-18',5),('2027-03-19',6),('2027-03-20',7),('2027-03-21',1),('2027-03-22',2),('2027-03-23',3),('2027-03-24',4),('2027-03-25',5),('2027-03-26',6),('2027-03-27',7),('2027-03-28',1),('2027-03-29',2),('2027-03-30',3),('2027-03-31',4),('2027-04-01',5),('2027-04-02',6),('2027-04-03',7),('2027-04-04',1),('2027-04-05',2),('2027-04-06',3),('2027-04-07',4),('2027-04-08',5),('2027-04-09',6),('2027-04-10',7),('2027-04-11',1),('2027-04-12',2),('2027-04-13',3),('2027-04-14',4),('2027-04-15',5),('2027-04-16',6),('2027-04-17',7),('2027-04-18',1),('2027-04-19',2),('2027-04-20',3),('2027-04-21',4),('2027-04-22',5),('2027-04-23',6),('2027-04-24',7),('2027-04-25',1),('2027-04-26',2),('2027-04-27',3),('2027-04-28',4),('2027-04-29',5),('2027-04-30',6),('2027-05-01',7),('2027-05-02',1),('2027-05-03',2),('2027-05-04',3),('2027-05-05',4),('2027-05-06',5),('2027-05-07',6),('2027-05-08',7),('2027-05-09',1),('2027-05-10',2),('2027-05-11',3),('2027-05-12',4),('2027-05-13',5),('2027-05-14',6),('2027-05-15',7),('2027-05-16',1),('2027-05-17',2),('2027-05-18',3),('2027-05-19',4),('2027-05-20',5),('2027-05-21',6),('2027-05-22',7),('2027-05-23',1),('2027-05-24',2),('2027-05-25',3),('2027-05-26',4),('2027-05-27',5),('2027-05-28',6),('2027-05-29',7),('2027-05-30',1),('2027-05-31',2),('2027-06-01',3),('2027-06-02',4),('2027-06-03',5),('2027-06-04',6),('2027-06-05',7),('2027-06-06',1),('2027-06-07',2),('2027-06-08',3),('2027-06-09',4),('2027-06-10',5),('2027-06-11',6),('2027-06-12',7),('2027-06-13',1),('2027-06-14',2),('2027-06-15',3),('2027-06-16',4),('2027-06-17',5),('2027-06-18',6),('2027-06-19',7),('2027-06-20',1),('2027-06-21',2),('2027-06-22',3),('2027-06-23',4),('2027-06-24',5),('2027-06-25',6),('2027-06-26',7),('2027-06-27',1),('2027-06-28',2),('2027-06-29',3),('2027-06-30',4),('2027-07-01',5),('2027-07-02',6),('2027-07-03',7),('2027-07-04',1),('2027-07-05',2),('2027-07-06',3),('2027-07-07',4),('2027-07-08',5),('2027-07-09',6),('2027-07-10',7),('2027-07-11',1),('2027-07-12',2),('2027-07-13',3),('2027-07-14',4),('2027-07-15',5),('2027-07-16',6),('2027-07-17',7),('2027-07-18',1),('2027-07-19',2),('2027-07-20',3),('2027-07-21',4),('2027-07-22',5),('2027-07-23',6),('2027-07-24',7),('2027-07-25',1),('2027-07-26',2),('2027-07-27',3),('2027-07-28',4),('2027-07-29',5),('2027-07-30',6),('2027-07-31',7),('2027-08-01',1),('2027-08-02',2),('2027-08-03',3),('2027-08-04',4),('2027-08-05',5),('2027-08-06',6),('2027-08-07',7),('2027-08-08',1),('2027-08-09',2),('2027-08-10',3),('2027-08-11',4),('2027-08-12',5),('2027-08-13',6),('2027-08-14',7),('2027-08-15',1),('2027-08-16',2),('2027-08-17',3),('2027-08-18',4),('2027-08-19',5),('2027-08-20',6),('2027-08-21',7),('2027-08-22',1),('2027-08-23',2),('2027-08-24',3),('2027-08-25',4),('2027-08-26',5),('2027-08-27',6),('2027-08-28',7),('2027-08-29',1),('2027-08-30',2),('2027-08-31',3),('2027-09-01',4),('2027-09-02',5),('2027-09-03',6),('2027-09-04',7),('2027-09-05',1),('2027-09-06',2),('2027-09-07',3),('2027-09-08',4),('2027-09-09',5),('2027-09-10',6),('2027-09-11',7),('2027-09-12',1),('2027-09-13',2),('2027-09-14',3),('2027-09-15',4),('2027-09-16',5),('2027-09-17',6),('2027-09-18',7),('2027-09-19',1),('2027-09-20',2),('2027-09-21',3),('2027-09-22',4),('2027-09-23',5),('2027-09-24',6),('2027-09-25',7),('2027-09-26',1),('2027-09-27',2),('2027-09-28',3),('2027-09-29',4),('2027-09-30',5),('2027-10-01',6),('2027-10-02',7),('2027-10-03',1),('2027-10-04',2),('2027-10-05',3),('2027-10-06',4),('2027-10-07',5),('2027-10-08',6),('2027-10-09',7),('2027-10-10',1),('2027-10-11',2),('2027-10-12',3),('2027-10-13',4),('2027-10-14',5),('2027-10-15',6),('2027-10-16',7),('2027-10-17',1),('2027-10-18',2),('2027-10-19',3),('2027-10-20',4),('2027-10-21',5),('2027-10-22',6),('2027-10-23',7),('2027-10-24',1),('2027-10-25',2),('2027-10-26',3),('2027-10-27',4),('2027-10-28',5),('2027-10-29',6),('2027-10-30',7),('2027-10-31',1),('2027-11-01',2),('2027-11-02',3),('2027-11-03',4),('2027-11-04',5),('2027-11-05',6),('2027-11-06',7),('2027-11-07',1),('2027-11-08',2),('2027-11-09',3),('2027-11-10',4),('2027-11-11',5),('2027-11-12',6),('2027-11-13',7),('2027-11-14',1),('2027-11-15',2),('2027-11-16',3),('2027-11-17',4),('2027-11-18',5),('2027-11-19',6),('2027-11-20',7),('2027-11-21',1),('2027-11-22',2),('2027-11-23',3),('2027-11-24',4),('2027-11-25',5),('2027-11-26',6),('2027-11-27',7),('2027-11-28',1),('2027-11-29',2),('2027-11-30',3),('2027-12-01',4),('2027-12-02',5),('2027-12-03',6),('2027-12-04',7),('2027-12-05',1),('2027-12-06',2),('2027-12-07',3),('2027-12-08',4),('2027-12-09',5),('2027-12-10',6),('2027-12-11',7),('2027-12-12',1),('2027-12-13',2),('2027-12-14',3),('2027-12-15',4),('2027-12-16',5),('2027-12-17',6),('2027-12-18',7),('2027-12-19',1),('2027-12-20',2),('2027-12-21',3),('2027-12-22',4),('2027-12-23',5),('2027-12-24',6),('2027-12-25',7),('2027-12-26',1),('2027-12-27',2),('2027-12-28',3),('2027-12-29',4),('2027-12-30',5),('2027-12-31',6),('2028-01-01',7),('2028-01-02',1),('2028-01-03',2),('2028-01-04',3),('2028-01-05',4),('2028-01-06',5),('2028-01-07',6),('2028-01-08',7),('2028-01-09',1),('2028-01-10',2),('2028-01-11',3),('2028-01-12',4),('2028-01-13',5),('2028-01-14',6),('2028-01-15',7),('2028-01-16',1),('2028-01-17',2),('2028-01-18',3),('2028-01-19',4),('2028-01-20',5),('2028-01-21',6),('2028-01-22',7),('2028-01-23',1),('2028-01-24',2),('2028-01-25',3),('2028-01-26',4),('2028-01-27',5),('2028-01-28',6),('2028-01-29',7),('2028-01-30',1),('2028-01-31',2),('2028-02-01',3),('2028-02-02',4),('2028-02-03',5),('2028-02-04',6),('2028-02-05',7),('2028-02-06',1),('2028-02-07',2),('2028-02-08',3),('2028-02-09',4),('2028-02-10',5),('2028-02-11',6),('2028-02-12',7),('2028-02-13',1),('2028-02-14',2),('2028-02-15',3),('2028-02-16',4),('2028-02-17',5),('2028-02-18',6),('2028-02-19',7),('2028-02-20',1),('2028-02-21',2),('2028-02-22',3),('2028-02-23',4),('2028-02-24',5),('2028-02-25',6),('2028-02-26',7),('2028-02-27',1),('2028-02-28',2),('2028-02-29',3),('2028-03-01',4),('2028-03-02',5),('2028-03-03',6),('2028-03-04',7),('2028-03-05',1),('2028-03-06',2),('2028-03-07',3),('2028-03-08',4),('2028-03-09',5),('2028-03-10',6),('2028-03-11',7),('2028-03-12',1),('2028-03-13',2),('2028-03-14',3),('2028-03-15',4),('2028-03-16',5),('2028-03-17',6),('2028-03-18',7),('2028-03-19',1),('2028-03-20',2),('2028-03-21',3),('2028-03-22',4),('2028-03-23',5),('2028-03-24',6),('2028-03-25',7),('2028-03-26',1),('2028-03-27',2),('2028-03-28',3),('2028-03-29',4),('2028-03-30',5),('2028-03-31',6),('2028-04-01',7),('2028-04-02',1),('2028-04-03',2),('2028-04-04',3),('2028-04-05',4),('2028-04-06',5),('2028-04-07',6),('2028-04-08',7),('2028-04-09',1),('2028-04-10',2),('2028-04-11',3),('2028-04-12',4),('2028-04-13',5),('2028-04-14',6),('2028-04-15',7),('2028-04-16',1),('2028-04-17',2),('2028-04-18',3),('2028-04-19',4),('2028-04-20',5),('2028-04-21',6),('2028-04-22',7),('2028-04-23',1),('2028-04-24',2),('2028-04-25',3),('2028-04-26',4),('2028-04-27',5),('2028-04-28',6),('2028-04-29',7),('2028-04-30',1),('2028-05-01',2),('2028-05-02',3),('2028-05-03',4),('2028-05-04',5),('2028-05-05',6),('2028-05-06',7),('2028-05-07',1),('2028-05-08',2),('2028-05-09',3),('2028-05-10',4),('2028-05-11',5),('2028-05-12',6),('2028-05-13',7),('2028-05-14',1),('2028-05-15',2),('2028-05-16',3),('2028-05-17',4),('2028-05-18',5),('2028-05-19',6),('2028-05-20',7),('2028-05-21',1),('2028-05-22',2),('2028-05-23',3),('2028-05-24',4),('2028-05-25',5),('2028-05-26',6),('2028-05-27',7),('2028-05-28',1),('2028-05-29',2),('2028-05-30',3),('2028-05-31',4),('2028-06-01',5),('2028-06-02',6),('2028-06-03',7),('2028-06-04',1),('2028-06-05',2),('2028-06-06',3),('2028-06-07',4),('2028-06-08',5),('2028-06-09',6),('2028-06-10',7),('2028-06-11',1),('2028-06-12',2),('2028-06-13',3),('2028-06-14',4),('2028-06-15',5),('2028-06-16',6),('2028-06-17',7),('2028-06-18',1),('2028-06-19',2),('2028-06-20',3),('2028-06-21',4),('2028-06-22',5),('2028-06-23',6),('2028-06-24',7),('2028-06-25',1),('2028-06-26',2),('2028-06-27',3),('2028-06-28',4),('2028-06-29',5),('2028-06-30',6),('2028-07-01',7),('2028-07-02',1),('2028-07-03',2),('2028-07-04',3),('2028-07-05',4),('2028-07-06',5),('2028-07-07',6),('2028-07-08',7),('2028-07-09',1),('2028-07-10',2),('2028-07-11',3),('2028-07-12',4),('2028-07-13',5),('2028-07-14',6),('2028-07-15',7),('2028-07-16',1),('2028-07-17',2),('2028-07-18',3),('2028-07-19',4),('2028-07-20',5),('2028-07-21',6),('2028-07-22',7),('2028-07-23',1),('2028-07-24',2),('2028-07-25',3),('2028-07-26',4),('2028-07-27',5),('2028-07-28',6),('2028-07-29',7),('2028-07-30',1),('2028-07-31',2),('2028-08-01',3),('2028-08-02',4),('2028-08-03',5),('2028-08-04',6),('2028-08-05',7),('2028-08-06',1),('2028-08-07',2),('2028-08-08',3),('2028-08-09',4),('2028-08-10',5),('2028-08-11',6),('2028-08-12',7),('2028-08-13',1),('2028-08-14',2),('2028-08-15',3),('2028-08-16',4),('2028-08-17',5),('2028-08-18',6),('2028-08-19',7),('2028-08-20',1),('2028-08-21',2),('2028-08-22',3),('2028-08-23',4),('2028-08-24',5),('2028-08-25',6),('2028-08-26',7),('2028-08-27',1),('2028-08-28',2),('2028-08-29',3),('2028-08-30',4),('2028-08-31',5),('2028-09-01',6),('2028-09-02',7),('2028-09-03',1),('2028-09-04',2),('2028-09-05',3),('2028-09-06',4),('2028-09-07',5),('2028-09-08',6),('2028-09-09',7),('2028-09-10',1),('2028-09-11',2),('2028-09-12',3),('2028-09-13',4),('2028-09-14',5),('2028-09-15',6),('2028-09-16',7),('2028-09-17',1),('2028-09-18',2),('2028-09-19',3),('2028-09-20',4),('2028-09-21',5),('2028-09-22',6),('2028-09-23',7),('2028-09-24',1),('2028-09-25',2),('2028-09-26',3),('2028-09-27',4),('2028-09-28',5),('2028-09-29',6),('2028-09-30',7),('2028-10-01',1),('2028-10-02',2),('2028-10-03',3),('2028-10-04',4),('2028-10-05',5),('2028-10-06',6),('2028-10-07',7),('2028-10-08',1),('2028-10-09',2),('2028-10-10',3),('2028-10-11',4),('2028-10-12',5),('2028-10-13',6),('2028-10-14',7),('2028-10-15',1),('2028-10-16',2),('2028-10-17',3),('2028-10-18',4),('2028-10-19',5),('2028-10-20',6),('2028-10-21',7),('2028-10-22',1),('2028-10-23',2),('2028-10-24',3),('2028-10-25',4),('2028-10-26',5),('2028-10-27',6),('2028-10-28',7),('2028-10-29',1),('2028-10-30',2),('2028-10-31',3),('2028-11-01',4),('2028-11-02',5),('2028-11-03',6),('2028-11-04',7),('2028-11-05',1),('2028-11-06',2),('2028-11-07',3),('2028-11-08',4),('2028-11-09',5),('2028-11-10',6),('2028-11-11',7),('2028-11-12',1),('2028-11-13',2),('2028-11-14',3),('2028-11-15',4),('2028-11-16',5),('2028-11-17',6),('2028-11-18',7),('2028-11-19',1),('2028-11-20',2),('2028-11-21',3),('2028-11-22',4),('2028-11-23',5),('2028-11-24',6),('2028-11-25',7),('2028-11-26',1),('2028-11-27',2),('2028-11-28',3),('2028-11-29',4),('2028-11-30',5),('2028-12-01',6),('2028-12-02',7),('2028-12-03',1),('2028-12-04',2),('2028-12-05',3),('2028-12-06',4),('2028-12-07',5),('2028-12-08',6),('2028-12-09',7),('2028-12-10',1),('2028-12-11',2),('2028-12-12',3),('2028-12-13',4),('2028-12-14',5),('2028-12-15',6),('2028-12-16',7),('2028-12-17',1),('2028-12-18',2),('2028-12-19',3),('2028-12-20',4),('2028-12-21',5),('2028-12-22',6),('2028-12-23',7),('2028-12-24',1),('2028-12-25',2),('2028-12-26',3),('2028-12-27',4),('2028-12-28',5),('2028-12-29',6),('2028-12-30',7),('2028-12-31',1),('2029-01-01',2),('2029-01-02',3),('2029-01-03',4),('2029-01-04',5),('2029-01-05',6),('2029-01-06',7),('2029-01-07',1),('2029-01-08',2),('2029-01-09',3),('2029-01-10',4),('2029-01-11',5),('2029-01-12',6),('2029-01-13',7),('2029-01-14',1),('2029-01-15',2),('2029-01-16',3),('2029-01-17',4),('2029-01-18',5),('2029-01-19',6),('2029-01-20',7),('2029-01-21',1),('2029-01-22',2),('2029-01-23',3),('2029-01-24',4),('2029-01-25',5),('2029-01-26',6),('2029-01-27',7),('2029-01-28',1),('2029-01-29',2),('2029-01-30',3),('2029-01-31',4),('2029-02-01',5),('2029-02-02',6),('2029-02-03',7),('2029-02-04',1),('2029-02-05',2),('2029-02-06',3),('2029-02-07',4),('2029-02-08',5),('2029-02-09',6),('2029-02-10',7),('2029-02-11',1),('2029-02-12',2),('2029-02-13',3),('2029-02-14',4),('2029-02-15',5),('2029-02-16',6),('2029-02-17',7),('2029-02-18',1),('2029-02-19',2),('2029-02-20',3),('2029-02-21',4),('2029-02-22',5),('2029-02-23',6),('2029-02-24',7),('2029-02-25',1),('2029-02-26',2),('2029-02-27',3),('2029-02-28',4),('2029-03-01',5),('2029-03-02',6),('2029-03-03',7),('2029-03-04',1),('2029-03-05',2),('2029-03-06',3),('2029-03-07',4),('2029-03-08',5),('2029-03-09',6),('2029-03-10',7),('2029-03-11',1),('2029-03-12',2),('2029-03-13',3),('2029-03-14',4),('2029-03-15',5),('2029-03-16',6),('2029-03-17',7),('2029-03-18',1),('2029-03-19',2),('2029-03-20',3),('2029-03-21',4),('2029-03-22',5),('2029-03-23',6),('2029-03-24',7),('2029-03-25',1),('2029-03-26',2),('2029-03-27',3),('2029-03-28',4),('2029-03-29',5),('2029-03-30',6),('2029-03-31',7),('2029-04-01',1),('2029-04-02',2),('2029-04-03',3),('2029-04-04',4),('2029-04-05',5),('2029-04-06',6),('2029-04-07',7),('2029-04-08',1),('2029-04-09',2),('2029-04-10',3),('2029-04-11',4),('2029-04-12',5),('2029-04-13',6),('2029-04-14',7),('2029-04-15',1),('2029-04-16',2),('2029-04-17',3),('2029-04-18',4),('2029-04-19',5),('2029-04-20',6),('2029-04-21',7),('2029-04-22',1),('2029-04-23',2),('2029-04-24',3),('2029-04-25',4),('2029-04-26',5),('2029-04-27',6),('2029-04-28',7),('2029-04-29',1),('2029-04-30',2),('2029-05-01',3),('2029-05-02',4),('2029-05-03',5),('2029-05-04',6),('2029-05-05',7),('2029-05-06',1),('2029-05-07',2),('2029-05-08',3),('2029-05-09',4),('2029-05-10',5),('2029-05-11',6),('2029-05-12',7),('2029-05-13',1),('2029-05-14',2),('2029-05-15',3),('2029-05-16',4),('2029-05-17',5),('2029-05-18',6),('2029-05-19',7),('2029-05-20',1),('2029-05-21',2),('2029-05-22',3),('2029-05-23',4),('2029-05-24',5),('2029-05-25',6),('2029-05-26',7),('2029-05-27',1),('2029-05-28',2),('2029-05-29',3),('2029-05-30',4),('2029-05-31',5),('2029-06-01',6),('2029-06-02',7),('2029-06-03',1),('2029-06-04',2),('2029-06-05',3),('2029-06-06',4),('2029-06-07',5),('2029-06-08',6),('2029-06-09',7),('2029-06-10',1),('2029-06-11',2),('2029-06-12',3),('2029-06-13',4),('2029-06-14',5),('2029-06-15',6),('2029-06-16',7),('2029-06-17',1),('2029-06-18',2),('2029-06-19',3),('2029-06-20',4),('2029-06-21',5),('2029-06-22',6),('2029-06-23',7),('2029-06-24',1),('2029-06-25',2),('2029-06-26',3),('2029-06-27',4),('2029-06-28',5),('2029-06-29',6),('2029-06-30',7),('2029-07-01',1),('2029-07-02',2),('2029-07-03',3),('2029-07-04',4),('2029-07-05',5),('2029-07-06',6),('2029-07-07',7),('2029-07-08',1),('2029-07-09',2),('2029-07-10',3),('2029-07-11',4),('2029-07-12',5),('2029-07-13',6),('2029-07-14',7),('2029-07-15',1),('2029-07-16',2),('2029-07-17',3),('2029-07-18',4),('2029-07-19',5),('2029-07-20',6),('2029-07-21',7),('2029-07-22',1),('2029-07-23',2),('2029-07-24',3),('2029-07-25',4),('2029-07-26',5),('2029-07-27',6),('2029-07-28',7),('2029-07-29',1),('2029-07-30',2),('2029-07-31',3),('2029-08-01',4),('2029-08-02',5),('2029-08-03',6),('2029-08-04',7),('2029-08-05',1),('2029-08-06',2),('2029-08-07',3),('2029-08-08',4),('2029-08-09',5),('2029-08-10',6),('2029-08-11',7),('2029-08-12',1),('2029-08-13',2),('2029-08-14',3),('2029-08-15',4),('2029-08-16',5),('2029-08-17',6),('2029-08-18',7),('2029-08-19',1),('2029-08-20',2),('2029-08-21',3),('2029-08-22',4),('2029-08-23',5),('2029-08-24',6),('2029-08-25',7),('2029-08-26',1),('2029-08-27',2),('2029-08-28',3),('2029-08-29',4),('2029-08-30',5),('2029-08-31',6),('2029-09-01',7),('2029-09-02',1),('2029-09-03',2),('2029-09-04',3),('2029-09-05',4),('2029-09-06',5),('2029-09-07',6),('2029-09-08',7),('2029-09-09',1),('2029-09-10',2),('2029-09-11',3),('2029-09-12',4),('2029-09-13',5),('2029-09-14',6),('2029-09-15',7),('2029-09-16',1),('2029-09-17',2),('2029-09-18',3),('2029-09-19',4),('2029-09-20',5),('2029-09-21',6),('2029-09-22',7),('2029-09-23',1),('2029-09-24',2),('2029-09-25',3),('2029-09-26',4),('2029-09-27',5),('2029-09-28',6),('2029-09-29',7),('2029-09-30',1),('2029-10-01',2),('2029-10-02',3),('2029-10-03',4),('2029-10-04',5),('2029-10-05',6),('2029-10-06',7),('2029-10-07',1),('2029-10-08',2),('2029-10-09',3),('2029-10-10',4),('2029-10-11',5),('2029-10-12',6),('2029-10-13',7),('2029-10-14',1),('2029-10-15',2),('2029-10-16',3),('2029-10-17',4),('2029-10-18',5),('2029-10-19',6),('2029-10-20',7),('2029-10-21',1),('2029-10-22',2),('2029-10-23',3),('2029-10-24',4),('2029-10-25',5),('2029-10-26',6),('2029-10-27',7),('2029-10-28',1),('2029-10-29',2),('2029-10-30',3),('2029-10-31',4),('2029-11-01',5),('2029-11-02',6),('2029-11-03',7),('2029-11-04',1),('2029-11-05',2),('2029-11-06',3),('2029-11-07',4),('2029-11-08',5),('2029-11-09',6),('2029-11-10',7),('2029-11-11',1),('2029-11-12',2),('2029-11-13',3),('2029-11-14',4),('2029-11-15',5),('2029-11-16',6),('2029-11-17',7),('2029-11-18',1),('2029-11-19',2),('2029-11-20',3),('2029-11-21',4),('2029-11-22',5),('2029-11-23',6),('2029-11-24',7),('2029-11-25',1),('2029-11-26',2),('2029-11-27',3),('2029-11-28',4),('2029-11-29',5),('2029-11-30',6),('2029-12-01',7),('2029-12-02',1),('2029-12-03',2),('2029-12-04',3),('2029-12-05',4),('2029-12-06',5),('2029-12-07',6),('2029-12-08',7),('2029-12-09',1),('2029-12-10',2),('2029-12-11',3),('2029-12-12',4),('2029-12-13',5),('2029-12-14',6),('2029-12-15',7),('2029-12-16',1),('2029-12-17',2),('2029-12-18',3),('2029-12-19',4),('2029-12-20',5),('2029-12-21',6),('2029-12-22',7),('2029-12-23',1),('2029-12-24',2),('2029-12-25',3),('2029-12-26',4),('2029-12-27',5),('2029-12-28',6),('2029-12-29',7),('2029-12-30',1),('2029-12-31',2),('2030-01-01',3),('2030-01-02',4),('2030-01-03',5),('2030-01-04',6),('2030-01-05',7),('2030-01-06',1),('2030-01-07',2),('2030-01-08',3),('2030-01-09',4),('2030-01-10',5),('2030-01-11',6),('2030-01-12',7),('2030-01-13',1),('2030-01-14',2),('2030-01-15',3),('2030-01-16',4),('2030-01-17',5),('2030-01-18',6),('2030-01-19',7),('2030-01-20',1),('2030-01-21',2),('2030-01-22',3),('2030-01-23',4),('2030-01-24',5),('2030-01-25',6),('2030-01-26',7),('2030-01-27',1),('2030-01-28',2),('2030-01-29',3),('2030-01-30',4),('2030-01-31',5),('2030-02-01',6),('2030-02-02',7),('2030-02-03',1),('2030-02-04',2),('2030-02-05',3),('2030-02-06',4),('2030-02-07',5),('2030-02-08',6),('2030-02-09',7),('2030-02-10',1),('2030-02-11',2),('2030-02-12',3),('2030-02-13',4),('2030-02-14',5),('2030-02-15',6),('2030-02-16',7),('2030-02-17',1),('2030-02-18',2),('2030-02-19',3),('2030-02-20',4),('2030-02-21',5),('2030-02-22',6),('2030-02-23',7),('2030-02-24',1),('2030-02-25',2),('2030-02-26',3),('2030-02-27',4),('2030-02-28',5),('2030-03-01',6),('2030-03-02',7),('2030-03-03',1),('2030-03-04',2),('2030-03-05',3),('2030-03-06',4),('2030-03-07',5),('2030-03-08',6),('2030-03-09',7),('2030-03-10',1),('2030-03-11',2),('2030-03-12',3),('2030-03-13',4),('2030-03-14',5),('2030-03-15',6),('2030-03-16',7),('2030-03-17',1),('2030-03-18',2),('2030-03-19',3),('2030-03-20',4),('2030-03-21',5),('2030-03-22',6),('2030-03-23',7),('2030-03-24',1),('2030-03-25',2),('2030-03-26',3),('2030-03-27',4),('2030-03-28',5),('2030-03-29',6),('2030-03-30',7),('2030-03-31',1),('2030-04-01',2),('2030-04-02',3),('2030-04-03',4),('2030-04-04',5),('2030-04-05',6),('2030-04-06',7),('2030-04-07',1),('2030-04-08',2),('2030-04-09',3),('2030-04-10',4),('2030-04-11',5),('2030-04-12',6),('2030-04-13',7),('2030-04-14',1),('2030-04-15',2),('2030-04-16',3),('2030-04-17',4),('2030-04-18',5),('2030-04-19',6),('2030-04-20',7),('2030-04-21',1),('2030-04-22',2),('2030-04-23',3),('2030-04-24',4),('2030-04-25',5),('2030-04-26',6),('2030-04-27',7),('2030-04-28',1),('2030-04-29',2),('2030-04-30',3),('2030-05-01',4),('2030-05-02',5),('2030-05-03',6),('2030-05-04',7),('2030-05-05',1),('2030-05-06',2),('2030-05-07',3),('2030-05-08',4),('2030-05-09',5),('2030-05-10',6),('2030-05-11',7),('2030-05-12',1),('2030-05-13',2),('2030-05-14',3),('2030-05-15',4),('2030-05-16',5),('2030-05-17',6),('2030-05-18',7),('2030-05-19',1),('2030-05-20',2),('2030-05-21',3),('2030-05-22',4),('2030-05-23',5),('2030-05-24',6),('2030-05-25',7),('2030-05-26',1),('2030-05-27',2),('2030-05-28',3),('2030-05-29',4),('2030-05-30',5),('2030-05-31',6),('2030-06-01',7),('2030-06-02',1),('2030-06-03',2),('2030-06-04',3),('2030-06-05',4),('2030-06-06',5),('2030-06-07',6),('2030-06-08',7),('2030-06-09',1),('2030-06-10',2),('2030-06-11',3),('2030-06-12',4),('2030-06-13',5),('2030-06-14',6),('2030-06-15',7),('2030-06-16',1),('2030-06-17',2),('2030-06-18',3),('2030-06-19',4),('2030-06-20',5),('2030-06-21',6),('2030-06-22',7),('2030-06-23',1),('2030-06-24',2),('2030-06-25',3),('2030-06-26',4),('2030-06-27',5),('2030-06-28',6),('2030-06-29',7),('2030-06-30',1),('2030-07-01',2),('2030-07-02',3),('2030-07-03',4),('2030-07-04',5),('2030-07-05',6),('2030-07-06',7),('2030-07-07',1),('2030-07-08',2),('2030-07-09',3),('2030-07-10',4),('2030-07-11',5),('2030-07-12',6),('2030-07-13',7),('2030-07-14',1),('2030-07-15',2),('2030-07-16',3),('2030-07-17',4),('2030-07-18',5),('2030-07-19',6),('2030-07-20',7),('2030-07-21',1),('2030-07-22',2),('2030-07-23',3),('2030-07-24',4),('2030-07-25',5),('2030-07-26',6),('2030-07-27',7),('2030-07-28',1),('2030-07-29',2),('2030-07-30',3),('2030-07-31',4),('2030-08-01',5),('2030-08-02',6),('2030-08-03',7),('2030-08-04',1),('2030-08-05',2),('2030-08-06',3),('2030-08-07',4),('2030-08-08',5),('2030-08-09',6),('2030-08-10',7),('2030-08-11',1),('2030-08-12',2),('2030-08-13',3),('2030-08-14',4),('2030-08-15',5),('2030-08-16',6),('2030-08-17',7),('2030-08-18',1),('2030-08-19',2),('2030-08-20',3),('2030-08-21',4),('2030-08-22',5),('2030-08-23',6),('2030-08-24',7),('2030-08-25',1),('2030-08-26',2),('2030-08-27',3),('2030-08-28',4),('2030-08-29',5),('2030-08-30',6),('2030-08-31',7),('2030-09-01',1),('2030-09-02',2),('2030-09-03',3),('2030-09-04',4),('2030-09-05',5),('2030-09-06',6),('2030-09-07',7),('2030-09-08',1),('2030-09-09',2),('2030-09-10',3),('2030-09-11',4),('2030-09-12',5),('2030-09-13',6),('2030-09-14',7),('2030-09-15',1),('2030-09-16',2),('2030-09-17',3),('2030-09-18',4),('2030-09-19',5),('2030-09-20',6),('2030-09-21',7),('2030-09-22',1),('2030-09-23',2),('2030-09-24',3),('2030-09-25',4),('2030-09-26',5),('2030-09-27',6),('2030-09-28',7),('2030-09-29',1),('2030-09-30',2),('2030-10-01',3),('2030-10-02',4),('2030-10-03',5),('2030-10-04',6),('2030-10-05',7),('2030-10-06',1),('2030-10-07',2),('2030-10-08',3),('2030-10-09',4),('2030-10-10',5),('2030-10-11',6),('2030-10-12',7),('2030-10-13',1),('2030-10-14',2),('2030-10-15',3),('2030-10-16',4),('2030-10-17',5),('2030-10-18',6),('2030-10-19',7),('2030-10-20',1),('2030-10-21',2),('2030-10-22',3),('2030-10-23',4),('2030-10-24',5),('2030-10-25',6),('2030-10-26',7),('2030-10-27',1),('2030-10-28',2),('2030-10-29',3),('2030-10-30',4),('2030-10-31',5),('2030-11-01',6),('2030-11-02',7),('2030-11-03',1),('2030-11-04',2),('2030-11-05',3),('2030-11-06',4),('2030-11-07',5),('2030-11-08',6),('2030-11-09',7),('2030-11-10',1),('2030-11-11',2),('2030-11-12',3),('2030-11-13',4),('2030-11-14',5),('2030-11-15',6),('2030-11-16',7),('2030-11-17',1),('2030-11-18',2),('2030-11-19',3),('2030-11-20',4),('2030-11-21',5),('2030-11-22',6),('2030-11-23',7),('2030-11-24',1),('2030-11-25',2),('2030-11-26',3),('2030-11-27',4),('2030-11-28',5),('2030-11-29',6),('2030-11-30',7),('2030-12-01',1),('2030-12-02',2),('2030-12-03',3),('2030-12-04',4),('2030-12-05',5),('2030-12-06',6),('2030-12-07',7),('2030-12-08',1),('2030-12-09',2),('2030-12-10',3),('2030-12-11',4),('2030-12-12',5),('2030-12-13',6),('2030-12-14',7),('2030-12-15',1),('2030-12-16',2),('2030-12-17',3),('2030-12-18',4),('2030-12-19',5),('2030-12-20',6),('2030-12-21',7),('2030-12-22',1),('2030-12-23',2),('2030-12-24',3),('2030-12-25',4),('2030-12-26',5),('2030-12-27',6),('2030-12-28',7),('2030-12-29',1),('2030-12-30',2),('2030-12-31',3),('2031-01-01',4),('2031-01-02',5),('2031-01-03',6),('2031-01-04',7),('2031-01-05',1),('2031-01-06',2),('2031-01-07',3),('2031-01-08',4),('2031-01-09',5),('2031-01-10',6),('2031-01-11',7),('2031-01-12',1),('2031-01-13',2),('2031-01-14',3),('2031-01-15',4),('2031-01-16',5),('2031-01-17',6),('2031-01-18',7),('2031-01-19',1),('2031-01-20',2),('2031-01-21',3),('2031-01-22',4),('2031-01-23',5),('2031-01-24',6),('2031-01-25',7),('2031-01-26',1),('2031-01-27',2),('2031-01-28',3),('2031-01-29',4),('2031-01-30',5),('2031-01-31',6),('2031-02-01',7),('2031-02-02',1),('2031-02-03',2),('2031-02-04',3),('2031-02-05',4),('2031-02-06',5),('2031-02-07',6),('2031-02-08',7),('2031-02-09',1),('2031-02-10',2),('2031-02-11',3),('2031-02-12',4),('2031-02-13',5),('2031-02-14',6),('2031-02-15',7),('2031-02-16',1),('2031-02-17',2),('2031-02-18',3),('2031-02-19',4),('2031-02-20',5),('2031-02-21',6),('2031-02-22',7),('2031-02-23',1),('2031-02-24',2),('2031-02-25',3),('2031-02-26',4),('2031-02-27',5),('2031-02-28',6),('2031-03-01',7),('2031-03-02',1),('2031-03-03',2),('2031-03-04',3),('2031-03-05',4),('2031-03-06',5),('2031-03-07',6),('2031-03-08',7),('2031-03-09',1),('2031-03-10',2),('2031-03-11',3),('2031-03-12',4),('2031-03-13',5),('2031-03-14',6),('2031-03-15',7),('2031-03-16',1),('2031-03-17',2),('2031-03-18',3),('2031-03-19',4),('2031-03-20',5),('2031-03-21',6),('2031-03-22',7),('2031-03-23',1),('2031-03-24',2),('2031-03-25',3),('2031-03-26',4),('2031-03-27',5),('2031-03-28',6),('2031-03-29',7),('2031-03-30',1),('2031-03-31',2),('2031-04-01',3),('2031-04-02',4),('2031-04-03',5),('2031-04-04',6),('2031-04-05',7),('2031-04-06',1),('2031-04-07',2),('2031-04-08',3),('2031-04-09',4),('2031-04-10',5),('2031-04-11',6),('2031-04-12',7),('2031-04-13',1),('2031-04-14',2),('2031-04-15',3),('2031-04-16',4),('2031-04-17',5),('2031-04-18',6),('2031-04-19',7),('2031-04-20',1),('2031-04-21',2),('2031-04-22',3),('2031-04-23',4),('2031-04-24',5),('2031-04-25',6),('2031-04-26',7),('2031-04-27',1),('2031-04-28',2),('2031-04-29',3),('2031-04-30',4),('2031-05-01',5),('2031-05-02',6),('2031-05-03',7),('2031-05-04',1),('2031-05-05',2),('2031-05-06',3),('2031-05-07',4),('2031-05-08',5),('2031-05-09',6),('2031-05-10',7),('2031-05-11',1),('2031-05-12',2),('2031-05-13',3),('2031-05-14',4),('2031-05-15',5),('2031-05-16',6),('2031-05-17',7),('2031-05-18',1),('2031-05-19',2),('2031-05-20',3),('2031-05-21',4),('2031-05-22',5),('2031-05-23',6),('2031-05-24',7),('2031-05-25',1),('2031-05-26',2),('2031-05-27',3),('2031-05-28',4),('2031-05-29',5),('2031-05-30',6),('2031-05-31',7),('2031-06-01',1),('2031-06-02',2),('2031-06-03',3),('2031-06-04',4),('2031-06-05',5),('2031-06-06',6),('2031-06-07',7),('2031-06-08',1),('2031-06-09',2),('2031-06-10',3),('2031-06-11',4),('2031-06-12',5),('2031-06-13',6),('2031-06-14',7),('2031-06-15',1),('2031-06-16',2),('2031-06-17',3),('2031-06-18',4),('2031-06-19',5),('2031-06-20',6),('2031-06-21',7),('2031-06-22',1),('2031-06-23',2),('2031-06-24',3),('2031-06-25',4),('2031-06-26',5),('2031-06-27',6),('2031-06-28',7),('2031-06-29',1),('2031-06-30',2),('2031-07-01',3),('2031-07-02',4),('2031-07-03',5),('2031-07-04',6),('2031-07-05',7),('2031-07-06',1),('2031-07-07',2),('2031-07-08',3),('2031-07-09',4),('2031-07-10',5),('2031-07-11',6),('2031-07-12',7),('2031-07-13',1),('2031-07-14',2),('2031-07-15',3),('2031-07-16',4),('2031-07-17',5),('2031-07-18',6),('2031-07-19',7),('2031-07-20',1),('2031-07-21',2),('2031-07-22',3),('2031-07-23',4),('2031-07-24',5),('2031-07-25',6),('2031-07-26',7),('2031-07-27',1),('2031-07-28',2),('2031-07-29',3),('2031-07-30',4),('2031-07-31',5),('2031-08-01',6),('2031-08-02',7),('2031-08-03',1),('2031-08-04',2),('2031-08-05',3),('2031-08-06',4),('2031-08-07',5),('2031-08-08',6),('2031-08-09',7),('2031-08-10',1),('2031-08-11',2),('2031-08-12',3),('2031-08-13',4),('2031-08-14',5),('2031-08-15',6),('2031-08-16',7),('2031-08-17',1),('2031-08-18',2),('2031-08-19',3),('2031-08-20',4),('2031-08-21',5),('2031-08-22',6),('2031-08-23',7),('2031-08-24',1),('2031-08-25',2),('2031-08-26',3),('2031-08-27',4),('2031-08-28',5),('2031-08-29',6),('2031-08-30',7),('2031-08-31',1),('2031-09-01',2),('2031-09-02',3),('2031-09-03',4),('2031-09-04',5),('2031-09-05',6),('2031-09-06',7),('2031-09-07',1),('2031-09-08',2),('2031-09-09',3),('2031-09-10',4),('2031-09-11',5),('2031-09-12',6),('2031-09-13',7),('2031-09-14',1),('2031-09-15',2),('2031-09-16',3),('2031-09-17',4),('2031-09-18',5),('2031-09-19',6),('2031-09-20',7),('2031-09-21',1),('2031-09-22',2),('2031-09-23',3),('2031-09-24',4),('2031-09-25',5),('2031-09-26',6),('2031-09-27',7),('2031-09-28',1),('2031-09-29',2),('2031-09-30',3),('2031-10-01',4),('2031-10-02',5),('2031-10-03',6),('2031-10-04',7),('2031-10-05',1),('2031-10-06',2),('2031-10-07',3),('2031-10-08',4),('2031-10-09',5),('2031-10-10',6),('2031-10-11',7),('2031-10-12',1),('2031-10-13',2),('2031-10-14',3),('2031-10-15',4),('2031-10-16',5),('2031-10-17',6),('2031-10-18',7),('2031-10-19',1),('2031-10-20',2),('2031-10-21',3),('2031-10-22',4),('2031-10-23',5),('2031-10-24',6),('2031-10-25',7),('2031-10-26',1),('2031-10-27',2),('2031-10-28',3),('2031-10-29',4),('2031-10-30',5),('2031-10-31',6),('2031-11-01',7),('2031-11-02',1),('2031-11-03',2),('2031-11-04',3),('2031-11-05',4),('2031-11-06',5),('2031-11-07',6),('2031-11-08',7),('2031-11-09',1),('2031-11-10',2),('2031-11-11',3),('2031-11-12',4),('2031-11-13',5),('2031-11-14',6),('2031-11-15',7),('2031-11-16',1),('2031-11-17',2),('2031-11-18',3),('2031-11-19',4),('2031-11-20',5),('2031-11-21',6),('2031-11-22',7),('2031-11-23',1),('2031-11-24',2),('2031-11-25',3),('2031-11-26',4),('2031-11-27',5),('2031-11-28',6),('2031-11-29',7),('2031-11-30',1),('2031-12-01',2),('2031-12-02',3),('2031-12-03',4),('2031-12-04',5),('2031-12-05',6),('2031-12-06',7),('2031-12-07',1),('2031-12-08',2),('2031-12-09',3),('2031-12-10',4),('2031-12-11',5),('2031-12-12',6),('2031-12-13',7),('2031-12-14',1),('2031-12-15',2),('2031-12-16',3),('2031-12-17',4),('2031-12-18',5),('2031-12-19',6),('2031-12-20',7),('2031-12-21',1),('2031-12-22',2),('2031-12-23',3),('2031-12-24',4),('2031-12-25',5),('2031-12-26',6),('2031-12-27',7),('2031-12-28',1),('2031-12-29',2),('2031-12-30',3),('2031-12-31',4),('2032-01-01',5),('2032-01-02',6),('2032-01-03',7),('2032-01-04',1),('2032-01-05',2),('2032-01-06',3),('2032-01-07',4),('2032-01-08',5),('2032-01-09',6),('2032-01-10',7),('2032-01-11',1),('2032-01-12',2),('2032-01-13',3),('2032-01-14',4),('2032-01-15',5),('2032-01-16',6),('2032-01-17',7),('2032-01-18',1),('2032-01-19',2),('2032-01-20',3),('2032-01-21',4),('2032-01-22',5),('2032-01-23',6),('2032-01-24',7),('2032-01-25',1),('2032-01-26',2),('2032-01-27',3),('2032-01-28',4),('2032-01-29',5),('2032-01-30',6),('2032-01-31',7),('2032-02-01',1),('2032-02-02',2),('2032-02-03',3),('2032-02-04',4),('2032-02-05',5),('2032-02-06',6),('2032-02-07',7),('2032-02-08',1),('2032-02-09',2),('2032-02-10',3),('2032-02-11',4),('2032-02-12',5),('2032-02-13',6),('2032-02-14',7),('2032-02-15',1),('2032-02-16',2),('2032-02-17',3),('2032-02-18',4),('2032-02-19',5),('2032-02-20',6),('2032-02-21',7),('2032-02-22',1),('2032-02-23',2),('2032-02-24',3),('2032-02-25',4),('2032-02-26',5),('2032-02-27',6),('2032-02-28',7),('2032-02-29',1),('2032-03-01',2),('2032-03-02',3),('2032-03-03',4),('2032-03-04',5),('2032-03-05',6),('2032-03-06',7),('2032-03-07',1),('2032-03-08',2),('2032-03-09',3),('2032-03-10',4),('2032-03-11',5),('2032-03-12',6),('2032-03-13',7),('2032-03-14',1),('2032-03-15',2),('2032-03-16',3),('2032-03-17',4),('2032-03-18',5),('2032-03-19',6),('2032-03-20',7),('2032-03-21',1),('2032-03-22',2),('2032-03-23',3),('2032-03-24',4),('2032-03-25',5),('2032-03-26',6),('2032-03-27',7),('2032-03-28',1),('2032-03-29',2),('2032-03-30',3),('2032-03-31',4),('2032-04-01',5),('2032-04-02',6),('2032-04-03',7),('2032-04-04',1),('2032-04-05',2),('2032-04-06',3),('2032-04-07',4),('2032-04-08',5),('2032-04-09',6),('2032-04-10',7),('2032-04-11',1),('2032-04-12',2),('2032-04-13',3),('2032-04-14',4),('2032-04-15',5),('2032-04-16',6),('2032-04-17',7),('2032-04-18',1),('2032-04-19',2),('2032-04-20',3),('2032-04-21',4),('2032-04-22',5),('2032-04-23',6),('2032-04-24',7),('2032-04-25',1),('2032-04-26',2),('2032-04-27',3),('2032-04-28',4),('2032-04-29',5),('2032-04-30',6),('2032-05-01',7),('2032-05-02',1),('2032-05-03',2),('2032-05-04',3),('2032-05-05',4),('2032-05-06',5),('2032-05-07',6),('2032-05-08',7),('2032-05-09',1),('2032-05-10',2),('2032-05-11',3),('2032-05-12',4),('2032-05-13',5),('2032-05-14',6),('2032-05-15',7),('2032-05-16',1),('2032-05-17',2),('2032-05-18',3),('2032-05-19',4),('2032-05-20',5),('2032-05-21',6),('2032-05-22',7),('2032-05-23',1),('2032-05-24',2),('2032-05-25',3),('2032-05-26',4),('2032-05-27',5),('2032-05-28',6),('2032-05-29',7),('2032-05-30',1),('2032-05-31',2),('2032-06-01',3),('2032-06-02',4),('2032-06-03',5),('2032-06-04',6),('2032-06-05',7),('2032-06-06',1),('2032-06-07',2),('2032-06-08',3),('2032-06-09',4),('2032-06-10',5),('2032-06-11',6),('2032-06-12',7),('2032-06-13',1),('2032-06-14',2),('2032-06-15',3),('2032-06-16',4),('2032-06-17',5),('2032-06-18',6),('2032-06-19',7),('2032-06-20',1),('2032-06-21',2),('2032-06-22',3),('2032-06-23',4),('2032-06-24',5),('2032-06-25',6),('2032-06-26',7),('2032-06-27',1),('2032-06-28',2),('2032-06-29',3),('2032-06-30',4),('2032-07-01',5),('2032-07-02',6),('2032-07-03',7),('2032-07-04',1),('2032-07-05',2),('2032-07-06',3),('2032-07-07',4),('2032-07-08',5),('2032-07-09',6),('2032-07-10',7),('2032-07-11',1),('2032-07-12',2),('2032-07-13',3),('2032-07-14',4),('2032-07-15',5),('2032-07-16',6),('2032-07-17',7),('2032-07-18',1),('2032-07-19',2),('2032-07-20',3),('2032-07-21',4),('2032-07-22',5),('2032-07-23',6),('2032-07-24',7),('2032-07-25',1),('2032-07-26',2),('2032-07-27',3),('2032-07-28',4),('2032-07-29',5),('2032-07-30',6),('2032-07-31',7),('2032-08-01',1),('2032-08-02',2),('2032-08-03',3),('2032-08-04',4),('2032-08-05',5),('2032-08-06',6),('2032-08-07',7),('2032-08-08',1),('2032-08-09',2),('2032-08-10',3),('2032-08-11',4),('2032-08-12',5),('2032-08-13',6),('2032-08-14',7),('2032-08-15',1),('2032-08-16',2),('2032-08-17',3),('2032-08-18',4),('2032-08-19',5),('2032-08-20',6),('2032-08-21',7),('2032-08-22',1),('2032-08-23',2),('2032-08-24',3),('2032-08-25',4),('2032-08-26',5),('2032-08-27',6),('2032-08-28',7),('2032-08-29',1),('2032-08-30',2),('2032-08-31',3),('2032-09-01',4),('2032-09-02',5),('2032-09-03',6),('2032-09-04',7),('2032-09-05',1),('2032-09-06',2),('2032-09-07',3),('2032-09-08',4),('2032-09-09',5),('2032-09-10',6),('2032-09-11',7),('2032-09-12',1),('2032-09-13',2),('2032-09-14',3),('2032-09-15',4),('2032-09-16',5),('2032-09-17',6),('2032-09-18',7),('2032-09-19',1),('2032-09-20',2),('2032-09-21',3),('2032-09-22',4),('2032-09-23',5),('2032-09-24',6),('2032-09-25',7),('2032-09-26',1),('2032-09-27',2),('2032-09-28',3),('2032-09-29',4),('2032-09-30',5),('2032-10-01',6),('2032-10-02',7),('2032-10-03',1),('2032-10-04',2),('2032-10-05',3),('2032-10-06',4),('2032-10-07',5),('2032-10-08',6),('2032-10-09',7),('2032-10-10',1),('2032-10-11',2),('2032-10-12',3),('2032-10-13',4),('2032-10-14',5),('2032-10-15',6),('2032-10-16',7),('2032-10-17',1),('2032-10-18',2),('2032-10-19',3),('2032-10-20',4),('2032-10-21',5),('2032-10-22',6),('2032-10-23',7),('2032-10-24',1),('2032-10-25',2),('2032-10-26',3),('2032-10-27',4),('2032-10-28',5),('2032-10-29',6),('2032-10-30',7),('2032-10-31',1),('2032-11-01',2),('2032-11-02',3),('2032-11-03',4),('2032-11-04',5),('2032-11-05',6),('2032-11-06',7),('2032-11-07',1),('2032-11-08',2),('2032-11-09',3),('2032-11-10',4),('2032-11-11',5),('2032-11-12',6),('2032-11-13',7),('2032-11-14',1),('2032-11-15',2),('2032-11-16',3),('2032-11-17',4),('2032-11-18',5),('2032-11-19',6),('2032-11-20',7),('2032-11-21',1),('2032-11-22',2),('2032-11-23',3),('2032-11-24',4),('2032-11-25',5),('2032-11-26',6),('2032-11-27',7),('2032-11-28',1),('2032-11-29',2),('2032-11-30',3),('2032-12-01',4),('2032-12-02',5),('2032-12-03',6),('2032-12-04',7),('2032-12-05',1),('2032-12-06',2),('2032-12-07',3),('2032-12-08',4),('2032-12-09',5),('2032-12-10',6),('2032-12-11',7),('2032-12-12',1),('2032-12-13',2),('2032-12-14',3),('2032-12-15',4),('2032-12-16',5),('2032-12-17',6),('2032-12-18',7),('2032-12-19',1),('2032-12-20',2),('2032-12-21',3),('2032-12-22',4),('2032-12-23',5),('2032-12-24',6),('2032-12-25',7),('2032-12-26',1),('2032-12-27',2),('2032-12-28',3),('2032-12-29',4),('2032-12-30',5),('2032-12-31',6),('2033-01-01',7),('2033-01-02',1),('2033-01-03',2),('2033-01-04',3),('2033-01-05',4),('2033-01-06',5),('2033-01-07',6),('2033-01-08',7),('2033-01-09',1),('2033-01-10',2),('2033-01-11',3),('2033-01-12',4),('2033-01-13',5),('2033-01-14',6),('2033-01-15',7),('2033-01-16',1),('2033-01-17',2),('2033-01-18',3),('2033-01-19',4),('2033-01-20',5),('2033-01-21',6),('2033-01-22',7),('2033-01-23',1),('2033-01-24',2),('2033-01-25',3),('2033-01-26',4),('2033-01-27',5),('2033-01-28',6),('2033-01-29',7),('2033-01-30',1),('2033-01-31',2),('2033-02-01',3),('2033-02-02',4),('2033-02-03',5),('2033-02-04',6),('2033-02-05',7),('2033-02-06',1),('2033-02-07',2),('2033-02-08',3),('2033-02-09',4),('2033-02-10',5),('2033-02-11',6),('2033-02-12',7),('2033-02-13',1),('2033-02-14',2),('2033-02-15',3),('2033-02-16',4),('2033-02-17',5),('2033-02-18',6),('2033-02-19',7),('2033-02-20',1),('2033-02-21',2),('2033-02-22',3),('2033-02-23',4),('2033-02-24',5),('2033-02-25',6),('2033-02-26',7),('2033-02-27',1),('2033-02-28',2),('2033-03-01',3),('2033-03-02',4),('2033-03-03',5),('2033-03-04',6),('2033-03-05',7),('2033-03-06',1),('2033-03-07',2),('2033-03-08',3),('2033-03-09',4),('2033-03-10',5),('2033-03-11',6),('2033-03-12',7),('2033-03-13',1),('2033-03-14',2),('2033-03-15',3),('2033-03-16',4),('2033-03-17',5),('2033-03-18',6),('2033-03-19',7),('2033-03-20',1),('2033-03-21',2),('2033-03-22',3),('2033-03-23',4),('2033-03-24',5),('2033-03-25',6),('2033-03-26',7),('2033-03-27',1),('2033-03-28',2),('2033-03-29',3),('2033-03-30',4),('2033-03-31',5),('2033-04-01',6),('2033-04-02',7),('2033-04-03',1),('2033-04-04',2),('2033-04-05',3),('2033-04-06',4),('2033-04-07',5),('2033-04-08',6),('2033-04-09',7),('2033-04-10',1),('2033-04-11',2),('2033-04-12',3),('2033-04-13',4),('2033-04-14',5),('2033-04-15',6),('2033-04-16',7),('2033-04-17',1),('2033-04-18',2),('2033-04-19',3),('2033-04-20',4),('2033-04-21',5),('2033-04-22',6),('2033-04-23',7),('2033-04-24',1),('2033-04-25',2),('2033-04-26',3),('2033-04-27',4),('2033-04-28',5),('2033-04-29',6),('2033-04-30',7),('2033-05-01',1),('2033-05-02',2),('2033-05-03',3),('2033-05-04',4),('2033-05-05',5),('2033-05-06',6),('2033-05-07',7),('2033-05-08',1),('2033-05-09',2),('2033-05-10',3),('2033-05-11',4),('2033-05-12',5),('2033-05-13',6),('2033-05-14',7),('2033-05-15',1),('2033-05-16',2),('2033-05-17',3),('2033-05-18',4),('2033-05-19',5),('2033-05-20',6),('2033-05-21',7),('2033-05-22',1),('2033-05-23',2),('2033-05-24',3),('2033-05-25',4),('2033-05-26',5),('2033-05-27',6),('2033-05-28',7),('2033-05-29',1),('2033-05-30',2),('2033-05-31',3),('2033-06-01',4),('2033-06-02',5),('2033-06-03',6),('2033-06-04',7),('2033-06-05',1),('2033-06-06',2),('2033-06-07',3),('2033-06-08',4),('2033-06-09',5),('2033-06-10',6),('2033-06-11',7),('2033-06-12',1),('2033-06-13',2),('2033-06-14',3),('2033-06-15',4),('2033-06-16',5),('2033-06-17',6),('2033-06-18',7),('2033-06-19',1),('2033-06-20',2),('2033-06-21',3),('2033-06-22',4),('2033-06-23',5),('2033-06-24',6),('2033-06-25',7),('2033-06-26',1),('2033-06-27',2),('2033-06-28',3),('2033-06-29',4),('2033-06-30',5),('2033-07-01',6),('2033-07-02',7),('2033-07-03',1),('2033-07-04',2),('2033-07-05',3),('2033-07-06',4),('2033-07-07',5),('2033-07-08',6),('2033-07-09',7),('2033-07-10',1),('2033-07-11',2),('2033-07-12',3),('2033-07-13',4),('2033-07-14',5),('2033-07-15',6),('2033-07-16',7),('2033-07-17',1),('2033-07-18',2),('2033-07-19',3),('2033-07-20',4),('2033-07-21',5),('2033-07-22',6),('2033-07-23',7),('2033-07-24',1),('2033-07-25',2),('2033-07-26',3),('2033-07-27',4),('2033-07-28',5),('2033-07-29',6),('2033-07-30',7),('2033-07-31',1),('2033-08-01',2),('2033-08-02',3),('2033-08-03',4),('2033-08-04',5),('2033-08-05',6),('2033-08-06',7),('2033-08-07',1),('2033-08-08',2),('2033-08-09',3),('2033-08-10',4),('2033-08-11',5),('2033-08-12',6),('2033-08-13',7),('2033-08-14',1),('2033-08-15',2),('2033-08-16',3),('2033-08-17',4),('2033-08-18',5),('2033-08-19',6),('2033-08-20',7),('2033-08-21',1),('2033-08-22',2),('2033-08-23',3),('2033-08-24',4),('2033-08-25',5),('2033-08-26',6),('2033-08-27',7),('2033-08-28',1),('2033-08-29',2),('2033-08-30',3),('2033-08-31',4),('2033-09-01',5),('2033-09-02',6),('2033-09-03',7),('2033-09-04',1),('2033-09-05',2),('2033-09-06',3),('2033-09-07',4),('2033-09-08',5),('2033-09-09',6),('2033-09-10',7),('2033-09-11',1),('2033-09-12',2),('2033-09-13',3),('2033-09-14',4),('2033-09-15',5),('2033-09-16',6),('2033-09-17',7),('2033-09-18',1),('2033-09-19',2),('2033-09-20',3),('2033-09-21',4),('2033-09-22',5),('2033-09-23',6),('2033-09-24',7),('2033-09-25',1),('2033-09-26',2),('2033-09-27',3),('2033-09-28',4),('2033-09-29',5),('2033-09-30',6),('2033-10-01',7),('2033-10-02',1),('2033-10-03',2),('2033-10-04',3),('2033-10-05',4),('2033-10-06',5),('2033-10-07',6),('2033-10-08',7),('2033-10-09',1),('2033-10-10',2),('2033-10-11',3),('2033-10-12',4),('2033-10-13',5),('2033-10-14',6),('2033-10-15',7),('2033-10-16',1),('2033-10-17',2),('2033-10-18',3),('2033-10-19',4),('2033-10-20',5),('2033-10-21',6),('2033-10-22',7),('2033-10-23',1),('2033-10-24',2),('2033-10-25',3),('2033-10-26',4),('2033-10-27',5),('2033-10-28',6),('2033-10-29',7),('2033-10-30',1),('2033-10-31',2),('2033-11-01',3),('2033-11-02',4),('2033-11-03',5),('2033-11-04',6),('2033-11-05',7),('2033-11-06',1),('2033-11-07',2),('2033-11-08',3),('2033-11-09',4),('2033-11-10',5),('2033-11-11',6),('2033-11-12',7),('2033-11-13',1),('2033-11-14',2),('2033-11-15',3),('2033-11-16',4),('2033-11-17',5),('2033-11-18',6),('2033-11-19',7),('2033-11-20',1),('2033-11-21',2),('2033-11-22',3),('2033-11-23',4),('2033-11-24',5),('2033-11-25',6),('2033-11-26',7),('2033-11-27',1),('2033-11-28',2),('2033-11-29',3),('2033-11-30',4),('2033-12-01',5),('2033-12-02',6),('2033-12-03',7),('2033-12-04',1),('2033-12-05',2),('2033-12-06',3),('2033-12-07',4),('2033-12-08',5),('2033-12-09',6),('2033-12-10',7),('2033-12-11',1),('2033-12-12',2),('2033-12-13',3),('2033-12-14',4),('2033-12-15',5),('2033-12-16',6),('2033-12-17',7),('2033-12-18',1),('2033-12-19',2),('2033-12-20',3),('2033-12-21',4),('2033-12-22',5),('2033-12-23',6),('2033-12-24',7),('2033-12-25',1),('2033-12-26',2),('2033-12-27',3),('2033-12-28',4),('2033-12-29',5),('2033-12-30',6),('2033-12-31',7),('2034-01-01',1),('2034-01-02',2),('2034-01-03',3),('2034-01-04',4),('2034-01-05',5),('2034-01-06',6),('2034-01-07',7),('2034-01-08',1),('2034-01-09',2),('2034-01-10',3),('2034-01-11',4),('2034-01-12',5),('2034-01-13',6),('2034-01-14',7),('2034-01-15',1),('2034-01-16',2),('2034-01-17',3),('2034-01-18',4),('2034-01-19',5),('2034-01-20',6),('2034-01-21',7),('2034-01-22',1),('2034-01-23',2),('2034-01-24',3),('2034-01-25',4),('2034-01-26',5),('2034-01-27',6),('2034-01-28',7),('2034-01-29',1),('2034-01-30',2),('2034-01-31',3),('2034-02-01',4),('2034-02-02',5),('2034-02-03',6),('2034-02-04',7),('2034-02-05',1),('2034-02-06',2),('2034-02-07',3),('2034-02-08',4),('2034-02-09',5),('2034-02-10',6),('2034-02-11',7),('2034-02-12',1),('2034-02-13',2),('2034-02-14',3),('2034-02-15',4),('2034-02-16',5),('2034-02-17',6),('2034-02-18',7),('2034-02-19',1),('2034-02-20',2),('2034-02-21',3),('2034-02-22',4),('2034-02-23',5),('2034-02-24',6),('2034-02-25',7),('2034-02-26',1),('2034-02-27',2),('2034-02-28',3),('2034-03-01',4),('2034-03-02',5),('2034-03-03',6),('2034-03-04',7),('2034-03-05',1),('2034-03-06',2),('2034-03-07',3),('2034-03-08',4),('2034-03-09',5),('2034-03-10',6),('2034-03-11',7),('2034-03-12',1),('2034-03-13',2),('2034-03-14',3),('2034-03-15',4),('2034-03-16',5),('2034-03-17',6),('2034-03-18',7),('2034-03-19',1),('2034-03-20',2),('2034-03-21',3),('2034-03-22',4),('2034-03-23',5),('2034-03-24',6),('2034-03-25',7),('2034-03-26',1),('2034-03-27',2),('2034-03-28',3),('2034-03-29',4),('2034-03-30',5),('2034-03-31',6),('2034-04-01',7),('2034-04-02',1),('2034-04-03',2),('2034-04-04',3),('2034-04-05',4),('2034-04-06',5),('2034-04-07',6),('2034-04-08',7),('2034-04-09',1),('2034-04-10',2),('2034-04-11',3),('2034-04-12',4),('2034-04-13',5),('2034-04-14',6),('2034-04-15',7),('2034-04-16',1),('2034-04-17',2),('2034-04-18',3),('2034-04-19',4),('2034-04-20',5),('2034-04-21',6),('2034-04-22',7),('2034-04-23',1),('2034-04-24',2),('2034-04-25',3),('2034-04-26',4),('2034-04-27',5),('2034-04-28',6),('2034-04-29',7),('2034-04-30',1),('2034-05-01',2),('2034-05-02',3),('2034-05-03',4),('2034-05-04',5),('2034-05-05',6),('2034-05-06',7),('2034-05-07',1),('2034-05-08',2),('2034-05-09',3),('2034-05-10',4),('2034-05-11',5),('2034-05-12',6),('2034-05-13',7),('2034-05-14',1),('2034-05-15',2),('2034-05-16',3),('2034-05-17',4),('2034-05-18',5),('2034-05-19',6),('2034-05-20',7),('2034-05-21',1),('2034-05-22',2),('2034-05-23',3),('2034-05-24',4),('2034-05-25',5),('2034-05-26',6),('2034-05-27',7),('2034-05-28',1),('2034-05-29',2),('2034-05-30',3),('2034-05-31',4),('2034-06-01',5),('2034-06-02',6),('2034-06-03',7),('2034-06-04',1),('2034-06-05',2),('2034-06-06',3),('2034-06-07',4),('2034-06-08',5),('2034-06-09',6),('2034-06-10',7),('2034-06-11',1),('2034-06-12',2),('2034-06-13',3),('2034-06-14',4),('2034-06-15',5),('2034-06-16',6),('2034-06-17',7),('2034-06-18',1),('2034-06-19',2),('2034-06-20',3),('2034-06-21',4),('2034-06-22',5),('2034-06-23',6),('2034-06-24',7),('2034-06-25',1),('2034-06-26',2),('2034-06-27',3),('2034-06-28',4),('2034-06-29',5),('2034-06-30',6),('2034-07-01',7),('2034-07-02',1),('2034-07-03',2),('2034-07-04',3),('2034-07-05',4),('2034-07-06',5),('2034-07-07',6),('2034-07-08',7),('2034-07-09',1),('2034-07-10',2),('2034-07-11',3),('2034-07-12',4),('2034-07-13',5),('2034-07-14',6),('2034-07-15',7),('2034-07-16',1),('2034-07-17',2),('2034-07-18',3),('2034-07-19',4),('2034-07-20',5),('2034-07-21',6),('2034-07-22',7),('2034-07-23',1),('2034-07-24',2),('2034-07-25',3),('2034-07-26',4),('2034-07-27',5),('2034-07-28',6),('2034-07-29',7),('2034-07-30',1),('2034-07-31',2),('2034-08-01',3),('2034-08-02',4),('2034-08-03',5),('2034-08-04',6),('2034-08-05',7),('2034-08-06',1),('2034-08-07',2),('2034-08-08',3),('2034-08-09',4),('2034-08-10',5),('2034-08-11',6),('2034-08-12',7),('2034-08-13',1),('2034-08-14',2),('2034-08-15',3),('2034-08-16',4),('2034-08-17',5),('2034-08-18',6),('2034-08-19',7),('2034-08-20',1),('2034-08-21',2),('2034-08-22',3),('2034-08-23',4),('2034-08-24',5),('2034-08-25',6),('2034-08-26',7),('2034-08-27',1),('2034-08-28',2),('2034-08-29',3),('2034-08-30',4),('2034-08-31',5),('2034-09-01',6),('2034-09-02',7),('2034-09-03',1),('2034-09-04',2),('2034-09-05',3),('2034-09-06',4),('2034-09-07',5),('2034-09-08',6),('2034-09-09',7),('2034-09-10',1),('2034-09-11',2),('2034-09-12',3),('2034-09-13',4),('2034-09-14',5),('2034-09-15',6),('2034-09-16',7),('2034-09-17',1),('2034-09-18',2),('2034-09-19',3),('2034-09-20',4),('2034-09-21',5),('2034-09-22',6),('2034-09-23',7),('2034-09-24',1),('2034-09-25',2),('2034-09-26',3),('2034-09-27',4),('2034-09-28',5),('2034-09-29',6),('2034-09-30',7),('2034-10-01',1),('2034-10-02',2),('2034-10-03',3),('2034-10-04',4),('2034-10-05',5),('2034-10-06',6),('2034-10-07',7),('2034-10-08',1),('2034-10-09',2),('2034-10-10',3),('2034-10-11',4),('2034-10-12',5),('2034-10-13',6),('2034-10-14',7),('2034-10-15',1),('2034-10-16',2),('2034-10-17',3),('2034-10-18',4),('2034-10-19',5),('2034-10-20',6),('2034-10-21',7),('2034-10-22',1),('2034-10-23',2),('2034-10-24',3),('2034-10-25',4),('2034-10-26',5),('2034-10-27',6),('2034-10-28',7),('2034-10-29',1),('2034-10-30',2),('2034-10-31',3),('2034-11-01',4),('2034-11-02',5),('2034-11-03',6),('2034-11-04',7),('2034-11-05',1),('2034-11-06',2),('2034-11-07',3),('2034-11-08',4),('2034-11-09',5),('2034-11-10',6),('2034-11-11',7),('2034-11-12',1),('2034-11-13',2),('2034-11-14',3),('2034-11-15',4),('2034-11-16',5),('2034-11-17',6),('2034-11-18',7),('2034-11-19',1),('2034-11-20',2),('2034-11-21',3),('2034-11-22',4),('2034-11-23',5),('2034-11-24',6),('2034-11-25',7),('2034-11-26',1),('2034-11-27',2),('2034-11-28',3),('2034-11-29',4),('2034-11-30',5),('2034-12-01',6),('2034-12-02',7),('2034-12-03',1),('2034-12-04',2),('2034-12-05',3),('2034-12-06',4),('2034-12-07',5),('2034-12-08',6),('2034-12-09',7),('2034-12-10',1),('2034-12-11',2),('2034-12-12',3),('2034-12-13',4),('2034-12-14',5),('2034-12-15',6),('2034-12-16',7),('2034-12-17',1),('2034-12-18',2),('2034-12-19',3),('2034-12-20',4),('2034-12-21',5),('2034-12-22',6),('2034-12-23',7),('2034-12-24',1),('2034-12-25',2),('2034-12-26',3),('2034-12-27',4),('2034-12-28',5),('2034-12-29',6),('2034-12-30',7),('2034-12-31',1),('2035-01-01',2),('2035-01-02',3),('2035-01-03',4),('2035-01-04',5),('2035-01-05',6),('2035-01-06',7),('2035-01-07',1),('2035-01-08',2),('2035-01-09',3),('2035-01-10',4),('2035-01-11',5),('2035-01-12',6),('2035-01-13',7),('2035-01-14',1),('2035-01-15',2),('2035-01-16',3),('2035-01-17',4),('2035-01-18',5),('2035-01-19',6),('2035-01-20',7),('2035-01-21',1),('2035-01-22',2),('2035-01-23',3),('2035-01-24',4),('2035-01-25',5),('2035-01-26',6),('2035-01-27',7),('2035-01-28',1),('2035-01-29',2),('2035-01-30',3),('2035-01-31',4),('2035-02-01',5),('2035-02-02',6),('2035-02-03',7),('2035-02-04',1),('2035-02-05',2),('2035-02-06',3),('2035-02-07',4),('2035-02-08',5),('2035-02-09',6),('2035-02-10',7),('2035-02-11',1),('2035-02-12',2),('2035-02-13',3),('2035-02-14',4),('2035-02-15',5),('2035-02-16',6),('2035-02-17',7),('2035-02-18',1),('2035-02-19',2),('2035-02-20',3),('2035-02-21',4),('2035-02-22',5),('2035-02-23',6),('2035-02-24',7),('2035-02-25',1),('2035-02-26',2),('2035-02-27',3),('2035-02-28',4),('2035-03-01',5),('2035-03-02',6),('2035-03-03',7),('2035-03-04',1),('2035-03-05',2),('2035-03-06',3),('2035-03-07',4),('2035-03-08',5),('2035-03-09',6),('2035-03-10',7),('2035-03-11',1),('2035-03-12',2),('2035-03-13',3),('2035-03-14',4),('2035-03-15',5),('2035-03-16',6),('2035-03-17',7),('2035-03-18',1),('2035-03-19',2),('2035-03-20',3),('2035-03-21',4),('2035-03-22',5),('2035-03-23',6),('2035-03-24',7),('2035-03-25',1),('2035-03-26',2),('2035-03-27',3),('2035-03-28',4),('2035-03-29',5),('2035-03-30',6),('2035-03-31',7),('2035-04-01',1),('2035-04-02',2),('2035-04-03',3),('2035-04-04',4),('2035-04-05',5),('2035-04-06',6),('2035-04-07',7),('2035-04-08',1),('2035-04-09',2),('2035-04-10',3),('2035-04-11',4),('2035-04-12',5),('2035-04-13',6),('2035-04-14',7),('2035-04-15',1),('2035-04-16',2),('2035-04-17',3),('2035-04-18',4),('2035-04-19',5),('2035-04-20',6),('2035-04-21',7),('2035-04-22',1),('2035-04-23',2),('2035-04-24',3),('2035-04-25',4),('2035-04-26',5),('2035-04-27',6),('2035-04-28',7),('2035-04-29',1),('2035-04-30',2),('2035-05-01',3),('2035-05-02',4),('2035-05-03',5),('2035-05-04',6),('2035-05-05',7),('2035-05-06',1),('2035-05-07',2),('2035-05-08',3),('2035-05-09',4),('2035-05-10',5),('2035-05-11',6),('2035-05-12',7),('2035-05-13',1),('2035-05-14',2),('2035-05-15',3),('2035-05-16',4),('2035-05-17',5),('2035-05-18',6),('2035-05-19',7),('2035-05-20',1),('2035-05-21',2),('2035-05-22',3),('2035-05-23',4),('2035-05-24',5),('2035-05-25',6),('2035-05-26',7),('2035-05-27',1),('2035-05-28',2),('2035-05-29',3),('2035-05-30',4),('2035-05-31',5),('2035-06-01',6),('2035-06-02',7),('2035-06-03',1),('2035-06-04',2),('2035-06-05',3),('2035-06-06',4),('2035-06-07',5),('2035-06-08',6),('2035-06-09',7),('2035-06-10',1),('2035-06-11',2),('2035-06-12',3),('2035-06-13',4),('2035-06-14',5),('2035-06-15',6),('2035-06-16',7),('2035-06-17',1),('2035-06-18',2),('2035-06-19',3),('2035-06-20',4),('2035-06-21',5),('2035-06-22',6),('2035-06-23',7),('2035-06-24',1),('2035-06-25',2),('2035-06-26',3),('2035-06-27',4),('2035-06-28',5),('2035-06-29',6),('2035-06-30',7),('2035-07-01',1),('2035-07-02',2),('2035-07-03',3),('2035-07-04',4),('2035-07-05',5),('2035-07-06',6),('2035-07-07',7),('2035-07-08',1),('2035-07-09',2),('2035-07-10',3),('2035-07-11',4),('2035-07-12',5),('2035-07-13',6),('2035-07-14',7),('2035-07-15',1),('2035-07-16',2),('2035-07-17',3),('2035-07-18',4),('2035-07-19',5),('2035-07-20',6),('2035-07-21',7),('2035-07-22',1),('2035-07-23',2),('2035-07-24',3),('2035-07-25',4),('2035-07-26',5),('2035-07-27',6),('2035-07-28',7),('2035-07-29',1),('2035-07-30',2),('2035-07-31',3),('2035-08-01',4),('2035-08-02',5),('2035-08-03',6),('2035-08-04',7),('2035-08-05',1),('2035-08-06',2),('2035-08-07',3),('2035-08-08',4),('2035-08-09',5),('2035-08-10',6),('2035-08-11',7),('2035-08-12',1),('2035-08-13',2),('2035-08-14',3),('2035-08-15',4),('2035-08-16',5),('2035-08-17',6),('2035-08-18',7),('2035-08-19',1),('2035-08-20',2),('2035-08-21',3),('2035-08-22',4),('2035-08-23',5),('2035-08-24',6),('2035-08-25',7),('2035-08-26',1),('2035-08-27',2),('2035-08-28',3),('2035-08-29',4),('2035-08-30',5),('2035-08-31',6),('2035-09-01',7),('2035-09-02',1),('2035-09-03',2),('2035-09-04',3),('2035-09-05',4),('2035-09-06',5),('2035-09-07',6),('2035-09-08',7),('2035-09-09',1),('2035-09-10',2),('2035-09-11',3),('2035-09-12',4),('2035-09-13',5),('2035-09-14',6),('2035-09-15',7),('2035-09-16',1),('2035-09-17',2),('2035-09-18',3),('2035-09-19',4),('2035-09-20',5),('2035-09-21',6),('2035-09-22',7),('2035-09-23',1),('2035-09-24',2),('2035-09-25',3),('2035-09-26',4),('2035-09-27',5),('2035-09-28',6),('2035-09-29',7),('2035-09-30',1),('2035-10-01',2),('2035-10-02',3),('2035-10-03',4),('2035-10-04',5),('2035-10-05',6),('2035-10-06',7),('2035-10-07',1),('2035-10-08',2),('2035-10-09',3),('2035-10-10',4),('2035-10-11',5),('2035-10-12',6),('2035-10-13',7),('2035-10-14',1),('2035-10-15',2),('2035-10-16',3),('2035-10-17',4),('2035-10-18',5),('2035-10-19',6),('2035-10-20',7),('2035-10-21',1),('2035-10-22',2),('2035-10-23',3),('2035-10-24',4),('2035-10-25',5),('2035-10-26',6),('2035-10-27',7),('2035-10-28',1),('2035-10-29',2),('2035-10-30',3),('2035-10-31',4),('2035-11-01',5),('2035-11-02',6),('2035-11-03',7),('2035-11-04',1),('2035-11-05',2),('2035-11-06',3),('2035-11-07',4),('2035-11-08',5),('2035-11-09',6),('2035-11-10',7),('2035-11-11',1),('2035-11-12',2),('2035-11-13',3),('2035-11-14',4),('2035-11-15',5),('2035-11-16',6),('2035-11-17',7),('2035-11-18',1),('2035-11-19',2),('2035-11-20',3),('2035-11-21',4),('2035-11-22',5),('2035-11-23',6),('2035-11-24',7),('2035-11-25',1),('2035-11-26',2),('2035-11-27',3),('2035-11-28',4),('2035-11-29',5),('2035-11-30',6),('2035-12-01',7),('2035-12-02',1),('2035-12-03',2),('2035-12-04',3),('2035-12-05',4),('2035-12-06',5),('2035-12-07',6),('2035-12-08',7),('2035-12-09',1),('2035-12-10',2),('2035-12-11',3),('2035-12-12',4),('2035-12-13',5),('2035-12-14',6),('2035-12-15',7),('2035-12-16',1),('2035-12-17',2),('2035-12-18',3),('2035-12-19',4),('2035-12-20',5),('2035-12-21',6),('2035-12-22',7),('2035-12-23',1),('2035-12-24',2),('2035-12-25',3),('2035-12-26',4),('2035-12-27',5),('2035-12-28',6),('2035-12-29',7),('2035-12-30',1),('2035-12-31',2),('2036-01-01',3),('2036-01-02',4),('2036-01-03',5),('2036-01-04',6),('2036-01-05',7),('2036-01-06',1),('2036-01-07',2),('2036-01-08',3),('2036-01-09',4),('2036-01-10',5),('2036-01-11',6),('2036-01-12',7),('2036-01-13',1),('2036-01-14',2),('2036-01-15',3),('2036-01-16',4),('2036-01-17',5),('2036-01-18',6),('2036-01-19',7),('2036-01-20',1),('2036-01-21',2),('2036-01-22',3),('2036-01-23',4),('2036-01-24',5),('2036-01-25',6),('2036-01-26',7),('2036-01-27',1),('2036-01-28',2),('2036-01-29',3),('2036-01-30',4),('2036-01-31',5),('2036-02-01',6),('2036-02-02',7),('2036-02-03',1),('2036-02-04',2),('2036-02-05',3),('2036-02-06',4),('2036-02-07',5),('2036-02-08',6),('2036-02-09',7),('2036-02-10',1),('2036-02-11',2),('2036-02-12',3),('2036-02-13',4),('2036-02-14',5),('2036-02-15',6),('2036-02-16',7),('2036-02-17',1),('2036-02-18',2),('2036-02-19',3),('2036-02-20',4),('2036-02-21',5),('2036-02-22',6),('2036-02-23',7),('2036-02-24',1),('2036-02-25',2),('2036-02-26',3),('2036-02-27',4),('2036-02-28',5),('2036-02-29',6),('2036-03-01',7),('2036-03-02',1),('2036-03-03',2),('2036-03-04',3),('2036-03-05',4),('2036-03-06',5),('2036-03-07',6),('2036-03-08',7),('2036-03-09',1),('2036-03-10',2),('2036-03-11',3),('2036-03-12',4),('2036-03-13',5),('2036-03-14',6),('2036-03-15',7),('2036-03-16',1),('2036-03-17',2),('2036-03-18',3),('2036-03-19',4),('2036-03-20',5),('2036-03-21',6),('2036-03-22',7),('2036-03-23',1),('2036-03-24',2),('2036-03-25',3),('2036-03-26',4),('2036-03-27',5),('2036-03-28',6),('2036-03-29',7),('2036-03-30',1),('2036-03-31',2),('2036-04-01',3),('2036-04-02',4),('2036-04-03',5),('2036-04-04',6),('2036-04-05',7),('2036-04-06',1),('2036-04-07',2),('2036-04-08',3),('2036-04-09',4),('2036-04-10',5),('2036-04-11',6),('2036-04-12',7),('2036-04-13',1),('2036-04-14',2),('2036-04-15',3),('2036-04-16',4),('2036-04-17',5),('2036-04-18',6),('2036-04-19',7),('2036-04-20',1),('2036-04-21',2),('2036-04-22',3),('2036-04-23',4),('2036-04-24',5),('2036-04-25',6),('2036-04-26',7),('2036-04-27',1),('2036-04-28',2),('2036-04-29',3),('2036-04-30',4),('2036-05-01',5),('2036-05-02',6),('2036-05-03',7),('2036-05-04',1),('2036-05-05',2),('2036-05-06',3),('2036-05-07',4),('2036-05-08',5),('2036-05-09',6),('2036-05-10',7),('2036-05-11',1),('2036-05-12',2),('2036-05-13',3),('2036-05-14',4),('2036-05-15',5),('2036-05-16',6),('2036-05-17',7),('2036-05-18',1),('2036-05-19',2),('2036-05-20',3),('2036-05-21',4),('2036-05-22',5),('2036-05-23',6),('2036-05-24',7),('2036-05-25',1),('2036-05-26',2),('2036-05-27',3),('2036-05-28',4),('2036-05-29',5),('2036-05-30',6),('2036-05-31',7),('2036-06-01',1),('2036-06-02',2),('2036-06-03',3),('2036-06-04',4),('2036-06-05',5),('2036-06-06',6),('2036-06-07',7),('2036-06-08',1),('2036-06-09',2),('2036-06-10',3),('2036-06-11',4),('2036-06-12',5),('2036-06-13',6),('2036-06-14',7),('2036-06-15',1),('2036-06-16',2),('2036-06-17',3),('2036-06-18',4),('2036-06-19',5),('2036-06-20',6),('2036-06-21',7),('2036-06-22',1),('2036-06-23',2),('2036-06-24',3),('2036-06-25',4),('2036-06-26',5),('2036-06-27',6),('2036-06-28',7),('2036-06-29',1),('2036-06-30',2),('2036-07-01',3),('2036-07-02',4),('2036-07-03',5),('2036-07-04',6),('2036-07-05',7),('2036-07-06',1),('2036-07-07',2),('2036-07-08',3),('2036-07-09',4),('2036-07-10',5),('2036-07-11',6),('2036-07-12',7),('2036-07-13',1),('2036-07-14',2),('2036-07-15',3),('2036-07-16',4),('2036-07-17',5),('2036-07-18',6),('2036-07-19',7),('2036-07-20',1),('2036-07-21',2),('2036-07-22',3),('2036-07-23',4),('2036-07-24',5),('2036-07-25',6),('2036-07-26',7),('2036-07-27',1),('2036-07-28',2),('2036-07-29',3),('2036-07-30',4),('2036-07-31',5),('2036-08-01',6),('2036-08-02',7),('2036-08-03',1),('2036-08-04',2),('2036-08-05',3),('2036-08-06',4),('2036-08-07',5),('2036-08-08',6),('2036-08-09',7),('2036-08-10',1),('2036-08-11',2),('2036-08-12',3),('2036-08-13',4),('2036-08-14',5),('2036-08-15',6),('2036-08-16',7),('2036-08-17',1),('2036-08-18',2),('2036-08-19',3),('2036-08-20',4),('2036-08-21',5),('2036-08-22',6),('2036-08-23',7),('2036-08-24',1),('2036-08-25',2),('2036-08-26',3),('2036-08-27',4),('2036-08-28',5),('2036-08-29',6),('2036-08-30',7),('2036-08-31',1),('2036-09-01',2),('2036-09-02',3),('2036-09-03',4),('2036-09-04',5),('2036-09-05',6),('2036-09-06',7),('2036-09-07',1),('2036-09-08',2),('2036-09-09',3),('2036-09-10',4),('2036-09-11',5),('2036-09-12',6),('2036-09-13',7),('2036-09-14',1),('2036-09-15',2),('2036-09-16',3),('2036-09-17',4),('2036-09-18',5),('2036-09-19',6),('2036-09-20',7),('2036-09-21',1),('2036-09-22',2),('2036-09-23',3),('2036-09-24',4),('2036-09-25',5),('2036-09-26',6),('2036-09-27',7),('2036-09-28',1),('2036-09-29',2),('2036-09-30',3),('2036-10-01',4),('2036-10-02',5),('2036-10-03',6),('2036-10-04',7),('2036-10-05',1),('2036-10-06',2),('2036-10-07',3),('2036-10-08',4),('2036-10-09',5),('2036-10-10',6),('2036-10-11',7),('2036-10-12',1),('2036-10-13',2),('2036-10-14',3),('2036-10-15',4),('2036-10-16',5),('2036-10-17',6),('2036-10-18',7),('2036-10-19',1),('2036-10-20',2),('2036-10-21',3),('2036-10-22',4),('2036-10-23',5),('2036-10-24',6),('2036-10-25',7),('2036-10-26',1),('2036-10-27',2),('2036-10-28',3),('2036-10-29',4),('2036-10-30',5),('2036-10-31',6),('2036-11-01',7),('2036-11-02',1),('2036-11-03',2),('2036-11-04',3),('2036-11-05',4),('2036-11-06',5),('2036-11-07',6),('2036-11-08',7),('2036-11-09',1),('2036-11-10',2),('2036-11-11',3),('2036-11-12',4),('2036-11-13',5),('2036-11-14',6),('2036-11-15',7),('2036-11-16',1),('2036-11-17',2),('2036-11-18',3),('2036-11-19',4),('2036-11-20',5),('2036-11-21',6),('2036-11-22',7),('2036-11-23',1),('2036-11-24',2),('2036-11-25',3),('2036-11-26',4),('2036-11-27',5),('2036-11-28',6),('2036-11-29',7),('2036-11-30',1),('2036-12-01',2),('2036-12-02',3),('2036-12-03',4),('2036-12-04',5),('2036-12-05',6),('2036-12-06',7),('2036-12-07',1),('2036-12-08',2),('2036-12-09',3),('2036-12-10',4),('2036-12-11',5),('2036-12-12',6),('2036-12-13',7),('2036-12-14',1),('2036-12-15',2),('2036-12-16',3),('2036-12-17',4),('2036-12-18',5),('2036-12-19',6),('2036-12-20',7),('2036-12-21',1),('2036-12-22',2),('2036-12-23',3),('2036-12-24',4),('2036-12-25',5),('2036-12-26',6),('2036-12-27',7),('2036-12-28',1),('2036-12-29',2),('2036-12-30',3),('2036-12-31',4),('2037-01-01',5),('2037-01-02',6),('2037-01-03',7),('2037-01-04',1),('2037-01-05',2),('2037-01-06',3),('2037-01-07',4),('2037-01-08',5),('2037-01-09',6),('2037-01-10',7),('2037-01-11',1),('2037-01-12',2),('2037-01-13',3),('2037-01-14',4),('2037-01-15',5),('2037-01-16',6),('2037-01-17',7),('2037-01-18',1),('2037-01-19',2),('2037-01-20',3),('2037-01-21',4),('2037-01-22',5),('2037-01-23',6),('2037-01-24',7),('2037-01-25',1),('2037-01-26',2),('2037-01-27',3),('2037-01-28',4),('2037-01-29',5),('2037-01-30',6),('2037-01-31',7),('2037-02-01',1),('2037-02-02',2),('2037-02-03',3),('2037-02-04',4),('2037-02-05',5),('2037-02-06',6),('2037-02-07',7),('2037-02-08',1),('2037-02-09',2),('2037-02-10',3),('2037-02-11',4),('2037-02-12',5),('2037-02-13',6),('2037-02-14',7),('2037-02-15',1),('2037-02-16',2),('2037-02-17',3),('2037-02-18',4),('2037-02-19',5),('2037-02-20',6),('2037-02-21',7),('2037-02-22',1),('2037-02-23',2),('2037-02-24',3),('2037-02-25',4),('2037-02-26',5),('2037-02-27',6),('2037-02-28',7),('2037-03-01',1),('2037-03-02',2),('2037-03-03',3),('2037-03-04',4),('2037-03-05',5),('2037-03-06',6),('2037-03-07',7),('2037-03-08',1),('2037-03-09',2),('2037-03-10',3),('2037-03-11',4),('2037-03-12',5),('2037-03-13',6),('2037-03-14',7),('2037-03-15',1),('2037-03-16',2),('2037-03-17',3),('2037-03-18',4),('2037-03-19',5),('2037-03-20',6),('2037-03-21',7),('2037-03-22',1),('2037-03-23',2),('2037-03-24',3),('2037-03-25',4),('2037-03-26',5),('2037-03-27',6),('2037-03-28',7),('2037-03-29',1),('2037-03-30',2),('2037-03-31',3),('2037-04-01',4),('2037-04-02',5),('2037-04-03',6),('2037-04-04',7),('2037-04-05',1),('2037-04-06',2),('2037-04-07',3),('2037-04-08',4),('2037-04-09',5),('2037-04-10',6),('2037-04-11',7),('2037-04-12',1),('2037-04-13',2),('2037-04-14',3),('2037-04-15',4),('2037-04-16',5),('2037-04-17',6),('2037-04-18',7),('2037-04-19',1),('2037-04-20',2),('2037-04-21',3),('2037-04-22',4),('2037-04-23',5),('2037-04-24',6),('2037-04-25',7),('2037-04-26',1),('2037-04-27',2),('2037-04-28',3),('2037-04-29',4),('2037-04-30',5),('2037-05-01',6),('2037-05-02',7),('2037-05-03',1),('2037-05-04',2),('2037-05-05',3),('2037-05-06',4),('2037-05-07',5),('2037-05-08',6),('2037-05-09',7),('2037-05-10',1),('2037-05-11',2),('2037-05-12',3),('2037-05-13',4),('2037-05-14',5),('2037-05-15',6),('2037-05-16',7),('2037-05-17',1),('2037-05-18',2),('2037-05-19',3),('2037-05-20',4),('2037-05-21',5),('2037-05-22',6),('2037-05-23',7),('2037-05-24',1),('2037-05-25',2),('2037-05-26',3),('2037-05-27',4),('2037-05-28',5),('2037-05-29',6),('2037-05-30',7),('2037-05-31',1),('2037-06-01',2),('2037-06-02',3),('2037-06-03',4),('2037-06-04',5),('2037-06-05',6),('2037-06-06',7),('2037-06-07',1),('2037-06-08',2),('2037-06-09',3),('2037-06-10',4),('2037-06-11',5),('2037-06-12',6),('2037-06-13',7),('2037-06-14',1),('2037-06-15',2),('2037-06-16',3),('2037-06-17',4),('2037-06-18',5),('2037-06-19',6),('2037-06-20',7),('2037-06-21',1),('2037-06-22',2),('2037-06-23',3),('2037-06-24',4),('2037-06-25',5),('2037-06-26',6),('2037-06-27',7),('2037-06-28',1),('2037-06-29',2),('2037-06-30',3),('2037-07-01',4),('2037-07-02',5),('2037-07-03',6),('2037-07-04',7),('2037-07-05',1),('2037-07-06',2),('2037-07-07',3),('2037-07-08',4),('2037-07-09',5),('2037-07-10',6),('2037-07-11',7),('2037-07-12',1),('2037-07-13',2),('2037-07-14',3),('2037-07-15',4),('2037-07-16',5),('2037-07-17',6),('2037-07-18',7),('2037-07-19',1),('2037-07-20',2),('2037-07-21',3),('2037-07-22',4),('2037-07-23',5),('2037-07-24',6),('2037-07-25',7),('2037-07-26',1),('2037-07-27',2),('2037-07-28',3),('2037-07-29',4),('2037-07-30',5),('2037-07-31',6),('2037-08-01',7),('2037-08-02',1),('2037-08-03',2),('2037-08-04',3),('2037-08-05',4),('2037-08-06',5),('2037-08-07',6),('2037-08-08',7),('2037-08-09',1),('2037-08-10',2),('2037-08-11',3),('2037-08-12',4),('2037-08-13',5),('2037-08-14',6),('2037-08-15',7),('2037-08-16',1),('2037-08-17',2),('2037-08-18',3),('2037-08-19',4),('2037-08-20',5),('2037-08-21',6),('2037-08-22',7),('2037-08-23',1),('2037-08-24',2),('2037-08-25',3),('2037-08-26',4),('2037-08-27',5),('2037-08-28',6),('2037-08-29',7),('2037-08-30',1),('2037-08-31',2),('2037-09-01',3),('2037-09-02',4),('2037-09-03',5),('2037-09-04',6),('2037-09-05',7),('2037-09-06',1),('2037-09-07',2),('2037-09-08',3),('2037-09-09',4),('2037-09-10',5),('2037-09-11',6),('2037-09-12',7),('2037-09-13',1),('2037-09-14',2),('2037-09-15',3),('2037-09-16',4),('2037-09-17',5),('2037-09-18',6),('2037-09-19',7),('2037-09-20',1),('2037-09-21',2),('2037-09-22',3),('2037-09-23',4),('2037-09-24',5),('2037-09-25',6),('2037-09-26',7),('2037-09-27',1),('2037-09-28',2),('2037-09-29',3),('2037-09-30',4),('2037-10-01',5),('2037-10-02',6),('2037-10-03',7),('2037-10-04',1),('2037-10-05',2),('2037-10-06',3),('2037-10-07',4),('2037-10-08',5),('2037-10-09',6),('2037-10-10',7),('2037-10-11',1),('2037-10-12',2),('2037-10-13',3),('2037-10-14',4),('2037-10-15',5),('2037-10-16',6),('2037-10-17',7),('2037-10-18',1),('2037-10-19',2),('2037-10-20',3),('2037-10-21',4),('2037-10-22',5),('2037-10-23',6),('2037-10-24',7),('2037-10-25',1),('2037-10-26',2),('2037-10-27',3),('2037-10-28',4),('2037-10-29',5),('2037-10-30',6),('2037-10-31',7),('2037-11-01',1),('2037-11-02',2),('2037-11-03',3),('2037-11-04',4),('2037-11-05',5),('2037-11-06',6),('2037-11-07',7),('2037-11-08',1),('2037-11-09',2),('2037-11-10',3),('2037-11-11',4),('2037-11-12',5),('2037-11-13',6),('2037-11-14',7),('2037-11-15',1),('2037-11-16',2),('2037-11-17',3),('2037-11-18',4),('2037-11-19',5),('2037-11-20',6),('2037-11-21',7),('2037-11-22',1),('2037-11-23',2),('2037-11-24',3),('2037-11-25',4),('2037-11-26',5),('2037-11-27',6),('2037-11-28',7),('2037-11-29',1),('2037-11-30',2),('2037-12-01',3),('2037-12-02',4),('2037-12-03',5),('2037-12-04',6),('2037-12-05',7),('2037-12-06',1),('2037-12-07',2),('2037-12-08',3),('2037-12-09',4),('2037-12-10',5),('2037-12-11',6),('2037-12-12',7),('2037-12-13',1),('2037-12-14',2),('2037-12-15',3),('2037-12-16',4),('2037-12-17',5),('2037-12-18',6),('2037-12-19',7),('2037-12-20',1),('2037-12-21',2),('2037-12-22',3),('2037-12-23',4),('2037-12-24',5),('2037-12-25',6),('2037-12-26',7),('2037-12-27',1),('2037-12-28',2),('2037-12-29',3),('2037-12-30',4),('2037-12-31',5),('2038-01-01',6),('2038-01-02',7),('2038-01-03',1),('2038-01-04',2),('2038-01-05',3),('2038-01-06',4),('2038-01-07',5),('2038-01-08',6),('2038-01-09',7),('2038-01-10',1),('2038-01-11',2),('2038-01-12',3),('2038-01-13',4),('2038-01-14',5),('2038-01-15',6),('2038-01-16',7),('2038-01-17',1),('2038-01-18',2),('2038-01-19',3),('2038-01-20',4),('2038-01-21',5),('2038-01-22',6),('2038-01-23',7),('2038-01-24',1),('2038-01-25',2),('2038-01-26',3),('2038-01-27',4),('2038-01-28',5),('2038-01-29',6),('2038-01-30',7),('2038-01-31',1),('2038-02-01',2),('2038-02-02',3),('2038-02-03',4),('2038-02-04',5),('2038-02-05',6),('2038-02-06',7),('2038-02-07',1),('2038-02-08',2),('2038-02-09',3),('2038-02-10',4),('2038-02-11',5),('2038-02-12',6),('2038-02-13',7),('2038-02-14',1),('2038-02-15',2),('2038-02-16',3),('2038-02-17',4),('2038-02-18',5),('2038-02-19',6),('2038-02-20',7),('2038-02-21',1),('2038-02-22',2),('2038-02-23',3),('2038-02-24',4),('2038-02-25',5),('2038-02-26',6),('2038-02-27',7),('2038-02-28',1),('2038-03-01',2),('2038-03-02',3),('2038-03-03',4),('2038-03-04',5),('2038-03-05',6),('2038-03-06',7),('2038-03-07',1),('2038-03-08',2),('2038-03-09',3),('2038-03-10',4),('2038-03-11',5),('2038-03-12',6),('2038-03-13',7),('2038-03-14',1),('2038-03-15',2),('2038-03-16',3),('2038-03-17',4),('2038-03-18',5),('2038-03-19',6),('2038-03-20',7),('2038-03-21',1),('2038-03-22',2),('2038-03-23',3),('2038-03-24',4),('2038-03-25',5),('2038-03-26',6),('2038-03-27',7),('2038-03-28',1),('2038-03-29',2),('2038-03-30',3),('2038-03-31',4),('2038-04-01',5),('2038-04-02',6),('2038-04-03',7),('2038-04-04',1),('2038-04-05',2),('2038-04-06',3),('2038-04-07',4),('2038-04-08',5),('2038-04-09',6),('2038-04-10',7),('2038-04-11',1),('2038-04-12',2),('2038-04-13',3),('2038-04-14',4),('2038-04-15',5),('2038-04-16',6),('2038-04-17',7),('2038-04-18',1),('2038-04-19',2),('2038-04-20',3),('2038-04-21',4),('2038-04-22',5),('2038-04-23',6),('2038-04-24',7),('2038-04-25',1),('2038-04-26',2),('2038-04-27',3),('2038-04-28',4),('2038-04-29',5),('2038-04-30',6),('2038-05-01',7),('2038-05-02',1),('2038-05-03',2),('2038-05-04',3),('2038-05-05',4),('2038-05-06',5),('2038-05-07',6),('2038-05-08',7),('2038-05-09',1),('2038-05-10',2),('2038-05-11',3),('2038-05-12',4),('2038-05-13',5),('2038-05-14',6),('2038-05-15',7),('2038-05-16',1),('2038-05-17',2),('2038-05-18',3),('2038-05-19',4),('2038-05-20',5),('2038-05-21',6),('2038-05-22',7),('2038-05-23',1),('2038-05-24',2),('2038-05-25',3),('2038-05-26',4),('2038-05-27',5),('2038-05-28',6),('2038-05-29',7),('2038-05-30',1),('2038-05-31',2),('2038-06-01',3),('2038-06-02',4),('2038-06-03',5),('2038-06-04',6),('2038-06-05',7),('2038-06-06',1),('2038-06-07',2),('2038-06-08',3),('2038-06-09',4),('2038-06-10',5),('2038-06-11',6),('2038-06-12',7),('2038-06-13',1),('2038-06-14',2),('2038-06-15',3),('2038-06-16',4),('2038-06-17',5),('2038-06-18',6),('2038-06-19',7),('2038-06-20',1),('2038-06-21',2),('2038-06-22',3),('2038-06-23',4),('2038-06-24',5),('2038-06-25',6),('2038-06-26',7),('2038-06-27',1),('2038-06-28',2),('2038-06-29',3),('2038-06-30',4),('2038-07-01',5),('2038-07-02',6),('2038-07-03',7),('2038-07-04',1),('2038-07-05',2),('2038-07-06',3),('2038-07-07',4),('2038-07-08',5),('2038-07-09',6),('2038-07-10',7),('2038-07-11',1),('2038-07-12',2),('2038-07-13',3),('2038-07-14',4),('2038-07-15',5),('2038-07-16',6),('2038-07-17',7),('2038-07-18',1),('2038-07-19',2),('2038-07-20',3),('2038-07-21',4),('2038-07-22',5),('2038-07-23',6),('2038-07-24',7),('2038-07-25',1),('2038-07-26',2),('2038-07-27',3),('2038-07-28',4),('2038-07-29',5),('2038-07-30',6),('2038-07-31',7),('2038-08-01',1),('2038-08-02',2),('2038-08-03',3),('2038-08-04',4),('2038-08-05',5),('2038-08-06',6),('2038-08-07',7),('2038-08-08',1),('2038-08-09',2),('2038-08-10',3),('2038-08-11',4),('2038-08-12',5),('2038-08-13',6),('2038-08-14',7),('2038-08-15',1),('2038-08-16',2),('2038-08-17',3),('2038-08-18',4),('2038-08-19',5),('2038-08-20',6),('2038-08-21',7),('2038-08-22',1),('2038-08-23',2),('2038-08-24',3),('2038-08-25',4),('2038-08-26',5),('2038-08-27',6),('2038-08-28',7),('2038-08-29',1),('2038-08-30',2),('2038-08-31',3),('2038-09-01',4),('2038-09-02',5),('2038-09-03',6),('2038-09-04',7),('2038-09-05',1),('2038-09-06',2),('2038-09-07',3),('2038-09-08',4),('2038-09-09',5),('2038-09-10',6),('2038-09-11',7),('2038-09-12',1),('2038-09-13',2),('2038-09-14',3),('2038-09-15',4),('2038-09-16',5),('2038-09-17',6),('2038-09-18',7),('2038-09-19',1),('2038-09-20',2),('2038-09-21',3),('2038-09-22',4),('2038-09-23',5),('2038-09-24',6),('2038-09-25',7),('2038-09-26',1),('2038-09-27',2),('2038-09-28',3),('2038-09-29',4),('2038-09-30',5),('2038-10-01',6),('2038-10-02',7),('2038-10-03',1),('2038-10-04',2),('2038-10-05',3),('2038-10-06',4),('2038-10-07',5),('2038-10-08',6),('2038-10-09',7),('2038-10-10',1),('2038-10-11',2),('2038-10-12',3),('2038-10-13',4),('2038-10-14',5),('2038-10-15',6),('2038-10-16',7),('2038-10-17',1),('2038-10-18',2),('2038-10-19',3),('2038-10-20',4),('2038-10-21',5),('2038-10-22',6),('2038-10-23',7),('2038-10-24',1),('2038-10-25',2),('2038-10-26',3),('2038-10-27',4),('2038-10-28',5),('2038-10-29',6),('2038-10-30',7),('2038-10-31',1),('2038-11-01',2),('2038-11-02',3),('2038-11-03',4),('2038-11-04',5),('2038-11-05',6),('2038-11-06',7),('2038-11-07',1),('2038-11-08',2),('2038-11-09',3),('2038-11-10',4),('2038-11-11',5),('2038-11-12',6),('2038-11-13',7),('2038-11-14',1),('2038-11-15',2),('2038-11-16',3),('2038-11-17',4),('2038-11-18',5),('2038-11-19',6),('2038-11-20',7),('2038-11-21',1),('2038-11-22',2),('2038-11-23',3),('2038-11-24',4),('2038-11-25',5),('2038-11-26',6),('2038-11-27',7),('2038-11-28',1),('2038-11-29',2),('2038-11-30',3),('2038-12-01',4),('2038-12-02',5),('2038-12-03',6),('2038-12-04',7),('2038-12-05',1),('2038-12-06',2),('2038-12-07',3),('2038-12-08',4),('2038-12-09',5),('2038-12-10',6),('2038-12-11',7),('2038-12-12',1),('2038-12-13',2),('2038-12-14',3),('2038-12-15',4),('2038-12-16',5),('2038-12-17',6),('2038-12-18',7),('2038-12-19',1),('2038-12-20',2),('2038-12-21',3),('2038-12-22',4),('2038-12-23',5),('2038-12-24',6),('2038-12-25',7),('2038-12-26',1),('2038-12-27',2),('2038-12-28',3),('2038-12-29',4),('2038-12-30',5),('2038-12-31',6),('2039-01-01',7),('2039-01-02',1),('2039-01-03',2),('2039-01-04',3),('2039-01-05',4),('2039-01-06',5),('2039-01-07',6),('2039-01-08',7),('2039-01-09',1),('2039-01-10',2),('2039-01-11',3),('2039-01-12',4),('2039-01-13',5),('2039-01-14',6),('2039-01-15',7),('2039-01-16',1),('2039-01-17',2),('2039-01-18',3),('2039-01-19',4),('2039-01-20',5),('2039-01-21',6),('2039-01-22',7),('2039-01-23',1),('2039-01-24',2),('2039-01-25',3),('2039-01-26',4),('2039-01-27',5),('2039-01-28',6),('2039-01-29',7),('2039-01-30',1),('2039-01-31',2),('2039-02-01',3),('2039-02-02',4),('2039-02-03',5),('2039-02-04',6),('2039-02-05',7),('2039-02-06',1),('2039-02-07',2),('2039-02-08',3),('2039-02-09',4),('2039-02-10',5),('2039-02-11',6),('2039-02-12',7),('2039-02-13',1),('2039-02-14',2),('2039-02-15',3),('2039-02-16',4),('2039-02-17',5),('2039-02-18',6),('2039-02-19',7),('2039-02-20',1),('2039-02-21',2),('2039-02-22',3),('2039-02-23',4),('2039-02-24',5),('2039-02-25',6),('2039-02-26',7),('2039-02-27',1),('2039-02-28',2),('2039-03-01',3),('2039-03-02',4),('2039-03-03',5),('2039-03-04',6),('2039-03-05',7),('2039-03-06',1),('2039-03-07',2),('2039-03-08',3),('2039-03-09',4),('2039-03-10',5),('2039-03-11',6),('2039-03-12',7),('2039-03-13',1),('2039-03-14',2),('2039-03-15',3),('2039-03-16',4),('2039-03-17',5),('2039-03-18',6),('2039-03-19',7),('2039-03-20',1),('2039-03-21',2),('2039-03-22',3),('2039-03-23',4),('2039-03-24',5),('2039-03-25',6),('2039-03-26',7),('2039-03-27',1),('2039-03-28',2),('2039-03-29',3),('2039-03-30',4),('2039-03-31',5),('2039-04-01',6),('2039-04-02',7),('2039-04-03',1),('2039-04-04',2),('2039-04-05',3),('2039-04-06',4),('2039-04-07',5),('2039-04-08',6),('2039-04-09',7),('2039-04-10',1),('2039-04-11',2),('2039-04-12',3),('2039-04-13',4),('2039-04-14',5),('2039-04-15',6),('2039-04-16',7),('2039-04-17',1),('2039-04-18',2),('2039-04-19',3),('2039-04-20',4),('2039-04-21',5),('2039-04-22',6),('2039-04-23',7),('2039-04-24',1),('2039-04-25',2),('2039-04-26',3),('2039-04-27',4),('2039-04-28',5),('2039-04-29',6),('2039-04-30',7),('2039-05-01',1),('2039-05-02',2),('2039-05-03',3),('2039-05-04',4),('2039-05-05',5),('2039-05-06',6),('2039-05-07',7),('2039-05-08',1),('2039-05-09',2),('2039-05-10',3),('2039-05-11',4),('2039-05-12',5),('2039-05-13',6),('2039-05-14',7),('2039-05-15',1),('2039-05-16',2),('2039-05-17',3),('2039-05-18',4),('2039-05-19',5),('2039-05-20',6),('2039-05-21',7),('2039-05-22',1),('2039-05-23',2),('2039-05-24',3),('2039-05-25',4),('2039-05-26',5),('2039-05-27',6),('2039-05-28',7),('2039-05-29',1),('2039-05-30',2),('2039-05-31',3),('2039-06-01',4),('2039-06-02',5),('2039-06-03',6),('2039-06-04',7),('2039-06-05',1),('2039-06-06',2),('2039-06-07',3),('2039-06-08',4),('2039-06-09',5),('2039-06-10',6),('2039-06-11',7),('2039-06-12',1),('2039-06-13',2),('2039-06-14',3),('2039-06-15',4),('2039-06-16',5),('2039-06-17',6),('2039-06-18',7),('2039-06-19',1),('2039-06-20',2),('2039-06-21',3),('2039-06-22',4),('2039-06-23',5),('2039-06-24',6),('2039-06-25',7),('2039-06-26',1),('2039-06-27',2),('2039-06-28',3),('2039-06-29',4),('2039-06-30',5),('2039-07-01',6),('2039-07-02',7),('2039-07-03',1),('2039-07-04',2),('2039-07-05',3),('2039-07-06',4),('2039-07-07',5),('2039-07-08',6),('2039-07-09',7),('2039-07-10',1),('2039-07-11',2),('2039-07-12',3),('2039-07-13',4),('2039-07-14',5),('2039-07-15',6),('2039-07-16',7),('2039-07-17',1),('2039-07-18',2),('2039-07-19',3),('2039-07-20',4),('2039-07-21',5),('2039-07-22',6),('2039-07-23',7),('2039-07-24',1),('2039-07-25',2),('2039-07-26',3),('2039-07-27',4),('2039-07-28',5),('2039-07-29',6),('2039-07-30',7),('2039-07-31',1),('2039-08-01',2),('2039-08-02',3),('2039-08-03',4),('2039-08-04',5),('2039-08-05',6),('2039-08-06',7),('2039-08-07',1),('2039-08-08',2),('2039-08-09',3),('2039-08-10',4),('2039-08-11',5),('2039-08-12',6),('2039-08-13',7),('2039-08-14',1),('2039-08-15',2),('2039-08-16',3),('2039-08-17',4),('2039-08-18',5),('2039-08-19',6),('2039-08-20',7),('2039-08-21',1),('2039-08-22',2),('2039-08-23',3),('2039-08-24',4),('2039-08-25',5),('2039-08-26',6),('2039-08-27',7),('2039-08-28',1),('2039-08-29',2),('2039-08-30',3),('2039-08-31',4),('2039-09-01',5),('2039-09-02',6),('2039-09-03',7),('2039-09-04',1),('2039-09-05',2),('2039-09-06',3),('2039-09-07',4),('2039-09-08',5),('2039-09-09',6),('2039-09-10',7),('2039-09-11',1),('2039-09-12',2),('2039-09-13',3),('2039-09-14',4),('2039-09-15',5),('2039-09-16',6),('2039-09-17',7),('2039-09-18',1),('2039-09-19',2),('2039-09-20',3),('2039-09-21',4),('2039-09-22',5),('2039-09-23',6),('2039-09-24',7),('2039-09-25',1),('2039-09-26',2),('2039-09-27',3),('2039-09-28',4),('2039-09-29',5),('2039-09-30',6),('2039-10-01',7),('2039-10-02',1),('2039-10-03',2),('2039-10-04',3),('2039-10-05',4),('2039-10-06',5),('2039-10-07',6),('2039-10-08',7),('2039-10-09',1),('2039-10-10',2),('2039-10-11',3),('2039-10-12',4),('2039-10-13',5),('2039-10-14',6),('2039-10-15',7),('2039-10-16',1),('2039-10-17',2),('2039-10-18',3),('2039-10-19',4),('2039-10-20',5),('2039-10-21',6),('2039-10-22',7),('2039-10-23',1),('2039-10-24',2),('2039-10-25',3),('2039-10-26',4),('2039-10-27',5),('2039-10-28',6),('2039-10-29',7),('2039-10-30',1),('2039-10-31',2),('2039-11-01',3),('2039-11-02',4),('2039-11-03',5),('2039-11-04',6),('2039-11-05',7),('2039-11-06',1),('2039-11-07',2),('2039-11-08',3),('2039-11-09',4),('2039-11-10',5),('2039-11-11',6),('2039-11-12',7),('2039-11-13',1),('2039-11-14',2),('2039-11-15',3),('2039-11-16',4),('2039-11-17',5),('2039-11-18',6),('2039-11-19',7),('2039-11-20',1),('2039-11-21',2),('2039-11-22',3),('2039-11-23',4),('2039-11-24',5),('2039-11-25',6),('2039-11-26',7),('2039-11-27',1),('2039-11-28',2),('2039-11-29',3),('2039-11-30',4),('2039-12-01',5),('2039-12-02',6),('2039-12-03',7),('2039-12-04',1),('2039-12-05',2),('2039-12-06',3),('2039-12-07',4),('2039-12-08',5),('2039-12-09',6),('2039-12-10',7),('2039-12-11',1),('2039-12-12',2),('2039-12-13',3),('2039-12-14',4),('2039-12-15',5),('2039-12-16',6),('2039-12-17',7),('2039-12-18',1),('2039-12-19',2),('2039-12-20',3),('2039-12-21',4),('2039-12-22',5),('2039-12-23',6),('2039-12-24',7),('2039-12-25',1),('2039-12-26',2),('2039-12-27',3),('2039-12-28',4),('2039-12-29',5),('2039-12-30',6),('2039-12-31',7),('2040-01-01',1),('2040-01-02',2),('2040-01-03',3),('2040-01-04',4),('2040-01-05',5),('2040-01-06',6),('2040-01-07',7),('2040-01-08',1),('2040-01-09',2),('2040-01-10',3),('2040-01-11',4),('2040-01-12',5),('2040-01-13',6),('2040-01-14',7);

/*Data for the table `int_eventos` */

insert  into `int_eventos`(`id`,`planta`,`nombre`,`monitor`,`alerta`,`revision`,`revisado`,`prioridad`,`estatus`,`creacion`,`modificacion`,`creado`,`modificado`) values (2,1,'Tiempo de tránsito excedido','S',102,2,'2020-04-22 02:51:31',NULL,'A','2020-04-01 00:00:00','2020-04-01 00:00:00',1,1),(3,1,'Tiempo de descarga excedido','S',103,2,'2020-04-22 02:51:31',NULL,'A','2020-04-01 00:00:00','2020-04-01 00:00:00',1,1),(1,1,'Tiempo de espera excedido','S',101,2,'2020-04-22 02:51:31',NULL,'A','2020-04-01 00:00:00','2020-04-01 00:00:00',0,0),(4,1,'Tiempo de preasignación excedido','S',104,2,'2020-04-22 02:51:31',NULL,'A','2020-04-01 00:00:00','2020-04-01 00:00:00',0,0);

/*Data for the table `int_listados` */

insert  into `int_listados`(`id`,`nombre`,`datos`,`grafica`,`orden`,`file_name`,`estatus`) values (8,'Tactos por semana','S','S',8,'tactos_semana','A'),(9,'Tactos por mes','S','S',9,'tactos_mes','A'),(7,'Tactos por día','S','S',7,'tactos_dia','A'),(10,'Historico de tactos','S','N',10,'historico','A'),(108,'Efiiencia por mes','S','S',0,'eficiencia_mes','A'),(107,'Efiiencia por semana','S','S',0,'eficiencia_semana','A'),(106,'Efiiencia por dia','S','S',0,'eficiencia_dia','A'),(105,'Efiiencia por area','S','S',0,'eficiencia_area','A'),(104,'Efiiencia por destino','S','S',0,'eficiencia_destino','A'),(103,'Eficiencia por tipo de carga','S','S',0,'eficiencia_tipo_carga','A'),(102,'Eficiencia por tipo de vehículo','S','S',0,'eficiencia_tipo_vehiculo','A'),(101,'Eficiencia por transporte','S','S',0,'eficiencia_transporte','A'),(3,'Tactos por chofer','S','S',3,'tactos_chofer','A'),(5,'Tactos por tipo de carga','S','S',5,'tactos_tipo_carga','A'),(6,'Tactos por área','S','S',6,'tactos_area','A'),(4,'Tactos por tipo de vehículo','S','S',4,'tactos_tipo_vehiculo','A'),(2,'Tactos por vehículo','S','S',2,'tactos_vehiculo','A'),(1,'Tactos por transporte','S','S',1,'tactos_transporte','A');

/*Data for the table `int_opciones` */

insert  into `int_opciones`(`id`,`rol`,`nombre`,`orden`,`visualizar`,`acciones`,`opcion_app`,`url`,`estatus`) values (560,'A','Visualizar alarmas',20,'S','SSSSS',23,'/exportar','A'),(550,'A','Terminar alertas',30,'S','SSSSS',9001,'N/A','A'),(30,'O','Visualizar los dispositivos en FINALIZADO',96,'S','SSSSS',66,'/catalogos','A'),(60,'O','Visualizar los dispositivos DESCARGANDO',95,'S','SSSSS',60,'/catalogos','A'),(110,'G','Mantenimiento de Transporte',60,'S','SSSSS',30,'/catalogos','A'),(220,'G','Mantenimiento de Vehículos',70,'S','SSSSS',31,'/catalogos','A'),(200,'G','Mantenimiento de Choferes',80,'S','SSSSS',32,'/catalogos','A'),(180,'G','Mantenimiento de Destinos',81,'S','SSSSS',33,'/catalogos','A'),(170,'G','Mantenimiento de dispositivos',90,'S','SSSSS',64,'/catalogos','A'),(50,'O','Visualizar los dispositivos DISPONIBLES',91,'S','SSSSS',61,'/catalogos','A'),(20,'G','Visualizar los dispositivos en TRÁNSITO',94,'S','SSSSS',65,'/catalogos','A'),(10,'O','Visualizar TODOS LOS DISPOSITIVOS',97,'S','SSSSS',62,'/catalogos','A'),(40,'O','Visualizar los dispositivos EN ESPERA',92,'S','SSSSS',63,'/catalogos','A'),(150,'G','Mantenimiento de Tablas generales',100,'S','SSSSS',34,'/catalogos','A'),(120,'G','Mantenimiento de Traductor',120,'S','SSSSS',39,'/catalogos','A'),(190,'G','Mantenimiento de Correos/Reportes',130,'S','SSSSS',36,'/catalogos','A'),(160,'G','Mantenimiento de Recipientes',140,'S','SSSSS',35,'/catalogos','A'),(210,'G','Mantenimiento de Alertas',150,'S','SSSSS',37,'/catalogos','A'),(510,'A','Gestión de usuarios',160,'S','SSSSS',41,'/catalogos','A'),(530,'A','Gestión de parámetros',170,'S','SSSSS',42,'/parametros','A'),(840,'*','MULTI-VISOR',180,'S','SSSSS',20,'/visor','A'),(820,'*','Consulta de gráficos',190,'S','SSSSS',21,'/graficas','A'),(810,'G','Descargar datos del sistema',200,'S','SSSSS',22,'/exportar','A'),(520,'A','Gestión de políticas',165,'S','SSSSS',43,'/catalogos','A'),(540,'A','Equipamiento',180,'S','SSSSS',44,'/parametros','A'),(140,'G','Mantenimiento de tiempos por descarga',171,'S','SSSSS',46,'/catalogos','A'),(130,'G','Mantenimiento de Tiempos por ruta',127,'S','SSSSS',40,'/catalogos','A');

/*Data for the table `objetos` */

insert  into `objetos`(`mapa_id`,`id`,`descripcion`,`ultima_actualizacion`) values (83,'1','Objeto 1','2019-09-22 00:57:45'),(83,'2','Objeto 2','2019-09-22 00:57:45'),(83,'3','Objeto 3','2019-09-22 00:57:45'),(84,'2','Objeto 2','2019-09-22 00:57:46'),(85,'2','Objeto 2','2019-09-22 01:57:42'),(85,'3','Objeto 3','2019-09-22 01:57:42'),(85,'1','Objeto 1','2019-09-22 01:57:42'),(86,'2','Objeto 2','2019-09-22 01:57:43'),(87,'1','Objeto 1','2019-11-04 07:05:24'),(87,'2','Objeto 2','2019-11-04 07:05:24'),(87,'3','Objeto 3','2019-11-04 07:05:24'),(87,'4','Objeto 4','2019-11-04 07:05:24'),(87,'5','Objeto 5','2019-11-04 07:05:24'),(87,'6','Objeto 6','2019-11-04 07:05:24'),(89,'1','Objeto 1','2019-11-04 07:05:27'),(89,'2','Objeto 2','2019-11-04 07:05:27'),(89,'4','Objeto 4','2019-11-04 07:05:27'),(89,'5','Objeto 5','2019-11-04 07:05:27'),(89,'3','Objeto 3','2019-11-04 07:05:27'),(89,'6','Objeto 6','2019-11-04 07:05:27'),(90,'1','Objeto 1','2019-11-04 07:07:36'),(90,'2','Objeto 2','2019-11-04 07:07:36'),(90,'4','Objeto 4','2019-11-04 07:07:36'),(90,'5','Objeto 5','2019-11-04 07:07:36'),(90,'3','Objeto 3','2019-11-04 07:07:36'),(90,'6','Objeto 6','2019-11-04 07:07:36'),(91,'1','Objeto 1','2019-11-04 07:07:40'),(91,'2','Objeto 2','2019-11-04 07:07:40'),(91,'4','Objeto 4','2019-11-04 07:07:40'),(91,'5','Objeto 5','2019-11-04 07:07:40'),(91,'3','Objeto 3','2019-11-04 07:07:40'),(91,'6','Objeto 6','2019-11-04 07:07:40'),(92,'1','Objeto 1','2019-11-04 10:59:41'),(92,'2','Objeto 2','2019-11-04 10:59:41'),(92,'4','Objeto 4','2019-11-04 10:59:41'),(92,'5','Objeto 5','2019-11-04 10:59:41'),(92,'3','Objeto 3','2019-11-04 10:59:41'),(92,'6','Objeto 6','2019-11-04 10:59:41'),(93,'1','Objeto 1','2019-11-04 10:59:45'),(93,'2','Objeto 2','2019-11-04 10:59:45'),(93,'4','Objeto 4','2019-11-04 10:59:45'),(93,'5','Objeto 5','2019-11-04 10:59:45'),(93,'3','Objeto 3','2019-11-04 10:59:45'),(93,'6','Objeto 6','2019-11-04 10:59:45'),(94,'1','Objeto 1','2019-11-04 10:54:05'),(94,'2','Objeto 2','2019-11-04 10:54:05'),(94,'4','Objeto 4','2019-11-04 10:54:05'),(94,'5','Objeto 5','2019-11-04 10:54:05'),(94,'3','Objeto 3','2019-11-04 10:54:05'),(94,'6','Objeto 6','2019-11-04 10:54:05'),(95,'1','Objeto 1','2019-11-04 10:54:07'),(95,'2','Objeto 2','2019-11-04 10:54:07'),(95,'4','Objeto 4','2019-11-04 10:54:07'),(95,'5','Objeto 5','2019-11-04 10:54:07'),(95,'3','Objeto 3','2019-11-04 10:54:07'),(95,'6','Objeto 6','2019-11-04 10:54:07'),(96,'1','Objeto 1','2019-11-08 03:01:02'),(96,'2','Objeto 2','2019-11-08 03:01:02'),(96,'4','Objeto 4','2019-11-08 03:01:02'),(96,'5','Objeto 5','2019-11-08 03:01:02'),(96,'3','Objeto 3','2019-11-08 03:01:02'),(96,'6','Objeto 6','2019-11-08 03:01:02'),(97,'1','Objeto 1','2019-11-04 20:45:19'),(97,'2','Objeto 2','2019-11-04 20:45:19'),(97,'4','Objeto 4','2019-11-04 20:45:19'),(97,'5','Objeto 5','2019-11-04 20:45:19'),(97,'3','Objeto 3','2019-11-04 20:45:19'),(97,'6','Objeto 6','2019-11-04 20:45:19'),(98,'1','Objeto 1','2019-11-16 22:17:03'),(98,'2','Objeto 2','2019-11-16 22:17:03'),(98,'3','Objeto 3','2019-11-16 22:17:03'),(98,'4','Objeto 4','2019-11-16 22:17:03'),(98,'5','Objeto 5','2019-11-16 22:17:03'),(98,'6','Objeto 6','2019-11-16 22:17:03'),(99,'1','Objeto 1','2019-11-22 16:59:47'),(99,'2','Objeto 2','2019-11-22 16:59:47'),(99,'3','Objeto 3','2019-11-22 16:59:47'),(99,'4','Objeto 4','2019-11-22 16:59:47'),(99,'5','Objeto 5','2019-11-22 16:59:47'),(99,'6','Objeto 6','2019-11-22 16:59:47'),(100,'7','Objeto 7','2019-11-22 16:59:48'),(100,'8','Objeto 8','2019-11-22 16:59:48'),(100,'9','Objeto 9','2019-11-22 16:59:48'),(100,'11','Objeto 11','2019-11-22 16:59:48'),(100,'10','Objeto 10','2019-11-22 16:59:48'),(100,'12','Objeto 12','2019-11-22 16:59:48'),(103,'2','Objeto 2','2019-11-28 06:45:41'),(103,'3','Objeto 3','2019-11-28 06:45:41'),(103,'1','Objeto 1','2019-12-02 13:24:49'),(104,'1','Objeto 1','2019-11-30 21:53:28'),(104,'2','Objeto 2','2019-11-30 21:53:28'),(104,'3','Objeto 3','2019-11-30 21:53:28'),(104,'4','Objeto 4','2019-11-30 21:53:28'),(104,'5','Objeto 5','2019-11-30 21:53:28'),(104,'6','Objeto 6','2019-11-30 21:53:28'),(105,'7','Objeto 7','2019-11-30 21:53:30'),(105,'8','Objeto 8','2019-11-30 21:53:30'),(105,'9','Objeto 9','2019-11-30 21:53:30'),(105,'11','Objeto 11','2019-11-30 21:53:30'),(105,'10','Objeto 10','2019-11-30 21:53:30'),(105,'12','Objeto 12','2019-11-30 21:53:30'),(106,'1','Objeto 1','2019-12-02 13:25:12'),(107,'1','Objeto 1','2019-12-13 13:25:56'),(107,'2','Objeto 2','2019-12-13 13:25:56'),(107,'3','Objeto 3','2019-12-13 13:25:56'),(107,'4','Objeto 4','2019-12-13 13:25:56'),(107,'5','Objeto 5','2019-12-13 13:25:56'),(107,'6','Objeto 6','2019-12-13 13:25:56'),(108,'7','Objeto 7','2019-12-13 13:25:57'),(108,'8','Objeto 8','2019-12-13 13:25:57'),(108,'9','Objeto 9','2019-12-13 13:25:57'),(108,'11','Objeto 11','2019-12-13 13:25:57'),(108,'10','Objeto 10','2019-12-13 13:25:57'),(108,'12','Objeto 12','2019-12-13 13:25:57'),(109,'1','Objeto 1','2019-12-13 13:39:20'),(109,'2','Objeto 2','2019-12-13 13:39:20'),(109,'3','Objeto 3','2019-12-13 13:39:20'),(109,'4','Objeto 4','2019-12-13 13:39:20'),(109,'5','Objeto 5','2019-12-13 13:39:20'),(109,'6','Objeto 6','2019-12-13 13:39:20'),(110,'7','Objeto 7','2019-12-13 13:39:22'),(110,'8','Objeto 8','2019-12-13 13:39:22'),(110,'9','Objeto 9','2019-12-13 13:39:22'),(110,'11','Objeto 11','2019-12-13 13:39:22'),(110,'10','Objeto 10','2019-12-13 13:39:22'),(110,'12','Objeto 12','2019-12-13 13:39:22'),(111,'1','Objeto 1','2019-12-13 13:45:58');

/*Data for the table `pu_colores` */

insert  into `pu_colores`(`id`,`planta`,`usuario`,`nombre`,`personalizada`,`obligatorio`,`seleccionado`,`fondo_total`,`fondo_barra_superior`,`fondo_barra_inferior`,`fondo_aplicacion`,`fondo_seleccion`,`fondo_boton`,`fondo_slider`,`fondo_tarjeta`,`fondo_boton_inactivo`,`fondo_boton_positivo`,`fondo_boton_negativo`,`fondo_boton_barra`,`fondo_tiptool`,`fondo_logo`,`fondo_snack_normal`,`fondo_snack_error`,`borde_total`,`borde_seleccion`,`borde_hover`,`borde_boton`,`borde_boton_inactivo`,`borde_tarjeta`,`borde_tiptool`,`color_impar`,`color_par`,`texto_tarjeta`,`texto_tarjeta_resalte`,`texto_barra_superior`,`texto_barra_inferior`,`texto_boton`,`texto_boton_inactivo`,`texto_boton_positivo`,`texto_boton_negativo`,`texto_boton_barra`,`texto_seleccion`,`texto_tiptool`,`texto_snack_normal`,`texto_snack_error`,`texto_solo_texto`,`estatus`,`eliminable`) values (1,0,1,'DARK','N','N','N','000000','404040','404040','303030','101010','353535','151515','252525','t','9ACD32','FF4500','ffffff','FFFFFF','ffffff','303030','FF6347','FFFFFF','DDDDDD','707070','BBBBBB','404040','606060','FFFFFF','t','303030','CCCCCC','FFFFFF','FFFFFF','FFFFFF','CCCCCC','404040','000000','FFFFFF','151515','FFFFFF','000000','FFFFFF','FFFFFF','909090','A','S'),(2,0,0,'AZUL CORPORATIVO','N','S','N','002369','f6f6f6','c9c7c7','FFFFFF','73ccf0','f0eded','cfd0d1','FFFFFF','FFFFFF','58D68D','ff0000','002369','FFFFFF','FFFFFF','d7eefc','FF6347','64718c','babdbf','73ccf0','4bc8fa','d7dadb','64718c','000000','EDFBFF','ffffff','002369','002369','002369','002369','002369','d7dadb','000000','ffffff','FFFFFF','002369','002369','002369','FFFFFF','909090','A','S'),(3,0,0,'AMARILLO M/L','N','N','N','2D3277','ffe600','ffe600','FFFFFF','ffe600','fff9c7','fcfbeb','fffef5','FFFFFF','ffe600','ff0000','ffffff','fff9c7','FFFFFF','fff9c7','FF0000','64718c','babdbf','73ccf0','4bc8fa','c9c17b','64718c','000000','EDFBFF','fffef5','2D3277','002369','2D3277','2D3277','2D3277','c9c17b','000000','ffffff','002369','002369','002369','002369','FFFFFF','a8a694','A','S'),(4,0,0,'rojo','N','N','N','#fa5555','ff0000','ff0000','f55555','','','','','','','','','','','','','','','','','','','','','','','','ffffff','ffffff','','','','','','','','','','','u','S');

/*Data for the table `pu_graficos` */

insert  into `pu_graficos`(`id`,`usuario`,`visualizar`,`grafico`,`titulo`,`titulo_fuente`,`sub_titulo`,`subtitulo_fuente`,`texto_x`,`texto_x_fuente`,`texto_y`,`texto_y_fuente`,`texto_z`,`texto_z_fuente`,`etiqueta_mostrar`,`etiqueta_fuente`,`etiqueta_leyenda`,`etiqueta_color`,`etiqueta_fondo`,`ancho`,`alto`,`margen_arriba`,`margen_abajo`,`margen_izquierda`,`margen_derecha`,`maximo_barras`,`maximo_barraspct`,`agrupar`,`agrupar_posicion`,`agrupar_texto`,`fecha`,`periodo_tipo`,`periodo_atras`,`mostrar_tabla`,`orden`,`color_fondo_barras`,`color_letras`,`color_fondo`,`color_leyenda_fondo`,`color_leyenda`,`ver_esperado`,`grueso_esperado`,`color_esperado`,`texto_esperado`,`valor_esperado`,`incluir_ceros`,`orden_grafica`,`mostrarpct`,`color_barra`,`color_barra_borde`,`color_barra_o`,`color_barra_borde_o`,`ver_leyenda`,`overlap`,`adicionales`,`adicionales_colores`,`adicionales_titulos`,`oee`,`oee_colores`,`oee_tipo`,`oee_nombre`,`tipo_principal`,`colores_multiples`,`color_spiline`,`grueso_spiline`,`mostrar_argumentos`,`activo`,`esperado_esquema`) values (1,0,'S',101,'Tactos por transporte',30,'',20,'TRANSPORTES',17,'NUMERO DE TACTOS',20,'PCT acumulado',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2019-10-21 23:56:41',NULL,NULL,NULL,'0',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','NNNNNN',NULL,NULL,'NNN',NULL,'BBB',';;','B','S','00ff00',3,'S','S',NULL),(2,0,'S',102,'Tactos por vehículo',30,'',20,'VEHICULOS',17,'NUMERO DE TACTOS',20,'PCT acumulado',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2019-10-21 23:59:46',NULL,NULL,NULL,'0',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','NNNNNN',NULL,NULL,'NNN',NULL,'BBB',';;','B',NULL,'00ff00',3,'S','S',NULL),(3,0,'S',103,'Tactos por chofer',30,'',20,'CHOFERES',17,'NUMERO DE TACTOS',20,'PCT acumulado',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2019-10-21 23:59:47',NULL,NULL,NULL,'0',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','NNNNNN',NULL,NULL,'NNN',NULL,'BBB',';;','B',NULL,NULL,3,'S','S',NULL),(4,0,'S',104,'Tactos por tipo de vehículo',30,'',20,'TIPOS DE VEHICULO',17,'NUMERO DE TACTOS',20,'PCT acumulado',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2019-10-21 23:59:48',NULL,NULL,NULL,'0',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','NNNNNN',NULL,NULL,'NNN',NULL,'BBB',';;','B',NULL,NULL,3,'S','S',NULL),(5,0,'S',105,'Tactos por tipo de carga',30,'',20,'TIPOS DE CARGA',17,'NUMERO DE TACTOS',20,'PCT acumulado',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2019-10-21 23:59:49',NULL,NULL,NULL,'0',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','NNNNNN',NULL,NULL,'NNN',NULL,'BBB',';;','B',NULL,NULL,3,'S','S',NULL),(7,0,'S',107,'Tactos por día',30,'',20,'DIAS',17,'NUMERO DE TACTOS',20,'PCT acumulado',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2019-10-21 23:59:50',NULL,NULL,NULL,'0',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','NNNNNN',NULL,NULL,'NNN',NULL,'BBB',';;','B',NULL,NULL,3,'S','S',NULL),(8,0,'S',108,'Tactos por semana',30,'',20,'SEMANAS',17,'NUMERO DE TACTOS',20,'PCT acumulado',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2019-10-21 23:59:52',NULL,NULL,NULL,'0',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','NNNNNN',NULL,NULL,'NNN',NULL,'BBB',';;','B',NULL,NULL,3,'S','S',NULL),(9,0,'S',109,'Tactos por mes',30,'',20,'MESES',17,'NUMERO DE TACTOS',20,'PCT acumulado',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2019-10-22 00:00:31',NULL,NULL,NULL,'0',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','NNNNNN',NULL,NULL,'NNN',NULL,'BBB',';;','B',NULL,NULL,3,'S','S',NULL),(159,0,'S',207,'Eficiencia por semana',30,NULL,20,'SEMANAS',17,'TIEMPO (HR)',20,'PCT Eficiencia',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2020-04-09 20:09:37',NULL,NULL,NULL,'1',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','0;0;0;0;0;0;0',NULL,NULL,'NNNSSS','721920;906071;fff432','BBB',';;','B',NULL,NULL,3,'S','S',NULL),(160,0,'S',208,'Eficiencia por mes',30,NULL,20,'MESES',17,'TIEMPO (HR)',20,'PCT Eficiencia',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2020-04-09 20:09:38',NULL,NULL,NULL,'1',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','0;0;0;0;0;0;0',NULL,NULL,'NNNSSS','721920;906071;fff432','BBB',';;','B',NULL,NULL,3,'S','S',NULL),(6,0,'S',106,'Tactos por área',30,'',20,'AREAS',17,'NUMERO DE TACTOS',20,'PCT acumulado',17,'S',16,'LEYENDA',NULL,NULL,0,0,0,0,0,0,0,0,'S','F','AGRUPADO','2020-03-29 00:20:35',NULL,NULL,NULL,'0',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','0;0;0;0;0;0;0',NULL,NULL,'NNNSSS',NULL,'BBB',';;','B',NULL,NULL,3,'S','S',NULL),(154,0,'S',201,'Eficiencia por transporte',30,NULL,20,'TRANSPORTES',17,'TIEMPO (HR)',20,'PCT Eficiencia',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2020-04-09 20:08:31',NULL,NULL,NULL,'1',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','0;0;0;0;0;0;0',NULL,NULL,'NNNSSS','721920;906071;fff432','BBB',';;','B',NULL,NULL,3,'S','S',NULL),(155,0,'S',202,'Eficiencia por tipo de vehículo',30,NULL,20,'TIPOS DE VEHICULO',17,'TIEMPO (HR)',20,'PCT Eficiencia',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2020-04-09 20:08:32',NULL,NULL,NULL,'1',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','0;0;0;0;0;0;0',NULL,NULL,'NNNSSS','721920;906071;fff432','BBB',';;','B',NULL,NULL,3,'S','S',NULL),(156,0,'S',203,'Eficiencia por tipo de carga',30,NULL,20,'TIPOS DE CARGA',17,'TIEMPO (HR)',20,'PCT Eficiencia',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2020-04-09 20:08:33',NULL,NULL,NULL,'1',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','0;0;0;0;0;0;0',NULL,NULL,'NNNSSS','721920;906071;fff432','BBB',';;','B',NULL,NULL,3,'S','S',NULL),(157,0,'S',205,'Eficiencia por área',30,NULL,20,'AREAS',17,'TIEMPO (HR)',20,'PCT Eficiencia',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2020-04-09 20:08:34',NULL,NULL,NULL,'1',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','0;0;0;0;0;0;0',NULL,NULL,'NNNSSS','721920;906071;fff432','BBB',';;','B',NULL,NULL,3,'S','S',NULL),(158,0,'S',206,'Eficiencia por día',30,NULL,20,'DIAS',17,'TIEMPO (HR)',20,'PCT Eficiencia',17,'S',16,'LEYENDA',NULL,NULL,NULL,0,0,0,0,0,0,0,'S','F','AGRUPADO','2020-04-09 20:08:35',NULL,NULL,NULL,'1',NULL,NULL,NULL,NULL,NULL,'N',2,'ff0000','ESPERADO','5.000','N','N','N',NULL,NULL,NULL,NULL,'S','S','0;0;0;0;0;0;0',NULL,NULL,'NNNSSS','721920;906071;fff432','BBB',';;','B',NULL,NULL,3,'S','S',NULL),(164,0,'S',204,'Eficiencia por destino',30,'',20,'DESTINOS',17,'TIEMPO (HR)',20,'PCT Eficiencia',17,'S',16,'LEYENDA','','',0,0,0,0,0,0,0,0,'S','N','AGRUPADO','2020-04-11 13:17:42',NULL,NULL,NULL,'1','','','','','','N',2,'ff0000','ESPERADO','5.000','N','M','N','','',NULL,NULL,'S','S','0;0;0;0;0;0;0',';;;;;',';;;;;','SSSSSS','721920;906071;fff432','BLB','','B','','',3,'S','S','N;;'),(165,1,'S',101,'Tactos por transporte',30,'',20,'TRANSPORTES',17,'NUMERO DE TACTOS',20,'PCT acumulado',17,'S',16,'LEYENDA','','',0,0,0,0,0,0,0,0,'S','F','AGRUPADO','2020-04-13 09:11:44',NULL,NULL,NULL,'0','','','','','','N',2,'ff0000','ESPERADO','5.000','N','N','N','','',NULL,NULL,'S','S','0;0;0;0;0;0;0',';;;;;',';;;;;','NNNNNN',';;','BBB',';;','B','S','00ff00',3,'S','S','N;;');

/*Data for the table `status_objetos` */

insert  into `status_objetos`(`id`,`descripcion`,`color`,`normal`,`mapa_Id`) values (0,'critico','red',0,83),(0,'critico','red',0,84),(1,'critico','red',0,83),(1,'critico','red',0,84),(2,'atencion','yellow',0,83),(2,'atencion','yellow',0,84),(3,'normal','',1,83),(3,'atencion','yellow',1,84),(10,'atencion','yellow',0,83),(0,'critico','red',0,85),(10,'atencion','yellow',0,85),(0,'atencion','yellow',0,86),(0,'critico','red',0,87),(10,'atencion','yellow',0,87),(0,'critico','red',0,89),(10,'atencion','yellow',0,89),(0,'critico','red',0,90),(10,'atencion','yellow',0,90),(0,'critico','red',0,91),(10,'atencion','yellow',0,91),(0,'critico','red',0,92),(10,'atencion','yellow',0,92),(0,'critico','red',0,93),(10,'atencion','yellow',0,93),(0,'critico','red',0,94),(10,'atencion','yellow',0,94),(0,'critico','red',0,95),(10,'atencion','yellow',0,95),(0,'critico','red',0,96),(10,'atencion','yellow',0,96),(0,'critico','red',0,97),(10,'atencion','yellow',0,97),(0,'critico','red',0,98),(10,'atencion','yellow',0,98),(0,'critico','red',0,99),(10,'atencion','yellow',0,99),(0,'critico','red',0,100),(10,'atencion','yellow',0,100),(0,'critico','red',0,103),(10,'atencion','yellow',0,103),(0,'critico','red',0,104),(10,'atencion','yellow',0,104),(0,'critico','red',0,105),(10,'atencion','yellow',0,105),(0,'critico','red',0,106),(10,'atencion','yellow',0,106),(0,'critico','red',0,107),(10,'atencion','yellow',0,107),(0,'critico','red',0,108),(10,'atencion','yellow',0,108),(0,'critico','red',0,109),(10,'atencion','yellow',0,109),(0,'critico','red',0,110),(10,'atencion','yellow',0,110),(0,'critico','red',0,111),(10,'atencion','yellow',0,111);

/*Data for the table `tablas` */

insert  into `tablas`(`id`,`nombre`) values (50,'TIPO DE VEHÍCULO'),(10,'TIPO DE TRANSPORTE'),(30,'AREAS DE LA PLANTA'),(70,'DEPARTAMENTOS'),(80,'COMPANIAS'),(90,'PLANTAS'),(65,'TIPOS DE CARGA'),(20,'MARCAS DE VEHÍCULO'),(25,'MODELOS DE VEHÍCULO');

/*Data for the table `tipos_figuras` */

insert  into `tipos_figuras`(`id`,`descripcion`) values (107,'Diamond'),(108,'Oval'),(109,'DownArrow'),(110,'Parallelogram'),(111,'RegularPentagon'),(112,'Hexagon'),(113,'Can'),(114,'Rectangle'),(115,'IsoscelesTriangle'),(116,'5pointStar'),(117,'RightArrow'),(118,'RoundedRectangle'),(119,'Cross'),(120,'6pointStar'),(121,'UpArrow'),(122,'LeftArrow'),(123,'BentUpArrow'),(124,'Cube'),(125,'Picture'),(126,'FlowchartData'),(127,'Trapezoid');
")


            End If

        End If

        cadSQL = "SELECT * FROM " & rutaBD & ".sentencias WHERE estatus = 'N'"
        Dim sentencias As DataSet = consultaSEL(cadSQL)

        If sentencias.Tables(0).Rows.Count > 0 Then
            huboProceso = True
            Dim tSentencias = 0, aSentencias = 0
            For Each sentencia In sentencias.Tables(0).Rows
                tSentencias = tSentencias + 1
                regsAfectados = consultaACT(sentencia!sentencia)
                If regsAfectados <> -1 Then
                    aSentencias = aSentencias + 1
                    regsAfectados = consultaACT("UPDATE " & rutaBD & ".sentencias SET estatus = 'S' WHERE id = " & sentencia!id)
                End If
            Next
            regsAfectados = consultaACT("DELETE FROM " & rutaBD & ".sentencias WHERE estatus = 'S'")
            XtraMessageBox.Show("Se procesaron " & tSentencias & " sentencias SQL y se aplicaron " & aSentencias & " de manera correcta", "Logisticar v1.0", MessageBoxButtons.OK, If(aSentencias = tSentencias, MessageBoxIcon.Information, MessageBoxIcon.Error))

        End If
        If huboProceso Then
            XtraMessageBox.Show("Se realizaron actualizaciones en la base de datos por lo que deberá iniciar de nuevo el Backend", "Logisticar v1.0", MessageBoxButtons.OK, MessageBoxIcon.Information)
            Application.Exit()

        Else
            iniciarPantalla()
        End If

    End Sub

End Class


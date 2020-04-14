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
        ElseIf argumentos.Length <= 1 Then
            XtraMessageBox.Show("No se puede iniciar el monitor: Se requiere la cadena de conexión", "Sesión iniciada", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Application.Exit()
        Else
            idProceso = Process.GetCurrentProcess.Id
            cadenaConexion = argumentos(1)
            cadenaConexion = "server=localhost;user id=root;password=usbw;port=3307;Convert Zero Datetime=True;Allow User Variables=True"
            iniciarPantalla()

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
        GroupControl1.Width = ListBoxControl1.Width
        ListBoxControl1.Height = Me.Height - 250
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
        If enMonitor Or Not estadoPrograma Then Exit Sub

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
        revisaFlag.Enabled = False
        revisarEventos()
        cancelarAlertas()
        depurar()
        enMonitor = False
        rev_revisarMensajes()
        rev_llamadas()
        enviar_mensajes()

        revisaFlag.Enabled = True

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
            cadSQL = "SELECT a.id, 0 AS lote, (SELECT id FROM " & rutaBD & ".cat_alertas z WHERE (z.linea = 'S' OR a.transporte IN (SELECT detalle FROM " & rutaBD & ".relaciones WHERE operacion = 1 AND indice = z.id)) AND evento = 101 AND estatus = 'A' AND TIME_TO_SEC(TIMEDIFF(NOW(), a.desde)) >= transcurrido ORDER BY prioridad DESC, linea LIMIT 1) AS idalerta FROM " & rutaBD & ".requesters a WHERE a.alarmado = 'N' AND a.estado = 10 AND a.preasignado = 'N' HAVING idalerta > 0 "
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
        If leyendoLog Then Exit Sub
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
        Dim cadSQL As String = "SELECT destino, id, fecha_recibo, IF(estado = 10, estimado + des_estimado, 0) AS espera FROM " & rutaBD & ".requesters WHERE estado = 10 AND estatus <> 2 ORDER BY destino, fecha_asignacion"

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


End Class


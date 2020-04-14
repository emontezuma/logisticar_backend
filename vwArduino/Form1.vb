Imports MySql.Data.MySqlClient
Imports System.IO.Ports
Imports System.IO
Imports System.Text
Imports System.Net.Mail
Imports System.Net
Imports System.ComponentModel
Imports System.Data
Imports System.Windows.Forms
Imports System.Speech.Synthesis


Public Class Form1

    Dim Estado As Integer = 0
    Dim procesandoAudios As Boolean = False
    Dim eSegundos = 0
    Dim procesandoEscalamientos As Boolean
    Dim procesandoLlamadas As Boolean = False
    Dim estadoPrograma As Boolean = False
    Dim MensajeLlamada = ""
    Dim errorPuerto As Boolean = False
    Dim ptoError As String = ""
    Dim idProceso As String
    Dim be_log_activar As Boolean
    Dim contador As Integer = 0

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Dim argumentos As String() = Environment.GetCommandLineArgs()
        Me.Height = 45
        If Process.GetProcessesByName _
          (Process.GetCurrentProcess.ProcessName).Length > 1 Then
        ElseIf argumentos.Length <= 1 Then
            MsgBox("No se puede iniciar el envío de correos: Se requiere la cadena de conexión", MsgBoxStyle.Critical, "SIGMA Monitor")
        Else
            cadenaConexion = argumentos(1)
            contador = contador + 1
            TextBox1.Text = contador & ") Se inicia la interfaz telefónica con la cadena de conexión " & cadenaConexion & vbCrLf & TextBox1.Text

            'cadenaConexion = "server=localhost;user id=root;password=usbw;port=3307;Convert Zero Datetime=True"
            Dim idProceso = Process.GetCurrentProcess.Id

            idProceso = Process.GetCurrentProcess.Id

            estadoPrograma = True
            generarLlamadas()
        End If
        Application.Exit()
    End Sub

    Private Sub generarLlamadas()
        Dim rutaAudios
        Dim rutaSMS
        Dim ptoCOMM1 As String = "", ptoCOMM2 As String = "", ptoCOMM3 As String = "", ptoCOMM4 As String = "", ptoCOMM5 As String = "", ptoCOMM6 As String = ""
        Dim ptoCOMM1P As String = "", ptoCOMM2P As String = "", ptoCOMM3P As String = "", ptoCOMM4P As String = "", ptoCOMM5P As String = "", ptoCOMM6P As String = ""
        Dim escape_veces = 3
        Dim escape_accion = ""
        Dim escape_lista = 0
        Dim escape_mensaje = ""
        Dim veces_reproducir = 1
        Dim tOutLlamada = 20
        Dim tOutSMS = 5
        Dim escape_mensaje_propio As Boolean = True
        Dim be_alarmas_llamadas As Boolean = False
        Dim be_alarmas_sms As Boolean = False
        Dim cadSQL As String = "SELECT * FROM " & rutaBD & ".configuracion"
        Dim readerDS As DataSet = consultaSEL(cadSQL)

        If readerDS.Tables(0).Rows.Count > 0 Then
            Dim reader As DataRow = readerDS.Tables(0).Rows(0)
            ptoCOMM1 = ValNull(reader!puerto_comm1, "A")
            ptoCOMM1P = ValNull(reader!puerto_comm1_par, "A")
            ptoCOMM2 = ValNull(reader!puerto_comm2, "A")
            ptoCOMM2P = ValNull(reader!puerto_comm2_par, "A")
            ptoCOMM3 = ValNull(reader!puerto_comm3, "A")
            ptoCOMM3P = ValNull(reader!puerto_comm3_par, "A")
            ptoCOMM4 = ValNull(reader!puerto_comm4, "A")
            ptoCOMM4P = ValNull(reader!puerto_comm4_par, "A")
            ptoCOMM5 = ValNull(reader!puerto_comm5, "A")
            ptoCOMM5P = ValNull(reader!puerto_comm5_par, "A")
            ptoCOMM6 = ValNull(reader!puerto_comm6, "A")
            rutaAudios = ValNull(reader!ruta_audios, "A")
            ptoCOMM6P = ValNull(reader!puerto_comm6_par, "A")
            rutaSMS = ValNull(reader!ruta_sms, "A")

            escape_veces = ValNull(reader!escape_llamadas, "N")
            escape_accion = ValNull(reader!escape_accion, "A")
            escape_lista = ValNull(reader!escape_lista, "A")
            escape_mensaje = ValNull(reader!escape_mensaje, "A")
            veces_reproducir = ValNull(reader!veces_reproducir, "N")
            tOutLlamada = ValNull(reader!timeout_llamadas, "N")
            tOutSMS = ValNull(reader!timeout_sms, "N")

            be_alarmas_llamadas = ValNull(reader!be_alarmas_llamadas, "A") = "S"
            be_alarmas_sms = ValNull(reader!be_alarmas_sms, "A") = "S"



            escape_mensaje_propio = ValNull(reader!escape_mensaje_propio, "A") = "S"
        ElseIf Not errorPuerto Then
            errorPuerto = True
            agregarLOG("No se ha configurado la interacción con Arduino, por favor configure la aplicación e intente de nuevo", 0, 9)
            Exit Sub
        End If

        If be_alarmas_llamadas Or be_alarmas_sms Then
            contador = contador + 1
            TextBox1.Text = contador & ") Activado una llamada/SMS" & vbCrLf & TextBox1.Text
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
            contador = contador + 1
            TextBox1.Text = contador & ") Ruta de audios: " & rutaAudios & vbCrLf & TextBox1.Text
            contador = contador + 1
            TextBox1.Text = contador & ") Ruta de SMS: " & rutaSMS & vbCrLf & TextBox1.Text

            If ptoCOMM1.Length = 0 And ptoCOMM2.Length = 0 And ptoCOMM3.Length = 0 And ptoCOMM4.Length = 0 And ptoCOMM5.Length = 0 And ptoCOMM6.Length = 0 Then
                If Not errorPuerto Then
                    agregarLOG("No se ha configurado ningún puerto de comunicaciones para la interacción con Arduino, por favor configure la aplicación e intente de nuevo", 0, 9)
                    errorPuerto = True
                    Exit Sub
                End If
            End If
            If ptoCOMM1P.Length = 0 And ptoCOMM2P.Length = 0 And ptoCOMM3P.Length = 0 And ptoCOMM4P.Length = 0 And ptoCOMM5P.Length = 0 And ptoCOMM6P.Length = 0 Then
                If Not errorPuerto Then
                    errorPuerto = True
                    agregarLOG("No se ha configurado ningún puerto de comunicaciones para la interacción con Arduino, por favor configure la aplicación e intente de nuevo", 0, 9)
                    Exit Sub
                End If
            End If
            If escape_veces = 0 Then escape_veces = 3
            If veces_reproducir = 0 Then veces_reproducir = 1
            ptoCOMM1 = ptoCOMM1.ToUpper
            ptoCOMM2 = ptoCOMM2.ToUpper
            ptoCOMM3 = ptoCOMM3.ToUpper
            ptoCOMM3 = ptoCOMM4.ToUpper
            ptoCOMM5 = ptoCOMM5.ToUpper
            ptoCOMM6 = ptoCOMM6.ToUpper
            'Llamadas telefónicas

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
                contador = contador + 1
                TextBox1.Text = contador & ") Se encontraron " & LlamadasPendientes & " audios" & vbCrLf & TextBox1.Text

                Dim iniLlamadas = DateTime.Now
                'Dim Comando As OdbcCommand = New OdbcCommand(CadSQL)
                If Not ValPuerto(ptoCOMM1, ptoCOMM1P) Then
                    contador = contador + 1
                    TextBox1.Text = contador & ") No se puede accder al puerto " & ptoCOMM1 & " " & ptoCOMM1P & vbCrLf & TextBox1.Text

                    agregarLOG("Puerto:" & ptoCOMM1 & " parámetros: " & ptoCOMM1P & ". No se emitieron " & LlamadasPendientes & " llamada(s). Error: " & ptoError, 0, 2)
                Else

                    For Each FoundFile As String In My.Computer.FileSystem.GetFiles(
          rutaAudios, Microsoft.VisualBasic.FileIO.SearchOption.SearchTopLevelOnly, "*.wav")
                        If My.Computer.FileSystem.FileExists(FoundFile) Then
                            If Not estadoPrograma Then
                                Exit Sub
                            End If
                            Try
                                If Not SerialPort1.IsOpen Then
                                    SerialPort1.Open()
                                End If
                                'ReceiveSerialData()
                                Dim Numero = Microsoft.VisualBasic.Strings.Left(Path.GetFileName(FoundFile), 10)
                                Dim nombreArchivo = Path.GetFileName(FoundFile)
                                contador = contador + 1
                                TextBox1.Text = contador & ") se encontró audio para el número " & Numero & vbCrLf & TextBox1.Text

                                If IsNumeric(Numero) Then
                                    contador = contador + 1
                                    TextBox1.Text = contador & ") se inicia llamada a través de la interfaz telefónica" & Numero & vbCrLf & TextBox1.Text

                                    Try
                                        contador = contador + 1
                                        TextBox1.Text = contador & ") VB 'VOID'" & vbCrLf & TextBox1.Text

                                        SerialPort1.Write("VOID" & vbNewLine)
                                        Demora(1)
                                        contador = contador + 1
                                        TextBox1.Text = contador & ") VB '~CALL'" & vbCrLf & TextBox1.Text

                                        SerialPort1.Write("~CALL" & Numero & vbNewLine)
                                        Dim LimiteTimeout As Integer = tOutLlamada
                                        Dim Salir = False
                                        Dim TiempoInicial = DateTime.Now
                                        MensajeLlamada = ""
                                        Do While Not Salir
                                            If Not estadoPrograma Then
                                                Exit Sub
                                            End If
                                            'Se cuentan hasta 30seg
                                            Application.DoEvents()
                                            Dim TiempoFinal = DateTime.Now
                                            Dim TotalSegundos = TiempoFinal - TiempoInicial
                                            If TotalSegundos.Seconds >= LimiteTimeout Then
                                                contador = contador + 1
                                                TextBox1.Text = contador & ") SE CANCELA POR TIMEOUT'" & vbCrLf & TextBox1.Text
                                                MensajeLlamada = "timeout"
                                                Salir = True
                                            ElseIf MensajeLlamada.Length > 0 Then
                                                If Microsoft.VisualBasic.Strings.InStr(MensajeLlamada, "CONNECTED") > 0 Then
                                                    Salir = True
                                                End If
                                            End If
                                        Loop
                                        If MensajeLlamada = "timeout" Then
                                            contador = contador + 1
                                            TextBox1.Text = contador & ") VB 'VOID'" & vbCrLf & TextBox1.Text

                                            SerialPort1.Write("VOID" & vbNewLine)
                                            'Se busca las veces que se ha repeoducido el audio
                                            Dim newFile = ""
                                            If escape_veces > 0 Then
                                                'Controlar las llamadas

                                                Dim eliminado = False
                                                If escape_veces <= 1 And Microsoft.VisualBasic.Strings.InStr(FoundFile, "_1.wav") > 0 Then
                                                    eliminarArchivo(FoundFile)
                                                    eliminado = True
                                                ElseIf escape_veces <= 2 And Microsoft.VisualBasic.Strings.InStr(FoundFile, "_2.wav") > 0 Then
                                                    eliminarArchivo(FoundFile)
                                                    eliminado = True
                                                ElseIf escape_veces <= 3 And Microsoft.VisualBasic.Strings.InStr(FoundFile, "_3.wav") > 0 Then
                                                    eliminarArchivo(FoundFile)
                                                    eliminado = True
                                                ElseIf escape_veces <= 4 And Microsoft.VisualBasic.Strings.InStr(FoundFile, "_4.wav") > 0 Then
                                                    eliminarArchivo(FoundFile)
                                                    eliminado = True
                                                ElseIf escape_veces <= 5 And Microsoft.VisualBasic.Strings.InStr(FoundFile, "_5.wav") > 0 Then
                                                    eliminarArchivo(FoundFile)
                                                    eliminado = True
                                                End If
                                                If Not eliminado Then
                                                    newFile = FoundFile
                                                    newFile = Microsoft.VisualBasic.Strings.Replace(newFile, "_4", "_5")
                                                    newFile = Microsoft.VisualBasic.Strings.Replace(newFile, "_3", "_4")
                                                    newFile = Microsoft.VisualBasic.Strings.Replace(newFile, "_2", "_3")
                                                    newFile = Microsoft.VisualBasic.Strings.Replace(newFile, "_1", "_2")
                                                    My.Computer.FileSystem.RenameFile(FoundFile, Path.GetFileName(newFile))
                                                    agregarLOG("Se hizo una llamada sin respuesta al repositorio: " & Numero, 0, 2)
                                                ElseIf escape_accion = "E" Then
                                                    'Se escapa la llamada
                                                    If escape_lista > 0 Then
                                                        Dim regsAfectados = consultaACT("INSERT into " & rutaBD & ".mensajes (canal, tipo, lista, texto) VALUES (0, 99, " & escape_lista & ", '" & Numero & "'),(2, 99, " & escape_lista & ", '" & Numero & "')")
                                                    End If
                                                    If escape_mensaje_propio Then
                                                        'Se crea rl archivo
                                                        Try
                                                            System.IO.File.Create(rutaSMS & "\" & Numero & Format(Now, "hhmmss") & "9999" & ".txt").Dispose()
                                                            Dim objWriter As New System.IO.StreamWriter(rutaSMS & "\" & Numero & Format(Now, "hhmmss") & "9999" & ".txt", True)
                                                            escape_mensaje = "Se agotó el número de intentos de llamada para el teléfono " & Numero
                                                            Dim antes As String = "ÃÀÁÄÂÈÉËÊÌÍÏÎÒÓÖÔÙÚÜÛãàáäâèéëêìíïîòóöôùúüûÑñÇç"
                                                            Dim ahora As String = "AAAAAEEEEIIIIOOOOUUUUaaaaaeeeeiiiioooouuuunncc"
                                                            For i = 0 To antes.Length - 1
                                                                escape_mensaje = Replace(escape_mensaje, antes(i), ahora(i))
                                                            Next
                                                            escape_mensaje = Replace(escape_mensaje, ";", " ")
                                                            escape_mensaje = Replace(escape_mensaje, "\", "-")
                                                            escape_mensaje = Replace(escape_mensaje, "/", "-")
                                                            objWriter.WriteLine(escape_mensaje)
                                                            objWriter.Close()
                                                        Catch ex As Exception

                                                        End Try
                                                    End If
                                                    escaparLlamada(Numero)
                                                End If
                                            End If
                                        Else
                                            contador = contador + 1
                                            TextBox1.Text = contador & ") REPRODUCIENDO AUDIOS" & vbCrLf & TextBox1.Text
                                            Dim fs As FileStream = New FileStream(FoundFile, FileMode.Open, FileAccess.Read)
                                            Dim sp As System.Media.SoundPlayer = New System.Media.SoundPlayer(fs)
                                            For i = 0 To veces_reproducir - 1
                                                sp.PlaySync()
                                            Next i
                                            fs.Close()
                                            Salir = False
                                            TiempoInicial = DateTime.Now
                                            Dim TiempoFinal = DateTime.Now
                                            Do While Not Salir And My.Computer.FileSystem.FileExists(FoundFile)
                                                'Se cuentan hasta 30seg
                                                eliminarArchivo(FoundFile)
                                                TiempoFinal = DateTime.Now
                                                Dim TotalSegundos = TiempoFinal - TiempoInicial
                                                If TotalSegundos.Seconds > 1 Then
                                                    Salir = True
                                                End If
                                            Loop
                                            If Salir Then
                                                agregarLOG("Se hizo una llamada, se reprodujo un audio pero no se eliminó correctamente el archivo...", 0, 9)
                                            Else
                                                agregarLOG("Se acaba de realizar una llamada satisfactoria al repositorio : " & Numero)
                                            End If
                                        End If
                                    Catch ex As Exception
                                    End Try
                                End If
                            Catch ex2 As Exception
                            End Try
                        End If
                        'Se mueven los archivos a otra carpeta
                    Next
                    Dim tSegundos = DateTime.Now - iniLlamadas

                    agregarLOG("Se procesaron " & LlamadasPendientes & " llamada(s) de voz en " & tSegundos.Seconds & " segundo(s)...")

                End If
            End If
            'Se envian SMS

            LlamadasPendientes = 0
            For Each FoundFile As String In My.Computer.FileSystem.GetFiles(
    rutaSMS, Microsoft.VisualBasic.FileIO.SearchOption.SearchTopLevelOnly, "*.txt")
                LlamadasPendientes = LlamadasPendientes + 1
            Next
            If LlamadasPendientes > 0 And be_alarmas_sms Then
                contador = contador + 1
                TextBox1.Text = contador & ") Se encontraron " & LlamadasPendientes & " SMS" & vbCrLf & TextBox1.Text
                Dim iniLlamadas = DateTime.Now

                If Not ValPuerto(ptoCOMM1, ptoCOMM1P) Then
                    agregarLOG("Puerto:" & ptoCOMM1 & " parámetros: " & ptoCOMM1P & ". No se emitieron " & LlamadasPendientes & " SMS. Error: " & ptoError, 0, 2)
                Else

                    For Each FoundFile As String In My.Computer.FileSystem.GetFiles(
          rutaSMS, Microsoft.VisualBasic.FileIO.SearchOption.SearchTopLevelOnly, "*.txt")
                        If Not estadoPrograma Then
                            Exit Sub
                        End If
                        If My.Computer.FileSystem.FileExists(FoundFile) Then
                            Try
                                If Not SerialPort1.IsOpen Then
                                    SerialPort1.Open()
                                End If
                                Dim miReader As StreamReader = My.Computer.FileSystem.OpenTextFileReader(FoundFile)
                                Dim elMensaje As String = miReader.ReadLine
                                elMensaje = Microsoft.VisualBasic.Strings.Left(elMensaje, 120)
                                miReader.Close()
                                If elMensaje.Length > 0 Then
                                    Dim Numero = Microsoft.VisualBasic.Strings.Left(Path.GetFileName(FoundFile), 10)
                                    Dim nombreArchivo = Path.GetFileName(FoundFile)
                                    If IsNumeric(Numero) Then
                                        Try
                                            contador = contador + 1
                                            TextBox1.Text = contador & ") VB 'VOID'" & vbCrLf & TextBox1.Text

                                            SerialPort1.Write("VOID" & vbNewLine)
                                            Demora(1)
                                            contador = contador + 1
                                            TextBox1.Text = contador & ") VB '~SMS01" & Numero & elMensaje & "'" & vbCrLf & TextBox1.Text

                                            SerialPort1.Write("~SMS01" & Numero & elMensaje & vbNewLine)
                                            Dim LimiteTimeout As Integer = tOutSMS
                                            Dim Salir = False
                                            Dim TiempoInicial = DateTime.Now
                                            MensajeLlamada = ""
                                            Do While Not Salir
                                                If Not estadoPrograma Then
                                                    Exit Sub
                                                End If
                                                'Se cuentan hasta 30seg
                                                Application.DoEvents()
                                                Dim TiempoFinal = DateTime.Now
                                                Dim TotalSegundos = TiempoFinal - TiempoInicial
                                                If TotalSegundos.Seconds >= LimiteTimeout Then
                                                    contador = contador + 1
                                                    TextBox1.Text = contador & ") SE CANCELA POR TIMEOUT DE SMS'" & vbCrLf & TextBox1.Text

                                                    MensajeLlamada = "timeout"
                                                    Salir = True
                                                ElseIf MensajeLlamada.Length > 0 Then
                                                    If Microsoft.VisualBasic.Strings.InStr(MensajeLlamada, "OK") Or Microsoft.VisualBasic.Strings.InStr(MensajeLlamada, "finalizada") Then
                                                        Salir = True
                                                    Else
                                                        Salir = True
                                                        MensajeLlamada = "timeout"
                                                    End If
                                                End If
                                            Loop
                                            If MensajeLlamada = "timeout" Then
                                                'Se busca las veces que se ha repeoducido el audio
                                                escaparSMS(Numero)
                                            Else
                                                contador = contador + 1
                                                TextBox1.Text = contador & ") SE ENVIA SMS'" & vbCrLf & TextBox1.Text

                                                agregarLOG("Se envío en mensaje de texto correctamente al repositorio: " & Numero, 1, 0)
                                            End If
                                            eliminarArchivo(FoundFile)
                                        Catch ex As Exception
                                        End Try
                                    Else
                                        eliminarArchivo(FoundFile)
                                    End If
                                Else
                                    eliminarArchivo(FoundFile)
                                End If
                            Catch ex2 As Exception
                            End Try
                        End If
                        'Se mueven los archivos a otra carpeta
                    Next
                    Dim tSegundos = DateTime.Now - iniLlamadas

                    agregarLOG("Se procesaron " & LlamadasPendientes & " mensaje(s) de texto (SMS) en " & tSegundos.Seconds & " segundo(s)...")

                End If
            End If
        Else
            contador = contador + 1
            TextBox1.Text = contador & ") No estan activo llamada/SMS" & vbCrLf & TextBox1.Text

        End If
    End Sub

    Sub eliminarArchivo(archivo)
        Try

            My.Computer.FileSystem.DeleteFile(archivo)
            File.Delete(archivo)
        Catch ex As Exception

        End Try

    End Sub

    Function ValPuerto(ePuerto As String, ePar As String) As Boolean

        ValPuerto = True

        Try
            SerialPort1.Close()
        Catch ex As Exception
            ptoError = ex.Message
            Exit Function

        End Try



        Try
            SerialPort1.PortName = ePuerto

            Dim parametros = ePar.Split(New Char() {","c})

            SerialPort1.BaudRate = parametros(0) '19200
            SerialPort1.DataBits = parametros(1) '8
            SerialPort1.Parity = parametros(2) '0


            SerialPort1.StopBits = parametros(3) '1


            SerialPort1.Handshake = parametros(4) '2
            SerialPort1.RtsEnable = parametros(5) = "S" 'True

            SerialPort1.Open()

        Catch ex As Exception
            ValPuerto = False
            ptoError = ex.Message
            Exit Function
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

    Private Sub agregarLOG(cadena As String, Optional reporte As Integer = 0, Optional tipo As Integer = 0, Optional aplicacion As Integer = 30)
        If Not be_log_activar Then Exit Sub
        'tipo 0: Info
        'tipo 2: Advertencia
        'tipo 9: Error
        Dim regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".log (aplicacion, tipo, proceso, texto) VALUES (" & aplicacion & ", " & tipo & ", " & reporte & ", '" & Microsoft.VisualBasic.Strings.Left(cadena, 250) & "')")
    End Sub

    Sub Demora(Segundos As Integer)
        Dim Salir = False
        Dim TInicial = DateTime.Now
        Dim TotalSegundos As TimeSpan
        Do While Not Salir
            Dim TiempoFinal = DateTime.Now
            TotalSegundos = TiempoFinal - TInicial
            If TotalSegundos.Seconds > Segundos Then
                Salir = True
            End If
        Loop
    End Sub


    Sub escaparLlamada(numero)
        agregarLOG("Se agotó el número de intentos de llamada de voz al repositorio: " & numero, 0, 2)
    End Sub

    Sub escaparSMS(numero)
        agregarLOG("Se agotó el número de intentos de envio de SMS al repositorio: " & numero, 0, 2)
    End Sub

    Private Sub SerialPort1_DataReceived(sender As Object, e As SerialDataReceivedEventArgs) Handles SerialPort1.DataReceived
        MensajeLlamada = SerialPort1.ReadLine
        contador = contador + 1
        TextBox1.Text = contador & ") COMM " & MensajeLlamada & vbCrLf & TextBox1.Text
    End Sub

    Private Sub Label1_Click(sender As Object, e As EventArgs) Handles Label1.Click

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If Me.Height = 45 Then
            Me.Height = 315
        Else
            Me.Height = 45
        End If
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        'If Not procesandoLlamadas Then Exit Sub
        'TextBox1.Text = ""
        'procesandoLlamadas = True
        'generarLlamadas()
        'procesandoLlamadas = False

    End Sub

    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        TextBox1.Text = ""
        contador = 0
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Application.Exit()
    End Sub
End Class
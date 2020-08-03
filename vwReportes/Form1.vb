Imports MySql.Data.MySqlClient
Imports System.IO.Ports
Imports System.IO
Imports System.Text
Imports System.Net.Mail
Imports System.Net
Imports System.ComponentModel
Imports System.Data
Imports System.Windows.Forms
Imports DevExpress.XtraCharts
Imports DevExpress.XtraGauges.Win
Imports DevExpress.XtraGauges.Win.Base
Imports DevExpress.XtraGauges.Win.Gauges.Circular
Imports DevExpress.XtraGauges.Core.Model
Imports DevExpress.XtraGauges.Core.Base
Imports DevExpress.XtraGauges.Core.Drawing
Imports System.Drawing
Imports System.Drawing.Imaging


Public Class Form1

    Dim Estado As Integer = 0
    Dim procesandoAudios As Boolean = False
    Dim eSegundos = 0
    Dim procesandoEscalamientos As Boolean
    Dim procesandoRepeticiones As Boolean
    Dim estadoPrograma As Boolean
    Dim MensajeLlamada = ""
    Dim errorCorreos As String = ""
    Dim cad_consolidado As String = "CONSOLIDADO"
    Dim bajo_color As String
    Dim medio_color As String
    Dim alto_color As String
    Dim escaladas_color As String
    Dim noatendio_color As String
    Dim alto_etiqueta As String
    Dim escaladas_etiqueta As String
    Dim noatendio_etiqueta As String
    Dim bajo_hasta As Integer
    Dim medio_hasta As Integer
    Dim nPlanta As String = ""

    Public be_log_activar As Boolean = False


    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim argumentos As String() = Environment.GetCommandLineArgs()
        If Process.GetProcessesByName _
          (Process.GetCurrentProcess.ProcessName).Length > 1 Then
        ElseIf argumentos.Length <= 1 Then
            MsgBox("No se puede iniciar el envío de correos: Se requiere la cadena de conexión", MsgBoxStyle.Critical, "SIGMA Monitor")
        Else
            cadenaConexion = argumentos(1)
            cadenaConexion = "server=localhost;user id=root;password=usbw;port=3307;Convert Zero Datetime=True;Allow User Variables=True"
            Dim idProceso = Process.GetCurrentProcess.Id

            idProceso = Process.GetCurrentProcess.Id



            estadoPrograma = True
            enviarReportes()

        End If
        Application.Exit()
    End Sub

    Private Sub enviarReportes()
        'Se envía correo

        Dim cadSQL As String = "SELECT * FROM " & rutaBD & ".control WHERE fecha = '" & Format(Now, "yyyyMMddHH") & "'"
        Dim readerDS As DataSet = consultaSEL(cadSQL)
        If readerDS.Tables(0).Rows.Count > 0 Then
            'Exit Sub
        End If


        Dim regsAfectados = 0
        'Escalada 4
        Dim miError As String = ""

        Dim correo_cuenta As String
        Dim correo_puerto As String
        Dim correo_ssl As Boolean
        Dim correo_clave As String
        Dim correo_host As String
        Dim rutaFiles As String
        Dim be_envio_reportes As Boolean = False

        cadSQL = "SELECT * FROM " & rutaBD & ".configuracion"
        readerDS = consultaSEL(cadSQL)
        If readerDS.Tables(0).Rows.Count > 0 Then
            Dim reader As DataRow = readerDS.Tables(0).Rows(0)
            correo_cuenta = ValNull(reader!correo_cuenta, "A")
            correo_clave = ValNull(reader!correo_clave, "A")
            correo_puerto = ValNull(reader!correo_puerto, "A")
            correo_ssl = ValNull(reader!correo_ssl, "A") = "S"
            be_envio_reportes = ValNull(reader!be_envio_reportes, "A") = "S"
            correo_host = ValNull(reader!correo_host, "A")
            rutaFiles = ValNull(reader!ruta_archivos_enviar, "A")
            alto_etiqueta = ValNull(reader!alto_etiqueta, "A")
            escaladas_etiqueta = ValNull(reader!escaladas_etiqueta, "A")
            noatendio_etiqueta = ValNull(reader!noatendio_etiqueta, "A")
            cad_consolidado = ValNull(reader!cad_consolidado, "A")
            alto_color = ValNull(reader!alto_color, "A")
            medio_color = ValNull(reader!medio_color, "A")
            bajo_color = ValNull(reader!bajo_color, "A")
            escaladas_color = ValNull(reader!escaladas_color, "A")
            noatendio_color = ValNull(reader!noatendio_color, "A")
            bajo_hasta = ValNull(reader!bajo_hasta, "N")
            medio_hasta = ValNull(reader!medio_hasta, "N")
            be_log_activar = ValNull(reader!be_log_activar, "A") = "S"
            nPlanta = ValNull(reader!planta, "A")
        End If
        If be_envio_reportes Then
            If bajo_hasta = 0 Then bajo_hasta = 50
            If medio_hasta = 0 Then medio_hasta = 75
            If alto_etiqueta.Length = 0 Then alto_etiqueta = "Buenas"
            If escaladas_etiqueta.Length = 0 Then escaladas_etiqueta = "Escaladas"
            If noatendio_etiqueta.Length = 0 Then noatendio_etiqueta = "No atendidas"
            alto_color = "#" & alto_color
            escaladas_color = "#" & escaladas_color
            noatendio_color = "#" & noatendio_color
            If alto_color.Length = 0 Then alto_color = System.Drawing.Color.LimeGreen.ToString
            If escaladas_color.Length = 0 Then escaladas_color = System.Drawing.Color.OrangeRed.ToString
            If noatendio_color.Length = 0 Then noatendio_color = System.Drawing.Color.Tomato.ToString

            If rutaFiles.Length = 0 Then
                rutaFiles = My.Computer.FileSystem.SpecialDirectories.MyDocuments
            Else
                rutaFiles = Strings.Replace(rutaFiles, "/", "\")
                If Not My.Computer.FileSystem.DirectoryExists(rutaFiles) Then
                    Try
                        My.Computer.FileSystem.CreateDirectory(rutaFiles)
                    Catch ex As Exception
                        rutaFiles = My.Computer.FileSystem.SpecialDirectories.MyDocuments
                    End Try
                End If
            End If
            If rutaFiles <> My.Computer.FileSystem.SpecialDirectories.MyDocuments Then
                For Each foundFile As String In My.Computer.FileSystem.GetFiles(
  rutaFiles, Microsoft.VisualBasic.FileIO.SearchOption.SearchTopLevelOnly, "*.png")
                    Try
                        File.Delete(foundFile)
                    Catch ex2 As Exception

                    End Try

                    'Se mueven los archivos a otra carpeta
                Next

                For Each foundFile As String In My.Computer.FileSystem.GetFiles(
  rutaFiles, Microsoft.VisualBasic.FileIO.SearchOption.SearchTopLevelOnly, "*.csv")
                    Try
                        File.Delete(foundFile)
                    Catch ex2 As Exception

                    End Try

                    'Se mueven los archivos a otra carpeta
                Next

            End If
            If Not estadoPrograma Then
                Exit Sub
            End If
            cadSQL = "Select * FROM " & rutaBD & ".cat_correos WHERE estatus = 'A'"
            'Se preselecciona la voz
            Dim indice = 0

            Dim mensajesDS As DataSet = consultaSEL(cadSQL)
            Dim mensajeGenerado = False
            Dim tMensajes = 0

            If mensajesDS.Tables(0).Rows.Count > 0 Then

                Dim enlazado = False
                Dim errorCorreo = ""
                Dim smtpServer As New SmtpClient()

                Try
                    smtpServer.Credentials = New Net.NetworkCredential(correo_cuenta, correo_clave)
                    smtpServer.Port = correo_puerto
                    smtpServer.Host = correo_host '"smtp.live.com" '"smtp.gmail.com"
                    smtpServer.EnableSsl = correo_ssl
                    enlazado = True
                Catch ex As Exception
                    errorCorreo = ex.Message
                End Try
                If enlazado Then

                    For Each elmensaje In mensajesDS.Tables(0).Rows
                        Dim envio = elmensaje!extraccion.Split(New Char() {";"c})
                        'Se busca si hay uno del día y hra
                        If envio(2).Length > 0 And envio(3).Length > 0 Then
                            Dim enviarDia As Boolean = False
                            Dim diaSemana = DateAndTime.Weekday(Now)
                            Dim cadFrecuencia As String = "Este reporte se le envía Todos los días"
                            If envio(2) = "T" Then
                                enviarDia = True
                            ElseIf envio(2) = "LV" And diaSemana >= 2 And diaSemana <= 6 Then
                                enviarDia = True
                                cadFrecuencia = "Este reporte se le envía de lunes a viernes"
                            ElseIf envio(2) = "L" And diaSemana = 2 Then
                                enviarDia = True
                                cadFrecuencia = "Este reporte se le envía los lunes"
                            ElseIf envio(2) = "M" And diaSemana = 3 Then
                                enviarDia = True
                                cadFrecuencia = "Este reporte se le envía los martes"
                            ElseIf envio(2) = "MI" And diaSemana = 4 Then
                                enviarDia = True
                                cadFrecuencia = "Este reporte se le envía los miércoles"
                            ElseIf envio(2) = "J" And diaSemana = 5 Then
                                enviarDia = True
                                cadFrecuencia = "Este reporte se le envía los jueves"
                            ElseIf envio(2) = "V" And diaSemana = 6 Then
                                enviarDia = True
                                cadFrecuencia = "Este reporte se le envía los viernes"
                            ElseIf envio(2) = "S" And diaSemana = 7 Then
                                enviarDia = True
                                cadFrecuencia = "Este reporte se le envía los sábados"
                            ElseIf envio(2) = "D" And diaSemana = 1 Then
                                enviarDia = True
                                cadFrecuencia = "Este reporte se le envía los domingos"
                            ElseIf envio(2) = "1M" And Val(Today.Day) = 1 Then
                                enviarDia = True
                                cadFrecuencia = "Este reporte se le envía el primer día del mes"
                            ElseIf envio(2) = "UM" And Val(Today.Day) = Date.DaysInMonth(Today.Year, Today.Month) Then
                                enviarDia = True
                                cadFrecuencia = "Este reporte se le envía el último día del mes"
                            End If

                            'eemv
                            'enviarDia = True


                            If enviarDia Then
                                Dim enviar As Boolean = False
                                Dim hora = Val(Format(Now, "HH"))
                                If envio(3) = "T" Then
                                    enviar = True
                                    cadFrecuencia = cadFrecuencia & " a cada hora"
                                ElseIf Val(envio(3)) = Val(hora) Then
                                    cadFrecuencia = cadFrecuencia & IIf(Val(hora) = 1, " a la 1:00am", "a las " & Val(hora) & " horas")
                                    enviar = True
                                End If


                                'eemv
                                'enviar = True


                                If enviar Then
                                    Dim mail As New MailMessage
                                    Try
                                        Dim cuerpo As String = ValNull(elmensaje!cuerpo, "A")
                                        Dim titulo As String = ValNull(elmensaje!titulo, "A")
                                        If titulo.Length = 0 Then titulo = "LOGISTICAR Reportes automáticos"
                                        If cuerpo.Length = 0 Then cuerpo = "Se le ha enviado este correo. No responda ya que esta cuenta no es monitoreada"

                                        mail.From = New MailAddress(correo_cuenta) 'TextBox1.Text & "@gmail.com")
                                        Dim mails As String = ValNull(elmensaje!para, "A")
                                        Dim mailsV As String() = mails.Split(New Char() {";"c})
                                        For Each cuenta In mailsV
                                            If cuenta.Length > 0 Then
                                                cuenta = Strings.Replace(cuenta, vbCrLf, "")
                                                cuenta = Strings.Replace(cuenta, vbLf, "")
                                                mail.To.Add(cuenta)
                                            End If
                                        Next
                                        mails = ValNull(elmensaje!copia, "A")
                                        mailsV = mails.Split(New Char() {";"c})
                                        For Each cuenta In mailsV
                                            If cuenta.Length > 0 Then
                                                cuenta = Strings.Replace(cuenta, vbCrLf, "")
                                                cuenta = Strings.Replace(cuenta, vbLf, "")
                                                mail.CC.Add(cuenta)
                                            End If
                                        Next
                                        mails = ValNull(elmensaje!oculta, "A")
                                        mailsV = mails.Split(New Char() {";"c})
                                        For Each cuenta In mailsV
                                            If cuenta.Length > 0 Then
                                                cuenta = Strings.Replace(cuenta, vbCrLf, "")
                                                cuenta = Strings.Replace(cuenta, vbLf, "")
                                                mail.Bcc.Add(cuenta)
                                            End If
                                        Next
                                        mail.Subject = titulo
                                        errorCorreos = ""
                                        cuerpo = cuerpo & vbCrLf & "Reportes a enviar: "

                                        cadSQL = "SELECT a.reporte, b.nombre, b.grafica, b.file_name, b.grafica FROM " & rutaBD & ".det_correo a INNER JOIN " & rutaBD & ".int_listados b ON a.reporte = b.id WHERE a.correo = " & elmensaje!id & " ORDER BY b.orden"
                                        mensajesDS = consultaSEL(cadSQL)
                                        If mensajesDS.Tables(0).Rows.Count > 0 Then

                                            For Each reporte In mensajesDS.Tables(0).Rows
                                                Dim miReporte = generarReporte(reporte!reporte, reporte!nombre, reporte!file_name, envio(0), envio(1), rutaFiles, reporte!grafica)
                                                If miReporte = -1 Then
                                                    cuerpo = cuerpo & vbCrLf & reporte!nombre & " NO SE GENERÓ (por error) " & errorCorreos
                                                Else

                                                    If My.Computer.FileSystem.FileExists(rutaFiles & "\" & reporte!file_name & ".csv") Then
                                                        cuerpo = cuerpo & vbCrLf & reporte!nombre
                                                        Dim archivo As Attachment = New Attachment(rutaFiles & "\" & reporte!file_name & ".csv")
                                                        mail.Attachments.Add(archivo)
                                                    Else
                                                        cuerpo = cuerpo & vbCrLf & reporte!nombre & " NO SE GENERÓ (por datos) "
                                                    End If
                                                    If My.Computer.FileSystem.FileExists(rutaFiles & "\" & reporte!file_name & ".png") Then

                                                        Dim archivo As Attachment = New Attachment(rutaFiles & "\" & reporte!file_name & ".png")
                                                        mail.Attachments.Add(archivo)
                                                    End If
                                                End If
                                            Next
                                        End If
                                        cuerpo = cadFrecuencia & vbCrLf & vbCrLf & cuerpo
                                        mail.Body = cuerpo
                                        smtpServer.Send(mail)
                                        tMensajes = tMensajes + 1
                                        mensajeGenerado = True
                                    Catch ex As Exception
                                        agregarLOG("Error al enviar correos " & ex.Message)
                                    End Try
                                Else
                                    mensajeGenerado = True
                                End If
                            End If
                        End If
                        regsAfectados = consultaACT("UPDATE " & rutaBD & ".cat_correos SET ultimo_envio = '" & Format(Now, "yyyy/MM/dd HH:mm:ss") & "' WHERE id = " & elmensaje!id)
                    Next
                End If
                If enlazado Then
                    If tMensajes > 0 Then
                        agregarLOG("Se enviaron " & tMensajes & " reporte(s) vía correo electrónico ")
                    Else
                        agregarLOG("No se enviaron reportes vía correo electrónico")
                    End If

                Else
                    agregarLOG("Hubo un error en la conexión al servidor de correos. El error es: " & errorCorreo, 0, 9)
                End If
                smtpServer.Dispose()
            End If
            regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".control (fecha, mensajes) VALUES ('" & Format(Now, "yyyyMMddHH") & "', " & tMensajes & ")")
        End If
    End Sub

    Function generarReporte(idReporte As Integer, reporte As String, fName As String, periodo As String, nperiodos As Integer, ruta As String, graficar As String) As Integer
        generarReporte = 0

        Dim archivoSaliente = ruta & "\" & fName & ".csv"
        Dim archivoImagen = ruta & "\" & fName & ".png"

        Try
            My.Computer.FileSystem.DeleteFile(archivoSaliente)


        Catch ex As Exception

        End Try

        Try
            My.Computer.FileSystem.DeleteFile(archivoImagen)


        Catch ex As Exception

        End Try

        Dim eDesde = Now()
        Dim eHasta = Now()
        Dim ePeriodo = nperiodos
        Dim diaSemana = DateAndTime.Weekday(Now)
        Dim intervalo = DateInterval.Second
        Dim cadPeriodo As String = nperiodos & " segundo(s) atras"
        If periodo = 1 Then
            intervalo = DateInterval.Minute
            cadPeriodo = nperiodos & " minuto(s) atras"
        ElseIf periodo = 2 Then
            intervalo = DateInterval.Hour
            cadPeriodo = nperiodos & " hora(s) atras"
        ElseIf periodo = 3 Then
            intervalo = DateInterval.Day
            cadPeriodo = nperiodos & " día(s) atras"
        ElseIf periodo = 4 Then
            intervalo = DateInterval.Day
            ePeriodo = 6
            cadPeriodo = nperiodos & " semana(s) atras"
        ElseIf periodo = 5 Then
            intervalo = DateInterval.Month
            cadPeriodo = nperiodos & " mes(es) atras"
        ElseIf periodo = 6 Then
            intervalo = DateInterval.Year
            cadPeriodo = nperiodos & " año(s) atras"
        ElseIf periodo = 10 Then
            eDesde = CDate(Format(Now, "yyyy/MM/dd") & " 00:00:00")
            cadPeriodo = "Lo que va del día de hoy"
        ElseIf periodo = 11 Then
            cadPeriodo = "Lo que va de la semana"
            If diaSemana = 0 Then
                eDesde = CDate(Format(DateAdd(DateInterval.Day, -6, Now), "yyyy/MM/dd") & " 00:00:00")
            Else
                eDesde = CDate(Format(DateAdd(DateInterval.Day, (diaSemana - 2) * -1, Now), "yyyy/MM/dd") & " 00:00:00")
            End If
        ElseIf periodo = 12 Then
            cadPeriodo = "Lo que va del mes"
            eDesde = CDate(Format(Now, "yyyy/MM") & "/01 00:00:00")
        ElseIf periodo = 13 Then
            cadPeriodo = "Lo que va del a~o"
            eDesde = CDate(Format(Now, "yyyy") & "/01/01 00:00:00")
        ElseIf periodo = 20 Then
            cadPeriodo = "El día de ayer"
            eDesde = CDate(Format(DateAdd(DateInterval.Day, -1, Now), "yyyy/MM/dd") & " 00:00:00")
            eHasta = CDate(Format(DateAdd(DateInterval.Day, -1, Now), "yyyy/MM/dd") & " 23:59:59")
        ElseIf periodo = 21 Then
            cadPeriodo = "La semana pasada"
            Dim dayDiff As Integer = Date.Today.DayOfWeek - DayOfWeek.Monday
            eDesde = CDate(Format(Today.AddDays(-dayDiff), "yyyy/MM/dd") & " 00:00:00")
            eDesde = DateAdd(DateInterval.Day, -7, CDate(eDesde))
            eHasta = DateAdd(DateInterval.Day, 6, CDate(eDesde))
        ElseIf periodo = 22 Then
            cadPeriodo = "El mes pasado"
            eDesde = CDate(Format(DateAdd(DateInterval.Month, -1, Now), "yyyy/MM") & "/01 00:00:00")
            eHasta = CDate(Format(DateAdd(DateInterval.Day, -1, CDate(Format(Now, "yyyy/MM") & "/01")), "yyyy/MM/dd") & " 23:59:59")
        End If
        If periodo < 10 Then eDesde = DateAdd(intervalo, ePeriodo * -1, eDesde)
        Dim fDesde = Format(eDesde, "yyyy/MM/dd") & " 00:00:00"
        Dim fHasta = Format(eHasta, "yyyy/MM/dd") & " 23:59:59"
        Dim fDesdeSF = Format(eDesde, "yyyy/MM/dd")
        Dim fHastaSF = Format(eHasta, "yyyy/MM/dd")


        Dim comillas = Microsoft.VisualBasic.Strings.Left(Chr(34), 1)

        Dim inicial = ""
        Dim cabecera = ""
        Dim registros = ""
        Dim camposPareto = "d.nombre, c.transporte"
        Dim camposGrupo = "d.nombre, c.transporte"
        Dim cadTitulo = "Nombre del transporte"
        Dim cadTabla = "" & rutaBD & ".cat_transportes"
        Dim cadJoin = "c.transporte"
        Dim cadCampo = ""
        Dim numTabla = ""
        Dim grupoFecha = ""
        Dim cadGrafico = ""
        Dim grupoFechaG = ""
        If idReporte = 2 Then
            camposPareto = "d.nombre, c.vehiculo"
            cadTitulo = "Placas del vehículo"
            cadTabla = "" & rutaBD & ".cat_vehiculos"
            cadJoin = "c.chofer"
            camposGrupo = "d.nombre, c.chofer"
        ElseIf idReporte = 3 Then
            camposPareto = "d.nombre, c.chofer"
            cadTitulo = "Nombre del chofer"
            cadTabla = "" & rutaBD & ".cat_choferes"
            cadJoin = "c.chofer"
            camposGrupo = "d.nombre, c.chofer"
        ElseIf idReporte = 4 Then
            camposPareto = "IFNULL(d.nombre, 'N/A') AS nombre, b.tipo"
            cadTitulo = "Tipo de vehículo"
            cadTabla = "" & rutaBD & ".cat_generales"
            cadJoin = "b.tipo"
            camposGrupo = "nombre, b.tipo"
        ElseIf idReporte = 5 Then
            camposPareto = "IFNULL(d.nombre, 'N/A') AS nombre, c.carga"
            cadTitulo = "Tipo de carga"
            cadTabla = "" & rutaBD & ".cat_generales"
            cadJoin = "c.carga"
            camposGrupo = "nombre, c.carga"
        ElseIf idReporte = 6 Then
            camposPareto = "IFNULL(d.nombre, 'N/A') AS nombre, c.area"
            cadTitulo = "Area asociada al beeper"
            cadTabla = "" & rutaBD & ".cat_generales"
            cadJoin = "c.area"
            camposGrupo = "nombre, c.area"
        ElseIf idReporte = 7 Then
            cadTitulo = "Dia"
            camposPareto = "DATE_FORMAT(c.inicio, '%d/%m/%Y') AS nombre, 0 AS dia"
            camposGrupo = "DATE_FORMAT(c.inicio, '%d/%m/%Y'), dia"
        ElseIf idReporte = 8 Then
            cadTitulo = "Semana', 'Dia inicial de la semana"
            camposPareto = "DATE_FORMAT(c.inicio,'%x/%v') AS nombre, STR_TO_DATE(CONCAT(DATE_FORMAT(c.inicio,'%x/%v'), ' Monday'), '%x/%v %W') AS dia"
            camposGrupo = "DATE_FORMAT(c.inicio,'%x/%v') "
        ElseIf idReporte = 9 Then
            cadTitulo = "Mes"
            camposPareto = "DATE_FORMAT(c.inicio,'%Y/%m') AS nombre, 0 AS dia"
            camposGrupo = "DATE_FORMAT(c.inicio,'%Y/%m') "
        ElseIf idReporte = 10 Then
            cadTitulo = "Mes"
            camposPareto = "DATE_FORMAT(c.inicio,'%Y/%m') AS nombre, 0 AS dia"
            camposGrupo = "DATE_FORMAT(c.inicio,'%Y/%m') "


        ElseIf idReporte = 101 Then
            camposPareto = "d.nombre, c.transporte"
            cadTitulo = "Nombre del chofer"
            cadTabla = "" & rutaBD & ".cat_transportes"
            cadJoin = "c.transporte"
            camposGrupo = "d.nombre, c.transporte"
        ElseIf idReporte = 102 Then
            camposPareto = "IFNULL(d.nombre, 'N/A') AS nombre, b.tipo"
            cadTitulo = "Tipo de vehículo"
            cadTabla = "" & rutaBD & ".cat_generales"
            cadJoin = "b.tipo"
            camposGrupo = "nombre, b.tipo"
        ElseIf idReporte = 103 Then
            camposPareto = "IFNULL(d.nombre, 'N/A') AS nombre, c.carga"
            cadTitulo = "Tipo de carga"
            cadTabla = "" & rutaBD & ".cat_generales"
            cadJoin = "c.carga"
            camposGrupo = "nombre, c.carga"
        ElseIf idReporte = 105 Then
            camposPareto = "IFNULL(d.nombre, 'N/A') AS nombre, c.area"
            cadTitulo = "Area asociada al beeper"
            cadTabla = "" & rutaBD & ".cat_generales"
            cadJoin = "c.area"
            camposGrupo = "nombre, c.area"
        ElseIf idReporte = 104 Then
            camposPareto = "d.nombre, c.destino"
            cadTitulo = "Nombre del destino"
            cadTabla = "" & rutaBD & ".cat_destinos"
            cadJoin = "c.destino"
            camposGrupo = "d.nombre, c.destino"
        ElseIf idReporte = 106 Then
            cadTitulo = "Dia"
            camposPareto = "DATE_FORMAT(c.inicio, '%d/%m/%Y') AS nombre, 0 AS dia"
            camposGrupo = "DATE_FORMAT(c.inicio, '%d/%m/%Y'), dia"
        ElseIf idReporte = 107 Then
            cadTitulo = "Semana', 'Dia inicial de la semana"
            camposPareto = "DATE_FORMAT(c.inicio,'%x/%v') AS nombre, STR_TO_DATE(CONCAT(DATE_FORMAT(c.inicio,'%x/%v'), ' Monday'), '%x/%v %W') AS dia"
            camposGrupo = "DATE_FORMAT(c.inicio,'%x/%v') "
        ElseIf idReporte = 108 Then
            cadTitulo = "Mes"
            camposPareto = "DATE_FORMAT(c.inicio,'%Y/%m') AS nombre, 0 AS dia"
            camposGrupo = "DATE_FORMAT(c.inicio,'%Y/%m') "

        End If
        inicial = "Logisticar" & vbCrLf & nPlanta & vbCrLf & reporte & vbCrLf & cadPeriodo & vbCrLf & "Desde: " & Format(eDesde, "dd/MMM/yyyy HH:mm:ss") & ", hasta: " & Format(eHasta, "dd/MMM/yyyy HH:mm:ss") & vbCrLf

        Dim Leer As Boolean = False

        If idReporte = 1 Then
            cabecera = "Nombre del transporte, ID del transporte, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo total (Hr), Porcentaje "
        ElseIf idReporte = 2 Then
            cabecera = "Placas del vehículo, Tipo de vehículo, Transporte asociado, ID del vehiculo, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo total (Hr), Porcentaje "
        ElseIf idReporte = 3 Then
            cabecera = "Nombre del chofer, ID del chofer, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo total (Hr), Porcentaje "
        ElseIf idReporte = 4 Then
            cabecera = "Tipo de vehiculo, ID del tipo, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo total (Hr), Porcentaje "
        ElseIf idReporte = 5 Then
            cabecera = "Tipo de carga, ID del tipo de carga, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo total (Hr), Porcentaje "
        ElseIf idReporte = 6 Then
            cabecera = "Area del beeper, ID del area, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo total (Hr), Porcentaje "
        ElseIf idReporte = 7 Then
            cabecera = "Dia, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo total (Hr), Porcentaje "
        ElseIf idReporte = 8 Then
            cabecera = "A~o/Semana, Dia que inicio la semana, ID del area, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo total (Hr), Porcentaje "
        ElseIf idReporte = 9 Then
            cabecera = "A~o/Mes, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo total (Hr), Porcentaje "
        ElseIf idReporte = 10 Then
            cabecera = "Placas, ID, Tacto, Estatus del tacto, Origen, Destino, Inicio del traslado, Fin del traslado, Tiempo estimado de traslado (seg), Tiempo real del traslado (seg), Se alarmó el tiempo de traslado?, Inicio de la descarga, Fin de la  descarga, Tiempo estimado de la descarga (seg), Tiempo real de la descarga (seg), Se alarmo la descarga?, Tiempo TOTAL traslado+descarga (seg), Transporte, Nombre del chofer, Tipo de Carga, Tipo de vehiculo"
        ElseIf idReporte = 101 Then
            cabecera = "Nombre del transporte, ID del transporte, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo de espera (Hr), Tiempo total (Hr), Eficiencia (%) "
        ElseIf idReporte = 102 Then
            cabecera = "Tipo de vehiculo, ID del tipo, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo de espera (Hr), Tiempo total (Hr), Eficiencia (%) "
        ElseIf idReporte = 103 Then
            cabecera = "Tipo de carga, ID del tipo de carga, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo de espera (Hr), Tiempo total (Hr), Eficiencia (%) "
        ElseIf idReporte = 105 Then
            cabecera = "Area del beeper, ID del area, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo de espera (Hr), Tiempo total (Hr), Eficiencia (%) "
        ElseIf idReporte = 104 Then
            cabecera = "Destino, ID del destino, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo de espera (Hr), Tiempo total (Hr), Eficiencia (%) "

        ElseIf idReporte = 106 Then
            cabecera = "Dia, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo de espera (Hr), Tiempo total (Hr), Eficiencia (%) "
        ElseIf idReporte = 107 Then
            cabecera = "A~o/Semana, Dia que inicio la semana, ID del area, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo de espera (Hr), Tiempo total (Hr), Eficiencia (%) "
        ElseIf idReporte = 108 Then
            cabecera = "A~o/Mes, Tactos, Tiempo de traslado (Hr), Tiempo de descarga (Hr), Tiempo de espera (Hr), Tiempo total (Hr), Eficiencia (%) "
        End If

        Dim regsAfectados = 0

        If graficar <> "S" Then
            registros = "SELECT b.nombre, a.id, a.viaje, CASE WHEN a.estatus = 0 THEN 'En curso' WHEN a.estatus = 1 THEN 'Descargando' WHEN a.estatus = 2 THEN 'Terminado' WHEN a.estatus = 9 THEN 'Cancelado' END AS estatus, IFNULL(d.nombre, 'N/A') AS dOrigen, IFNULL(e.nombre, 'N/A') AS dDestino, a.inicio, a.fin, a.estimado, TIME_TO_SEC(TIMEDIFF(IF(a.estatus > 0, fin, NOW()), inicio)) AS tiempo, a.alarmado, a.des_inicio, a.des_fin, a.des_estimado, TIME_TO_SEC(TIMEDIFF(IF(a.estatus > 1, des_fin, NOW()), des_inicio)) AS des_tiempo, a.des_alarmado, TIME_TO_SEC(TIMEDIFF(IF(a.estatus > 0, fin, NOW()), inicio)) + TIME_TO_SEC(TIMEDIFF(IF(a.estatus > 1, des_fin, NOW()), des_inicio)), IFNULL(f.nombre, 'N/A') AS ntransporte, IFNULL(h.nombre, 'N/A') AS nchofer, IFNULL(i.nombre, 'N/A') AS ncarga, IFNULL(j.nombre, 'N/A') AS ntipo FROM " & rutaBD & ".movimientos_det a LEFT JOIN " & rutaBD & ".cat_destinos d ON a.origen = d.id LEFT JOIN " & rutaBD & ".cat_vehiculos b ON a.vehiculo = b.id LEFT JOIN " & rutaBD & ".cat_destinos e ON a.destino = e.id LEFT JOIN " & rutaBD & ".cat_transportes f ON a.transporte = f.id LEFT JOIN " & rutaBD & ".cat_choferes h ON a.chofer = h.id LEFT JOIN " & rutaBD & ".cat_generales i ON a.carga = i.id LEFT JOIN " & rutaBD & ".cat_generales j ON b.tipo = j.id WHERE a.inicio >= '" & fDesde & "' AND a.inicio <= '" & fHasta & "' ORDER by a.id"

            Dim listado As DataSet = consultaSEL(registros)
            If listado.Tables(0).Rows.Count > 0 Then
                Try
                    Dim file As System.IO.StreamWriter
                    file = My.Computer.FileSystem.OpenTextFileWriter(archivoSaliente, True)
                    file.WriteLine(inicial & cabecera)

                    For Each lineas In listado.Tables(0).Rows
                        Dim cadFile = ""
                        For i = 0 To 20
                            cadFile = cadFile & lineas(i) & ","
                        Next
                        file.WriteLine(cadFile)

                    Next
                    file.Close()
                Catch ex As Exception

                End Try

            End If


        Else
            'Se produce el gráfico
            Dim cadSQL = ""
            cadSQL = "SELECT * FROM " & rutaBD & ".pu_graficos WHERE (usuario = 1 OR usuario = 0) AND grafico = " & 100 + idReporte & " ORDER BY usuario DESC LIMIT 1"
            Dim config As DataSet = consultaSEL(cadSQL)
            regsAfectados = 0
            If errorBD.Length > 0 Then
                agregarLOG("Ocurrió un error al intentar leer MySQL. Error: " + Microsoft.VisualBasic.Strings.Left(errorBD, 250), 9, 0)
                errorCorreos = errorBD
                generarReporte = -1
            Else
                If config.Tables(0).Rows.Count > 0 Then
                    Dim cadOrden As String = " ORDER BY 3"
                    Dim indicador01 = "Tactos"

                    If config.Tables(0).Rows(0)!orden = 1 Then
                        indicador01 = "Tiempo de traslado"
                        cadOrden = " ORDER BY 4"
                    ElseIf config.Tables(0).Rows(0)!orden = 2 Then
                        indicador01 = "Tiempo de descarga"
                        cadOrden = " ORDER BY 5"
                    ElseIf config.Tables(0).Rows(0)!orden = 3 Then
                        indicador01 = "Tiempo total"
                        cadOrden = " ORDER BY 6"
                    End If
                    If config.Tables(0).Rows(0)!orden_grafica = "M" Then
                        cadOrden = cadOrden & " DESC"
                    ElseIf config.Tables(0).Rows(0)!orden_grafica = "M" Then
                        cadOrden = " ORDER BY 1"
                    End If
                    registros = "SELECT " & camposPareto & ", COUNT(*) AS docs, SUM(c.tiempo) / 3600 AS tiempo, SUM(c.des_tiempo) / 3600 AS des_tiempo, SUM(c.tiempo + c.des_tiempo) / 3600 AS total_tiempo FROM " & rutaBD & ".movimientos_det c INNER JOIN " & rutaBD & ".cat_vehiculos b ON c.vehiculo = b.id LEFT JOIN " & cadTabla & " d ON " & cadJoin & "  = d.id WHERE c.inicio >= '" & fDesde & "' AND c.inicio <= '" & fHasta & "' GROUP BY " & camposGrupo & cadOrden
                    If idReporte = 2 Then
                        registros = "SELECT b.nombre, c.vehiculo, COUNT(*) AS docs, SUM(c.tiempo) / 3600 AS tiempo, SUM(c.des_tiempo) / 3600 AS des_tiempo, SUM(c.tiempo + c.des_tiempo) / 3600 AS total_tiempo, IFNULL(e.nombre, 'N/A') AS ntipo, IFNULL(f.nombre, 'N/A') AS ntransporte FROM " & rutaBD & ".movimientos_det c LEFT JOIN " & rutaBD & ".cat_vehiculos b ON c.vehiculo = b.id LEFT JOIN " & rutaBD & ".cat_generales e ON b.tipo = e.id LEFT JOIN " & rutaBD & ".cat_transportes f ON b.linea = f.id WHERE c.inicio >= '" & fDesde & "' AND c.inicio <= '" & fHasta & "' GROUP BY b.nombre, c.vehiculo, ntipo, ntransporte " & cadOrden
                    ElseIf idReporte >= 7 And idReporte < 100 Then
                        registros = "SELECT " & camposPareto & ", COUNT(*) AS docs, SUM(c.tiempo) / 3600 AS tiempo, SUM(c.des_tiempo) / 3600 AS des_tiempo, SUM(c.tiempo + c.des_tiempo) / 3600 AS total_tiempo FROM " & rutaBD & ".movimientos_det c INNER JOIN " & rutaBD & ".cat_vehiculos b ON c.vehiculo = b.id WHERE c.inicio >= '" & fDesde & "' AND c.inicio <= '" & fHasta & "' GROUP BY " & camposGrupo & cadOrden
                    ElseIf idReporte <= 105 Then
                        registros = "SELECT " & camposPareto & ", COUNT(*) AS docs, SUM(c.tiempo) / 3600 AS tiempo, SUM(c.des_tiempo) / 3600 AS des_tiempo, SUM(c.espera) / 3600 AS espera, SUM(c.tiempo + c.des_tiempo + c.espera) / 3600 AS total_tiempo FROM " & rutaBD & ".movimientos_det c INNER JOIN " & rutaBD & ".cat_vehiculos b ON c.vehiculo = b.id LEFT JOIN " & cadTabla & " d ON " & cadJoin & "  = d.id WHERE c.inicio >= '" & fDesde & "' AND c.inicio <= '" & fHasta & "' GROUP BY " & camposGrupo & cadOrden
                    ElseIf idReporte > 105 Then
                        registros = "SELECT " & camposPareto & ", COUNT(*) AS docs, SUM(c.tiempo) / 3600 AS tiempo, SUM(c.des_tiempo) / 3600 AS des_tiempo, SUM(c.espera) / 3600 AS espera, SUM(c.tiempo + c.des_tiempo + c.espera) / 3600 AS total_tiempo FROM " & rutaBD & ".movimientos_det c INNER JOIN " & rutaBD & ".cat_vehiculos b ON c.vehiculo = b.id WHERE c.inicio >= '" & fDesde & "' AND c.inicio <= '" & fHasta & "' GROUP BY " & camposGrupo & cadOrden

                    End If
                    Dim indicador02 = "Porcentaje"
                    Dim indicador03 = ""
                    Dim indicador04 = ""

                    If idReporte > 100 Then
                        indicador01 = "Tránsito"
                        indicador02 = "Descarga"
                        indicador03 = "Espera"
                        indicador04 = "Eficiencia"
                    End If

                    Dim graficos As DataSet = consultaSEL(registros)
                    If graficos.Tables(0).Rows.Count > 0 Then
                        ChartControl1.Series.Clear()
                        ChartControl1.Titles.Clear()
                        Dim Titulo As New ChartTitle()
                        Titulo.Text = config.Tables(0).Rows(0)!titulo & Strings.Space(10)
                        Dim miFuente = New Drawing.Font("Lucida Sans", 10, FontStyle.Regular)
                        Dim miFuenteAlto = New Drawing.Font("Lucida Sans", 16, FontStyle.Bold)
                        Dim miFuenteEjes = New Drawing.Font("Lucida Sans", 11, FontStyle.Regular)

                        If idReporte < 100 Then
                            Titulo.Font = miFuenteAlto
                            Dim tabla_grafico As New DataTable("grafico")

                            ' Create an empty table.
                            Dim datos As New DataTable("grafico")
                            Dim row As DataRow = Nothing
                            datos.Columns.Add("orden", GetType(String))
                            datos.Columns.Add("estacion", GetType(String))
                            datos.Columns.Add("total", GetType(Decimal))
                            datos.Columns.Add("pct", GetType(Decimal))
                            tabla_grafico.Columns.Add("orden", GetType(String))
                            tabla_grafico.Columns.Add("estacion", GetType(String))
                            tabla_grafico.Columns.Add("total", GetType(Decimal))
                            tabla_grafico.Columns.Add("pct", GetType(Decimal))

                            Dim totalValores = 0
                            Dim totalLineas = 0
                            For Each lineas In graficos.Tables(0).Rows
                                If config.Tables(0).Rows(0)!orden = 0 Then
                                    totalValores = totalValores + lineas!docs
                                ElseIf config.Tables(0).Rows(0)!orden = 1 Then
                                    totalValores = totalValores + lineas!tiempo
                                ElseIf config.Tables(0).Rows(0)!orden = 2 Then
                                    totalValores = totalValores + lineas!des_tiempo
                                Else
                                    totalValores = totalValores + lineas!total_tiempo
                                End If
                            Next
                            Dim pctAcumulado = 0
                            For Each lineas In graficos.Tables(0).Rows
                                row = datos.NewRow()
                                row("orden") = "A"
                                row("estacion") = lineas!nombre

                                If config.Tables(0).Rows(0)!orden = 0 Then
                                    row("total") = lineas!docs
                                ElseIf config.Tables(0).Rows(0)!orden = 1 Then
                                    row("total") = lineas!tiempo
                                ElseIf config.Tables(0).Rows(0)!orden = 2 Then
                                    row("total") = lineas!des_tiempo
                                Else
                                    row("total") = lineas!total_tiempo
                                End If
                                pctAcumulado = pctAcumulado + row("total")
                                row("pct") = pctAcumulado / totalValores * 100
                                datos.Rows.Add(row)
                            Next
                            Try
                                Dim file As System.IO.StreamWriter
                                file = My.Computer.FileSystem.OpenTextFileWriter(archivoSaliente, True)
                                file.WriteLine(inicial & cabecera)

                                Dim lineaNum
                                pctAcumulado = 0
                                For Each lineas In graficos.Tables(0).Rows
                                    If config.Tables(0).Rows(0)!orden = 0 Then
                                        lineaNum = lineas!docs
                                    ElseIf config.Tables(0).Rows(0)!orden = 1 Then
                                        lineaNum = lineas!tiempo
                                    ElseIf config.Tables(0).Rows(0)!orden = 2 Then
                                        lineaNum = lineas!des_tiempo
                                    Else
                                        lineaNum = lineas!total_tiempo
                                    End If
                                    pctAcumulado = pctAcumulado + lineaNum
                                    If idReporte = 2 Then
                                        file.WriteLine(lineas(0) & ", " & lineas(1) & ", " & lineas(6) & ", " & lineas(7) & ", " & lineas(2) & ", " & lineas(3) & ", " & lineas(4) & ", " & lineas(5) & "," & pctAcumulado / totalValores * 100)
                                    ElseIf idReporte = 7 Or idReporte = 9 Then
                                        file.WriteLine(lineas(0) & ", " & lineas(2) & ", " & lineas(3) & ", " & lineas(4) & ", " & lineas(5) & "," & pctAcumulado / totalValores * 100)
                                    Else
                                        file.WriteLine(lineas(0) & ", " & lineas(1) & ", " & lineas(2) & ", " & lineas(3) & ", " & lineas(4) & ", " & lineas(5) & "," & pctAcumulado / totalValores * 100)
                                    End If

                                Next
                                file.Close()

                            Catch ex As Exception

                            End Try

                            If config.Tables(0).Rows(0)!maximo_barras > 0 Or config.Tables(0).Rows(0)!maximo_barraspct > 0 Then
                                Dim TotalVal = 0
                                Dim tBarras = 0
                                Dim valAcum = 0
                                Dim varios = 0
                                Dim valAgrupado = 0
                                Dim pctAgrupado = 0
                                For Each filas In datos.Rows
                                    TotalVal = TotalVal + filas!total
                                    tBarras = tBarras + 1
                                Next
                                If tBarras > config.Tables(0).Rows(0)!maximo_barras Or (config.Tables(0).Rows(0)!maximo_barraspct > 0 And config.Tables(0).Rows(0)!maximo_barraspct < 100) Then
                                    Dim view As DataView = New DataView(datos)
                                    Dim tmpTabla As New DataTable("grafico")
                                    ' Create an empty table.
                                    tmpTabla.Columns.Add("orden", GetType(String))
                                    tmpTabla.Columns.Add("estacion", GetType(String))
                                    tmpTabla.Columns.Add("total", GetType(Decimal))
                                    tmpTabla.Columns.Add("pct", GetType(Decimal))


                                    view.Sort = IIf(config.Tables(0).Rows(0)!orden_grafica = "M", " total DESC", IIf(config.Tables(0).Rows(0)!orden_grafica = "N", " total", "estacion"))
                                    Dim tabla_ordenada As DataTable = view.ToTable()
                                    tBarras = 0
                                    valAcum = 0
                                    valAgrupado = 0
                                    For Each filas In tabla_ordenada.Rows
                                        valAcum = valAcum + filas!total
                                        tBarras = tBarras + 1
                                        If tBarras > config.Tables(0).Rows(0)!maximo_barras Or ((valAcum / TotalVal * 100) > config.Tables(0).Rows(0)!maximo_barraspct And config.Tables(0).Rows(0)!maximo_barraspct > 0) Then
                                            valAgrupado = valAgrupado + filas!total
                                            pctAgrupado = pctAgrupado + filas!pct
                                            varios = varios + 1
                                        Else
                                            row = tmpTabla.NewRow()
                                            row("orden") = "B"
                                            row("estacion") = filas!estacion
                                            row("total") = filas!total
                                            row("pct") = filas!pct
                                            tmpTabla.Rows.Add(row)
                                        End If
                                    Next
                                    If valAgrupado > 0 And config.Tables(0).Rows(0)!agrupar = "S" Then
                                        Dim cadAgrupado As String = ValNull(config.Tables(0).Rows(0)!agrupar_texto, "A")
                                        If cadAgrupado.Length = 0 Then cadAgrupado = "AGRUPADO"
                                        cadAgrupado = cadAgrupado & " (" & varios & ")"
                                        row = tmpTabla.NewRow()
                                        If config.Tables(0).Rows(0)!agrupar_posicion = "P" Then
                                            row("orden") = "A"
                                        ElseIf config.Tables(0).Rows(0)!agrupar_posicion = "F" Then
                                            row("orden") = "Z"
                                        End If
                                        row("estacion") = cadAgrupado
                                        row("total") = valAgrupado
                                        row("pct") = valAgrupado
                                        tmpTabla.Rows.Add(row)
                                    End If
                                    Dim view_o As DataView = New DataView(tmpTabla)
                                    If config.Tables(0).Rows(0)!agrupar_posicion = "O" Then
                                        view_o.Sort = IIf(config.Tables(0).Rows(0)!orden_grafica = "M", " total DESC", IIf(config.Tables(0).Rows(0)!orden_grafica = "N", " total", "estacion"))

                                    Else
                                        view_o.Sort = "orden ASC," & IIf(config.Tables(0).Rows(0)!orden_grafica = "M", " total DESC", IIf(config.Tables(0).Rows(0)!orden_grafica = "N", " total", "estacion"))
                                    End If
                                    tabla_grafico = view_o.ToTable()
                                Else
                                    tabla_grafico = datos
                                End If
                            Else
                                tabla_grafico = datos
                            End If
                            Dim series1 As New Series(indicador01, ViewType.Bar)

                            ChartControl1.Series.Add(series1)
                            series1.DataSource = tabla_grafico
                            series1.LabelsVisibility = DevExpress.Utils.DefaultBoolean.True
                            series1.View.Color = Color.SkyBlue
                            series1.ArgumentScaleType = ScaleType.Qualitative
                            series1.ArgumentDataMember = "estacion"
                            series1.ValueScaleType = ScaleType.Numerical
                            series1.ValueDataMembers.AddRange(New String() {"total"})
                            series1.Label.BackColor = Color.DarkBlue
                            series1.Label.TextColor = Color.White
                            series1.Label.Font = miFuente
                            series1.Label.TextPattern = "{V:F1}"

                            Dim series2 As New Series(indicador02, ViewType.Spline)

                            ChartControl1.Series.Add(series2)
                            series2.DataSource = tabla_grafico
                            series2.LabelsVisibility = DevExpress.Utils.DefaultBoolean.True
                            series2.View.Color = Color.Green
                            series2.ArgumentScaleType = ScaleType.Qualitative
                            series2.ArgumentDataMember = "estacion"
                            series2.ValueScaleType = ScaleType.Numerical
                            series2.ValueDataMembers.AddRange(New String() {"pct"})
                            series2.Label.BackColor = Color.DarkBlue
                            series2.Label.TextColor = Color.White
                            series2.Label.Font = miFuente
                            series2.Label.TextPattern = "{V:F1}"

                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Visibility = DevExpress.Utils.DefaultBoolean.True
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Label.Font = miFuenteEjes
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.GridSpacingAuto = False
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.GridSpacing = 1
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Title.Text = config.Tables(0).Rows(0)!texto_y
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Title.Font = miFuenteAlto
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Title.Visibility = DevExpress.Utils.DefaultBoolean.True
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.GridLines.Visible = False
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Tickmarks.Visible = False
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Tickmarks.MinorVisible = False

                            Dim myAxisY As New SecondaryAxisY("PCT")
                            CType(ChartControl1.Diagram, XYDiagram).SecondaryAxesY.Clear()
                            CType(ChartControl1.Diagram, XYDiagram).SecondaryAxesY.Add(myAxisY)
                            CType(series2.View, LineSeriesView).AxisY = myAxisY
                            myAxisY.Title.Text = config.Tables(0).Rows(0)!texto_z
                            myAxisY.Title.Visible = True
                            myAxisY.Label.Font = miFuenteEjes
                            myAxisY.Title.Font = miFuenteAlto
                            myAxisY.GridLines.Visible = False
                            myAxisY.Tickmarks.Visible = False
                            myAxisY.Tickmarks.MinorVisible = False
                            myAxisY.Title.TextColor = Color.Green
                            myAxisY.Label.TextColor = Color.Green
                            myAxisY.Color = Color.Green

                            CType(ChartControl1.Diagram, XYDiagram).AxisX.GridLines.Visible = False
                            CType(ChartControl1.Diagram, XYDiagram).AxisX.Label.Font = miFuenteEjes
                            CType(ChartControl1.Diagram, XYDiagram).AxisX.Title.Text = Strings.Space(5) & config.Tables(0).Rows(0)!texto_x & Strings.Space(10)
                            CType(ChartControl1.Diagram, XYDiagram).AxisX.GridLines.Visible = False
                            CType(ChartControl1.Diagram, XYDiagram).AxisX.Title.Font = miFuenteAlto
                            CType(ChartControl1.Diagram, XYDiagram).AxisX.Title.Visibility = DevExpress.Utils.DefaultBoolean.True
                            If config.Tables(0).Rows(0)!overlap = "R" Then
                                CType(ChartControl1.Diagram, XYDiagram).AxisX.Label.ResolveOverlappingOptions.AllowStagger = False
                                CType(ChartControl1.Diagram, XYDiagram).AxisX.Label.ResolveOverlappingOptions.AllowRotate = True
                            Else
                                CType(ChartControl1.Diagram, XYDiagram).AxisX.Label.ResolveOverlappingOptions.AllowStagger = True
                                CType(ChartControl1.Diagram, XYDiagram).AxisX.Label.ResolveOverlappingOptions.AllowRotate = False
                            End If


                            ChartControl1.Titles.Add(Titulo)
                            Dim Titulo2 As New ChartTitle()

                            Titulo2.Font = miFuente
                            Titulo2.Text = "Extraccion de datos: " & cadPeriodo
                            ChartControl1.Titles.Add(Titulo2)
                            Dim Titulo3 As New ChartTitle()
                            Titulo3.Font = miFuente
                            Titulo3.Text = "Generado el: " & Format(Now, "ddd dd-MM-yyyy HH:mm:ss")
                            ChartControl1.Titles.Add(Titulo3)
                            Dim Titulo4 As New ChartTitle()
                            Titulo4.Font = miFuente
                            Titulo4.Text = "Extrayendo los datos desde: " & Format(eDesde, "dd-MM-yyyy HH:mm:ss") & " hasta: " &
                                            Format(eHasta, "dd-MM-yyyy HH:mm:ss")
                            ChartControl1.Titles.Add(Titulo4)
                            ChartControl1.Width = 1000
                            ChartControl1.Height = 700

                            If config.Tables(0).Rows(0)!ver_leyenda = "S" Then
                                ChartControl1.Legend.Visibility = DevExpress.Utils.DefaultBoolean.True
                            Else
                                ChartControl1.Legend.Visibility = DevExpress.Utils.DefaultBoolean.False
                            End If
                            Try
                                Dim rutaImagen = Microsoft.VisualBasic.Strings.Replace(archivoImagen, "\", "\\")
                                SaveChartImageToFile(ChartControl1, ImageFormat.Png, rutaImagen)
                                Dim image As Image = GetChartImage(ChartControl1, ImageFormat.Png)
                                image.Save(rutaImagen)

                            Catch ex As Exception
                                agregarLOG("Ocurrió un error al intentar construir un archivo de adjunto de reporte (gráfico). Error: " & ex.Message, 7, 0)
                            End Try
                        ElseIf idReporte >= 100 Then
                            Titulo.Font = miFuenteAlto
                            Dim tabla_grafico As New DataTable("grafico")

                            ' Create an empty table.
                            Dim datos As New DataTable("grafico")
                            Dim row As DataRow = Nothing

                            datos.Columns.Add("orden", GetType(String))
                            datos.Columns.Add("tactos", GetType(Decimal))
                            datos.Columns.Add("estacion", GetType(String))
                            datos.Columns.Add("espera", GetType(Decimal))
                            datos.Columns.Add("tiempo", GetType(Decimal))
                            datos.Columns.Add("des_tiempo", GetType(Decimal))
                            datos.Columns.Add("total_tiempo", GetType(Decimal))
                            datos.Columns.Add("eficiencia", GetType(Decimal))

                            tabla_grafico.Columns.Add("orden", GetType(String))
                            tabla_grafico.Columns.Add("tactos", GetType(Decimal))
                            tabla_grafico.Columns.Add("estacion", GetType(String))
                            tabla_grafico.Columns.Add("espera", GetType(Decimal))
                            tabla_grafico.Columns.Add("tiempo", GetType(Decimal))
                            tabla_grafico.Columns.Add("des_tiempo", GetType(Decimal))
                            tabla_grafico.Columns.Add("total_tiempo", GetType(Decimal))
                            tabla_grafico.Columns.Add("eficiencia", GetType(Decimal))

                            For Each lineas In graficos.Tables(0).Rows
                                row = datos.NewRow()
                                row("orden") = "A"
                                row("espera") = lineas!espera
                                row("estacion") = lineas!nombre
                                row("tiempo") = lineas!tiempo
                                row("des_tiempo") = lineas!des_tiempo
                                row("total_tiempo") = lineas!total_tiempo
                                row("tactos") = lineas!docs
                                row("eficiencia") = (row("total_tiempo") - row("espera")) / row("total_tiempo") * 100
                                datos.Rows.Add(row)
                            Next
                            Try
                                Dim file As System.IO.StreamWriter
                                file = My.Computer.FileSystem.OpenTextFileWriter(archivoSaliente, True)
                                file.WriteLine(inicial & cabecera)

                                For Each lineas In graficos.Tables(0).Rows
                                    If idReporte = 106 Or idReporte = 108 Then
                                        file.WriteLine(lineas(0) & ", " & lineas(2) & ", " & lineas(3) & ", " & lineas(4) & ", " & lineas(5) & ", " & lineas(6) & ", " & (lineas(3) + lineas(4)) / lineas(6) * 100)
                                    Else
                                        file.WriteLine(lineas(0) & ", " & lineas(1) & ", " & lineas(2) & ", " & lineas(3) & ", " & lineas(4) & ", " & lineas(5) & ", " & lineas(6) & ", " & (lineas(3) + lineas(4)) / lineas(6) * 100)
                                    End If

                                Next
                                file.Close()

                            Catch ex As Exception

                            End Try

                            If config.Tables(0).Rows(0)!maximo_barras > 0 Or config.Tables(0).Rows(0)!maximo_barraspct > 0 Then
                                Dim TotalVal = 0
                                Dim tBarras = 0
                                Dim valAcum = 0
                                Dim varios = 0
                                Dim valAgrupado = 0
                                Dim pctAgrupado = 0
                                For Each filas In datos.Rows
                                    TotalVal = TotalVal + filas!total
                                    tBarras = tBarras + 1
                                Next
                                If tBarras > config.Tables(0).Rows(0)!maximo_barras Or (config.Tables(0).Rows(0)!maximo_barraspct > 0 And config.Tables(0).Rows(0)!maximo_barraspct < 100) Then
                                    Dim view As DataView = New DataView(datos)
                                    Dim tmpTabla As New DataTable("grafico")
                                    ' Create an empty table.

                                    tmpTabla.Columns.Add("orden", GetType(String))
                                    tmpTabla.Columns.Add("tactos", GetType(Decimal))
                                    tmpTabla.Columns.Add("estacion", GetType(String))
                                    tmpTabla.Columns.Add("espera", GetType(Decimal))
                                    tmpTabla.Columns.Add("tiempo", GetType(Decimal))
                                    tmpTabla.Columns.Add("des_tiempo", GetType(Decimal))
                                    tmpTabla.Columns.Add("total_tiempo", GetType(Decimal))
                                    tmpTabla.Columns.Add("eficiencia", GetType(Decimal))

                                    view.Sort = IIf(config.Tables(0).Rows(0)!orden_grafica = "M", " total_tiempo DESC", IIf(config.Tables(0).Rows(0)!orden_grafica = "N", " total_tiempo", "estacion"))
                                    Dim tabla_ordenada As DataTable = view.ToTable()
                                    tBarras = 0
                                    valAcum = 0
                                    valAgrupado = 0
                                    Dim valAgrupado2 = 0, valAgrupado3 = 0, valAgrupado4 = 0, valAgrupado5 = 0
                                    For Each filas In tabla_ordenada.Rows
                                        valAcum = valAcum + filas!total_tiempo
                                        tBarras = tBarras + 1
                                        If tBarras > config.Tables(0).Rows(0)!maximo_barras Or ((valAcum / TotalVal * 100) > config.Tables(0).Rows(0)!maximo_barraspct And config.Tables(0).Rows(0)!maximo_barraspct > 0) Then
                                            valAgrupado = valAgrupado + filas!total_tiempo
                                            valAgrupado2 = valAgrupado2 + filas!espera
                                            valAgrupado3 = valAgrupado3 + filas!des_tiempo
                                            valAgrupado4 = valAgrupado4 + filas!tiempo
                                            valAgrupado5 = valAgrupado5 + filas!docs
                                            varios = varios + 1
                                        Else
                                            row = tmpTabla.NewRow()

                                            row("orden") = "A"
                                            row("estacion") = filas!estacion
                                            row("espera") = filas!espera
                                            row("tiempo") = filas!tiempo
                                            row("des_tiempo") = filas!des_tiempo
                                            row("total_tiempo") = filas!total_tiempo
                                            row("tactos") = filas!docs
                                            row("eficiencia") = (row("total_tiempo") - row("espera")) / row("total_tiempo") * 100
                                            tmpTabla.Rows.Add(row)
                                        End If
                                    Next
                                    If valAgrupado > 0 And config.Tables(0).Rows(0)!agrupar = "S" Then
                                        Dim cadAgrupado As String = ValNull(config.Tables(0).Rows(0)!agrupar_texto, "A")
                                        If cadAgrupado.Length = 0 Then cadAgrupado = "AGRUPADO"
                                        cadAgrupado = cadAgrupado & " (" & varios & ")"
                                        row = tmpTabla.NewRow()
                                        If config.Tables(0).Rows(0)!agrupar_posicion = "P" Then
                                            row("orden") = "A"
                                        ElseIf config.Tables(0).Rows(0)!agrupar_posicion = "F" Then
                                            row("orden") = "Z"
                                        End If

                                        row("estacion") = cadAgrupado
                                        row("espera") = valAgrupado2
                                        row("tiempo") = valAgrupado4
                                        row("des_tiempo") = valAgrupado3
                                        row("total_tiempo") = valAgrupado
                                        row("tactos") = valAgrupado5
                                        row("eficiencia") = (valAgrupado - valAgrupado2) / valAgrupado * 100

                                        tmpTabla.Rows.Add(row)
                                    End If
                                    Dim view_o As DataView = New DataView(tmpTabla)
                                    If config.Tables(0).Rows(0)!agrupar_posicion = "O" Then
                                        view_o.Sort = IIf(config.Tables(0).Rows(0)!orden_grafica = "M", " total_tiempo DESC", IIf(config.Tables(0).Rows(0)!orden_grafica = "N", " total_tiempo", "estacion"))

                                    Else
                                        view_o.Sort = "orden ASC," & IIf(config.Tables(0).Rows(0)!orden_grafica = "M", " total_tiempo DESC", IIf(config.Tables(0).Rows(0)!orden_grafica = "N", " total_tiempo", "estacion"))
                                    End If
                                    tabla_grafico = view_o.ToTable()
                                Else
                                    tabla_grafico = datos
                                End If
                            Else
                                tabla_grafico = datos
                            End If

                            Dim series1 As New Series(indicador01, ViewType.Bar)
                            Dim series2 As New Series(indicador02, ViewType.Bar)
                            Dim series3 As New Series(indicador03, ViewType.Bar)

                            Dim series11 As New Series(indicador01, ViewType.StackedBar)
                            Dim series12 As New Series(indicador02, ViewType.StackedBar)
                            Dim series13 As New Series(indicador03, ViewType.StackedBar)


                            Dim series21 As New Series(indicador01, ViewType.FullStackedBar)
                            Dim series22 As New Series(indicador02, ViewType.FullStackedBar)
                                Dim series23 As New Series(indicador03, ViewType.FullStackedBar)

                            If ValNull(config.Tables(0).Rows(0)!tipo_principal, "A") = "S" Then
                                series1 = series11
                                series2 = series12
                                series3 = series13

                            ElseIf ValNull(config.Tables(0).Rows(0)!tipo_principal, "A") = "F" Then
                                series1 = series21
                                series2 = series22
                                series3 = series23
                            End If

                            ChartControl1.Series.Add(series1)
                            series1.DataSource = tabla_grafico
                            series1.LabelsVisibility = DevExpress.Utils.DefaultBoolean.True
                            Dim misColores As String = ValNull(config.Tables(0).Rows(0)!oee_colores, "A")
                            Dim misColores2 As String()
                            series1.View.Color = Color.SkyBlue
                            If misColores.Length = 0 Then
                                misColores = ";;"
                            End If
                            misColores2 = misColores.Split(New Char() {";"c})
                            If misColores2(0).Length > 0 Then
                                series1.View.Color = System.Drawing.ColorTranslator.FromHtml("#" & misColores2(0))
                            End If
                            series1.ArgumentScaleType = ScaleType.Qualitative
                            series1.ArgumentDataMember = "estacion"
                            series1.ValueScaleType = ScaleType.Numerical
                            series1.ValueDataMembers.AddRange(New String() {"tiempo"})
                            series1.Label.BackColor = Color.DarkBlue
                            series1.Label.TextColor = Color.White
                            series1.Label.Font = miFuente
                            series1.Label.TextPattern = "{V:F1}"

                            ChartControl1.Series.Add(series2)
                            series2.DataSource = tabla_grafico
                            series2.LabelsVisibility = DevExpress.Utils.DefaultBoolean.True
                            series2.View.Color = Color.BlueViolet
                            If misColores2(1).Length > 0 Then
                                series2.View.Color = System.Drawing.ColorTranslator.FromHtml("#" & misColores2(2))
                            End If
                            series2.ArgumentScaleType = ScaleType.Qualitative
                            series2.ArgumentDataMember = "estacion"
                            series2.ValueScaleType = ScaleType.Numerical
                            series2.ValueDataMembers.AddRange(New String() {"des_tiempo"})
                            series2.Label.BackColor = Color.DarkBlue
                            series2.Label.TextColor = Color.White
                            series2.Label.Font = miFuente
                            series2.Label.TextPattern = "{V:F1}"

                            ChartControl1.Series.Add(series3)
                            series3.DataSource = tabla_grafico
                            series3.LabelsVisibility = DevExpress.Utils.DefaultBoolean.True
                            series3.View.Color = Color.Red
                            If misColores2(2).Length > 0 Then
                                series3.View.Color = System.Drawing.ColorTranslator.FromHtml("#" & misColores2(1))
                            End If
                            series3.ArgumentScaleType = ScaleType.Qualitative
                            series3.ArgumentDataMember = "estacion"
                            series3.ValueScaleType = ScaleType.Numerical
                            series3.ValueDataMembers.AddRange(New String() {"espera"})
                            series3.Label.BackColor = Color.DarkBlue
                            series3.Label.TextColor = Color.White
                            series3.Label.Font = miFuente
                            series3.Label.TextPattern = "{V:F1}"

                            Dim series4 As New Series(indicador04, ViewType.Spline)

                            ChartControl1.Series.Add(series4)
                            series4.DataSource = tabla_grafico
                            series4.LabelsVisibility = DevExpress.Utils.DefaultBoolean.True
                            series4.View.Color = Color.Green
                            If ValNull(config.Tables(0).Rows(0)!color_spiline, "A").Length > 0 Then
                                series4.View.Color = System.Drawing.ColorTranslator.FromHtml("#" & config.Tables(0).Rows(0)!color_spiline)
                            End If
                            series4.ArgumentScaleType = ScaleType.Qualitative
                            series4.ArgumentDataMember = "estacion"
                            series4.ValueScaleType = ScaleType.Numerical
                            series4.ValueDataMembers.AddRange(New String() {"eficiencia"})
                            series4.Label.BackColor = Color.DarkBlue
                            series4.Label.TextColor = Color.White
                            series4.Label.Font = miFuente
                            series4.Label.TextPattern = "{V:F1}"


                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Visibility = DevExpress.Utils.DefaultBoolean.True
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Label.Font = miFuenteEjes
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.GridSpacingAuto = False
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.GridSpacing = 1
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Title.Text = config.Tables(0).Rows(0)!texto_y
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Title.Font = miFuenteAlto
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Title.Visibility = DevExpress.Utils.DefaultBoolean.True
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.GridLines.Visible = False
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Tickmarks.Visible = False
                            CType(ChartControl1.Diagram, XYDiagram).AxisY.Tickmarks.MinorVisible = False

                            Dim myAxisY As New SecondaryAxisY("EFI")
                            CType(ChartControl1.Diagram, XYDiagram).SecondaryAxesY.Clear()
                            CType(ChartControl1.Diagram, XYDiagram).SecondaryAxesY.Add(myAxisY)
                            CType(series4.View, LineSeriesView).AxisY = myAxisY
                            myAxisY.Title.Text = config.Tables(0).Rows(0)!texto_z
                            myAxisY.Title.Visible = True
                            myAxisY.Label.Font = miFuenteEjes
                            myAxisY.Title.Font = miFuenteAlto
                            myAxisY.GridLines.Visible = False
                            myAxisY.Tickmarks.Visible = False
                            myAxisY.Tickmarks.MinorVisible = False
                            myAxisY.Title.TextColor = Color.Green
                            myAxisY.Label.TextColor = Color.Green
                            myAxisY.Color = Color.Green

                            CType(ChartControl1.Diagram, XYDiagram).AxisX.GridLines.Visible = False
                            CType(ChartControl1.Diagram, XYDiagram).AxisX.Label.Font = miFuenteEjes
                            CType(ChartControl1.Diagram, XYDiagram).AxisX.Title.Text = Strings.Space(5) & config.Tables(0).Rows(0)!texto_x & Strings.Space(10)
                            CType(ChartControl1.Diagram, XYDiagram).AxisX.GridLines.Visible = False
                            CType(ChartControl1.Diagram, XYDiagram).AxisX.Title.Font = miFuenteAlto
                            CType(ChartControl1.Diagram, XYDiagram).AxisX.Title.Visibility = DevExpress.Utils.DefaultBoolean.True
                            If config.Tables(0).Rows(0)!overlap = "R" Then
                                CType(ChartControl1.Diagram, XYDiagram).AxisX.Label.ResolveOverlappingOptions.AllowStagger = False
                                CType(ChartControl1.Diagram, XYDiagram).AxisX.Label.ResolveOverlappingOptions.AllowRotate = True
                            Else
                                CType(ChartControl1.Diagram, XYDiagram).AxisX.Label.ResolveOverlappingOptions.AllowStagger = True
                                CType(ChartControl1.Diagram, XYDiagram).AxisX.Label.ResolveOverlappingOptions.AllowRotate = False
                            End If



                            ChartControl1.Series.AddRange(New Series() {series1, series2, series3})

                            ChartControl1.Titles.Add(Titulo)
                            Dim Titulo2 As New ChartTitle()

                            Titulo2.Font = miFuente
                            Titulo2.Text = "Extraccion de datos: " & cadPeriodo
                            ChartControl1.Titles.Add(Titulo2)
                            Dim Titulo3 As New ChartTitle()
                            Titulo3.Font = miFuente
                            Titulo3.Text = "Generado el: " & Format(Now, "ddd dd-MM-yyyy HH:mm:ss")
                            ChartControl1.Titles.Add(Titulo3)
                            Dim Titulo4 As New ChartTitle()
                            Titulo4.Font = miFuente
                            Titulo4.Text = "Extrayendo los datos desde: " & Format(eDesde, "dd-MM-yyyy HH:mm:ss") & " hasta: " &
                                            Format(eHasta, "dd-MM-yyyy HH:mm:ss")
                            ChartControl1.Titles.Add(Titulo4)
                            ChartControl1.Width = 1000
                            ChartControl1.Height = 700

                            If config.Tables(0).Rows(0)!ver_leyenda = "S" Then
                                ChartControl1.Legend.Visibility = DevExpress.Utils.DefaultBoolean.True
                            Else
                                ChartControl1.Legend.Visibility = DevExpress.Utils.DefaultBoolean.False
                            End If
                            Try
                                Dim rutaImagen = Microsoft.VisualBasic.Strings.Replace(archivoImagen, "\", "\\")
                                SaveChartImageToFile(ChartControl1, ImageFormat.Png, rutaImagen)
                                Dim image As Image = GetChartImage(ChartControl1, ImageFormat.Png)
                                image.Save(rutaImagen)

                            Catch ex As Exception
                                agregarLOG("Ocurrió un error al intentar construir un archivo de adjunto de reporte (gráfico). Error: " & ex.Message, 7, 0)
                            End Try
                        End If

                    End If
                End If

            End If

        End If


    End Function



    Function calcularPromedio(tiempo As Integer) As String
        calcularPromedio = ""
        tiempo = Math.Round(tiempo, 0)
        Dim horas = tiempo / 3600
        Dim minutos = (tiempo Mod 3600) / 60
        Dim segundos = tiempo Mod 60
        If segundos > 30 Then
            minutos = minutos + 1
        End If
        If minutos = 0 And horas = 0 Then
            minutos = 1
        End If
        calcularPromedio = Format(Math.Floor(horas), "00") & ":" & Format(Math.Floor(minutos), "00")
    End Function

    Private Sub agregarLOG(cadena As String, Optional reporte As Integer = 0, Optional tipo As Integer = 0, Optional aplicacion As Integer = 80)
        If Not be_log_activar Then Exit Sub
        'tipo 0: Info
        'tipo 2: Advertencia
        'tipo 9: Error
        Dim regsAfectados = consultaACT("INSERT INTO " & rutaBD & ".log (aplicacion, tipo, proceso, texto) VALUES (" & aplicacion & ", " & tipo & ", " & reporte & ", '" & Microsoft.VisualBasic.Strings.Left(cadena, 250) & "')")
    End Sub

    Private Function GetChartImage(ByVal chart As ChartControl, ByVal format As ImageFormat) As Image
        ' Create an image.  
        Dim image As Image = Nothing

        ' Create an image of the chart.  
        Using s As New MemoryStream()
            chart.ExportToImage(s, format)
            image = System.Drawing.Image.FromStream(s)
        End Using

        ' Return the image.  
        Return image
    End Function

    Private Function GetGaugeImage(ByVal chart As GaugeControl, ByVal format As ImageFormat) As Image
        ' Create an image.  
        Dim image As Image = Nothing

        ' Create an image of the chart.  
        Using s As New MemoryStream()
            chart.ExportToImage(s, format)
            image = System.Drawing.Image.FromStream(s)
        End Using

        ' Return the image.  
        Return image
    End Function

    Private Sub SaveChartImageToFile(ByVal chart As ChartControl, ByVal format As ImageFormat, ByVal fileName As String)
        ' Create an image in the specified format from the chart  
        ' and save it to the specified path.  
        chart.ExportToImage(fileName, format)
    End Sub

    Private Sub SaveGaugeImageToFile(ByVal chart As GaugeControl, ByVal format As ImageFormat, ByVal fileName As String)
        ' Create an image in the specified format from the chart  
        ' and save it to the specified path.  
        chart.ExportToImage(fileName, format)
    End Sub

    Private Sub Label1_Click(sender As Object, e As EventArgs) Handles Label1.Click

    End Sub
End Class


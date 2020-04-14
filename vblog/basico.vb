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
    Public rutaBD As String = "logisticar"

    Sub Main(argumentos As String())
        If Process.GetProcessesByName _
          (Process.GetCurrentProcess.ProcessName).Length > 1 Then

        ElseIf argumentos.Length = 0 Then
            MsgBox("No se puede iniciar la escritura en el LOG: Se requiere la cadena de conexión", MsgBoxStyle.Critical, "SIGMA Monitor")
        Else
            cadenaConexion = argumentos(0)
            'cadenaConexion = "server=localhost;user id=root;password=usbw;port=3307;Convert Zero Datetime=True"
            Dim idProceso = Process.GetCurrentProcess.Id
            Dim mensajesDS As DataSet
            Dim eMensaje = ""
            'Escalada 4
            Dim miError As String = ""
            Dim regsAfectados = 0

            Dim maximo_largo_mmcall As Integer = 40

            Dim cadSQL As String = "SELECT be_log_activar FROM " & rutaBD & ".configuracion"
            Dim readerDS As DataSet = consultaSEL(cadSQL)
            If readerDS.Tables(0).Rows.Count > 0 Then
                Dim reader As DataRow = readerDS.Tables(0).Rows(0)
                be_log_activar = ValNull(reader!be_log_activar, "A") = "S"
            End If
            If be_log_activar Then
                regsAfectados = consultaACT("UPDATE " & rutaBD & ".mensajes SET estatus = '" & idProceso & "' WHERE canal = 4 AND estatus = 'E'")
                Dim agrupado As Boolean = True
                cadSQL = "SELECT a.id, z.texto, a.proceso FROM " & rutaBD & ".mensajes a INNER JOIN " & rutaBD & ".mensajes_procesados z ON a.id = z.mensaje WHERE a.estatus = '" & idProceso & "' ORDER BY a.prioridad DESC, a.id"
                'Se preselecciona la voz
                mensajesDS = consultaSEL(cadSQL)
                If mensajesDS.Tables(0).Rows.Count > 0 Then
                    For Each elmensaje In mensajesDS.Tables(0).Rows
                        eMensaje = ValNull(elmensaje!texto, "A")
                        agregarLOG(eMensaje, elmensaje!proceso, 5, 30)
                    Next

                End If
                regsAfectados = consultaACT("UPDATE " & rutaBD & ".mensajes SET estatus = 'Z', enviada = NOW() WHERE canal = 4 AND estatus = '" & idProceso & "'")
                If regsAfectados > 0 Then
                    agregarLOG("Se generaron " & regsAfectados & " registro(s) de log")
                End If
            End If
        End If
        Application.Exit()
    End Sub
    Private Sub agregarLOG(cadena As String, Optional reporte As Integer = 0, Optional tipo As Integer = 0, Optional aplicacion As Integer = 60)
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

    Function validarURI(ByVal cadena As String) As Boolean
        Dim validatedUri As System.Uri
        Return Uri.TryCreate(cadena, UriKind.RelativeOrAbsolute, validatedUri)
    End Function

End Module


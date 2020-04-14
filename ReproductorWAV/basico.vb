Imports System.Resources
Imports System.Threading
Imports System.Globalization
Imports System.Data.Odbc
Imports DevExpress.XtraEditors
Imports DevExpress.LookAndFeel
Imports System.Windows.Forms


Module basico
    Public MiConexion As OdbcConnection
    Public MiFuente As Font = New System.Drawing.Font("Lucida Sans", 9, FontStyle.Regular)
    Public Memo1 As String, Memo2 As String, Memo3 As String
    Public rutaBD As String = "logisticar"

    Function Conexion() As String
        Conexion = ""
        MiConexion = New OdbcConnection("dsn=mmcall;MultipleActiveResultSets=true")
        Try
            MiConexion.Open()
        Catch ex As Exception
            Conexion = ex.Message

        End Try

        'Me.Text = Traducir("frmMenuTitulo")

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


    Sub Sombrear_Texto(ByVal sender As Object, ByVal e As EventArgs)
        TryCast(sender, TextEdit).SelectAll()
    End Sub





End Module

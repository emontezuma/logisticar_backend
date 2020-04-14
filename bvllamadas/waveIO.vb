Imports System
Imports System.Collections.Generic
Imports System.Text
Imports System.IO
Imports System.Runtime.InteropServices

Class WaveIO

    Public length As Integer

    Public channels As Short

    Public samplerate As Integer

    Public DataLength As Integer

    Public BitsPerSample As Short

    Private Sub WaveHeaderIN(ByVal spath As String)
        Dim fs As FileStream = New FileStream(spath, FileMode.Open, FileAccess.Read)
        Dim br As BinaryReader = New BinaryReader(fs)
        Me.length = (CType(fs.Length, Integer) - 8)
        fs.Position = 22
        Me.channels = br.ReadInt16
        fs.Position = 24
        Me.samplerate = br.ReadInt32
        fs.Position = 34
        Me.BitsPerSample = br.ReadInt16
        Me.DataLength = (CType(fs.Length, Integer) - 44)
        br.Close()
        fs.Close()
    End Sub

    Private Sub WaveHeaderOUT(ByVal sPath As String)
        Dim fs As FileStream = New FileStream(sPath, FileMode.Create, FileAccess.Write)
        Dim bw As BinaryWriter = New BinaryWriter(fs)
        fs.Position = 0
        bw.Write(New Char() {Microsoft.VisualBasic.ChrW(82), Microsoft.VisualBasic.ChrW(73), Microsoft.VisualBasic.ChrW(70), Microsoft.VisualBasic.ChrW(70)})
        bw.Write(Me.length)
        bw.Write(New Char() {Microsoft.VisualBasic.ChrW(87), Microsoft.VisualBasic.ChrW(65), Microsoft.VisualBasic.ChrW(86), Microsoft.VisualBasic.ChrW(69), Microsoft.VisualBasic.ChrW(102), Microsoft.VisualBasic.ChrW(109), Microsoft.VisualBasic.ChrW(116), Microsoft.VisualBasic.ChrW(32)})
        bw.Write(CType(16, Integer))
        bw.Write(CType(1, Short))
        bw.Write(Me.channels)
        bw.Write(Me.samplerate)
        bw.Write(CType((Me.samplerate _
                        * ((Me.BitsPerSample * Me.channels) _
                        / 8)), Integer))
        bw.Write(CType(((Me.BitsPerSample * Me.channels) _
                        / 8), Short))
        bw.Write(Me.BitsPerSample)
        bw.Write(New Char() {Microsoft.VisualBasic.ChrW(100), Microsoft.VisualBasic.ChrW(97), Microsoft.VisualBasic.ChrW(116), Microsoft.VisualBasic.ChrW(97)})
        bw.Write(Me.DataLength)
        bw.Close()
        fs.Close()
    End Sub

    Public Sub Merge(ByVal files() As String, ByVal outfile As String)
        Dim wa_IN As WaveIO = New WaveIO
        Dim wa_out As WaveIO = New WaveIO
        wa_out.DataLength = 0
        wa_out.length = 0
        'Gather header data
        For Each path As String In files
            wa_IN.WaveHeaderIN(path)
            wa_out.DataLength = (wa_out.DataLength + wa_IN.DataLength)
            wa_out.length = (wa_out.length + wa_IN.length)
        Next
        'Recontruct new header
        wa_out.BitsPerSample = wa_IN.BitsPerSample
        wa_out.channels = wa_IN.channels
        wa_out.samplerate = wa_IN.samplerate
        wa_out.WaveHeaderOUT(outfile)
        For Each path As String In files
            Dim fs As FileStream = New FileStream(path, FileMode.Open, FileAccess.Read)
            Dim arrfile() As Byte = New Byte(((fs.Length - 44)) - 1) {}
            fs.Position = 44
            fs.Read(arrfile, 0, arrfile.Length)
            fs.Close()
            Dim fo As FileStream = New FileStream(outfile, FileMode.Append, FileAccess.Write)
            Dim bw As BinaryWriter = New BinaryWriter(fo)
            bw.Write(arrfile)
            bw.Close()
            fo.Close()
        Next
    End Sub
End Class

<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Form1
    Inherits System.Windows.Forms.Form

    'Form reemplaza a Dispose para limpiar la lista de componentes.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Requerido por el Diseñador de Windows Forms
    Private components As System.ComponentModel.IContainer

    'NOTA: el Diseñador de Windows Forms necesita el siguiente procedimiento
    'Se puede modificar usando el Diseñador de Windows Forms.  
    'No lo modifique con el editor de código.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form1))
        Me.GroupControl5 = New DevExpress.XtraEditors.GroupControl()
        Me.LabelControl3 = New DevExpress.XtraEditors.LabelControl()
        Me.TextEdit3 = New DevExpress.XtraEditors.TextEdit()
        Me.SimpleButton8 = New DevExpress.XtraEditors.SimpleButton()
        Me.TextEdit16 = New DevExpress.XtraEditors.TextEdit()
        Me.LabelControl27 = New DevExpress.XtraEditors.LabelControl()
        Me.NotifyIcon1 = New System.Windows.Forms.NotifyIcon(Me.components)
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.ToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripMenuItem3 = New System.Windows.Forms.ToolStripMenuItem()
        Me.Timer1 = New System.Windows.Forms.Timer(Me.components)
        Me.XtraFolderBrowserDialog1 = New DevExpress.XtraEditors.XtraFolderBrowserDialog(Me.components)
        Me.SimpleButton2 = New DevExpress.XtraEditors.SimpleButton()
        Me.SimpleButton1 = New DevExpress.XtraEditors.SimpleButton()
        Me.LinkLabel1 = New System.Windows.Forms.LinkLabel()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        CType(Me.GroupControl5, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupControl5.SuspendLayout()
        CType(Me.TextEdit3.Properties, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.TextEdit16.Properties, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ContextMenuStrip1.SuspendLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'GroupControl5
        '
        Me.GroupControl5.AppearanceCaption.Font = New System.Drawing.Font("Lucida Sans", 9.0!, System.Drawing.FontStyle.Bold)
        Me.GroupControl5.AppearanceCaption.Options.UseFont = True
        Me.GroupControl5.CaptionImageOptions.Padding = New System.Windows.Forms.Padding(1)
        Me.GroupControl5.Controls.Add(Me.LabelControl3)
        Me.GroupControl5.Controls.Add(Me.TextEdit3)
        Me.GroupControl5.Controls.Add(Me.SimpleButton8)
        Me.GroupControl5.Controls.Add(Me.TextEdit16)
        Me.GroupControl5.Controls.Add(Me.LabelControl27)
        Me.GroupControl5.Location = New System.Drawing.Point(12, 131)
        Me.GroupControl5.Name = "GroupControl5"
        Me.GroupControl5.Size = New System.Drawing.Size(496, 116)
        Me.GroupControl5.TabIndex = 0
        Me.GroupControl5.Text = "Configuración para la reproducción de audio"
        '
        'LabelControl3
        '
        Me.LabelControl3.Appearance.Font = New System.Drawing.Font("Lucida Sans", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.LabelControl3.Appearance.Options.UseFont = True
        Me.LabelControl3.Location = New System.Drawing.Point(17, 81)
        Me.LabelControl3.Name = "LabelControl3"
        Me.LabelControl3.Size = New System.Drawing.Size(93, 17)
        Me.LabelControl3.TabIndex = 3
        Me.LabelControl3.Text = "Revisar (seg)"
        '
        'TextEdit3
        '
        Me.TextEdit3.EditValue = "10"
        Me.TextEdit3.Location = New System.Drawing.Point(118, 77)
        Me.TextEdit3.Name = "TextEdit3"
        Me.TextEdit3.Properties.Appearance.Font = New System.Drawing.Font("Lucida Sans", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.TextEdit3.Properties.Appearance.Options.UseFont = True
        Me.TextEdit3.Properties.Appearance.Options.UseTextOptions = True
        Me.TextEdit3.Properties.Appearance.TextOptions.HAlignment = DevExpress.Utils.HorzAlignment.Far
        Me.TextEdit3.Properties.BorderStyle = DevExpress.XtraEditors.Controls.BorderStyles.HotFlat
        Me.TextEdit3.Size = New System.Drawing.Size(59, 26)
        Me.TextEdit3.TabIndex = 4
        '
        'SimpleButton8
        '
        Me.SimpleButton8.Appearance.Font = New System.Drawing.Font("Lucida Sans", 9.0!, System.Drawing.FontStyle.Bold)
        Me.SimpleButton8.Appearance.Image = CType(resources.GetObject("SimpleButton8.Appearance.Image"), System.Drawing.Image)
        Me.SimpleButton8.Appearance.Options.UseFont = True
        Me.SimpleButton8.Appearance.Options.UseImage = True
        Me.SimpleButton8.Appearance.Options.UseTextOptions = True
        Me.SimpleButton8.Appearance.TextOptions.HAlignment = DevExpress.Utils.HorzAlignment.Center
        Me.SimpleButton8.Appearance.TextOptions.VAlignment = DevExpress.Utils.VertAlignment.Center
        Me.SimpleButton8.ButtonStyle = DevExpress.XtraEditors.Controls.BorderStyles.HotFlat
        Me.SimpleButton8.Location = New System.Drawing.Point(449, 46)
        Me.SimpleButton8.Name = "SimpleButton8"
        Me.SimpleButton8.Size = New System.Drawing.Size(28, 25)
        Me.SimpleButton8.TabIndex = 2
        Me.SimpleButton8.Text = "..."
        '
        'TextEdit16
        '
        Me.TextEdit16.EditValue = ""
        Me.TextEdit16.Location = New System.Drawing.Point(118, 45)
        Me.TextEdit16.Name = "TextEdit16"
        Me.TextEdit16.Properties.Appearance.Font = New System.Drawing.Font("Lucida Sans", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.TextEdit16.Properties.Appearance.Options.UseFont = True
        Me.TextEdit16.Properties.AppearanceFocused.BackColor = System.Drawing.Color.AliceBlue
        Me.TextEdit16.Properties.AppearanceFocused.BorderColor = System.Drawing.Color.LightBlue
        Me.TextEdit16.Properties.AppearanceFocused.Options.UseBackColor = True
        Me.TextEdit16.Properties.AppearanceFocused.Options.UseBorderColor = True
        Me.TextEdit16.Properties.BorderStyle = DevExpress.XtraEditors.Controls.BorderStyles.HotFlat
        Me.TextEdit16.Properties.MaxLength = 500
        Me.TextEdit16.Properties.NullText = "Usuario del servicio de SMS"
        Me.TextEdit16.Size = New System.Drawing.Size(325, 26)
        Me.TextEdit16.TabIndex = 1
        '
        'LabelControl27
        '
        Me.LabelControl27.Appearance.Font = New System.Drawing.Font("Lucida Sans", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.LabelControl27.Appearance.Options.UseFont = True
        Me.LabelControl27.Location = New System.Drawing.Point(17, 49)
        Me.LabelControl27.Name = "LabelControl27"
        Me.LabelControl27.Size = New System.Drawing.Size(33, 17)
        Me.LabelControl27.TabIndex = 0
        Me.LabelControl27.Text = "&Ruta"
        '
        'NotifyIcon1
        '
        Me.NotifyIcon1.ContextMenuStrip = Me.ContextMenuStrip1
        Me.NotifyIcon1.Icon = CType(resources.GetObject("NotifyIcon1.Icon"), System.Drawing.Icon)
        Me.NotifyIcon1.Text = "Cronos::. Reproductor de audios"
        Me.NotifyIcon1.Visible = True
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.Font = New System.Drawing.Font("Lucida Sans", 9.0!)
        Me.ContextMenuStrip1.ImageScalingSize = New System.Drawing.Size(20, 20)
        Me.ContextMenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItem1, Me.ToolStripSeparator1, Me.ToolStripMenuItem3})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        Me.ContextMenuStrip1.Size = New System.Drawing.Size(179, 54)
        Me.ContextMenuStrip1.Text = "Configuración"
        '
        'ToolStripMenuItem1
        '
        Me.ToolStripMenuItem1.Font = New System.Drawing.Font("Lucida Sans", 9.0!)
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        Me.ToolStripMenuItem1.Size = New System.Drawing.Size(178, 22)
        Me.ToolStripMenuItem1.Text = "Configuración"
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        Me.ToolStripSeparator1.Size = New System.Drawing.Size(175, 6)
        '
        'ToolStripMenuItem3
        '
        Me.ToolStripMenuItem3.Name = "ToolStripMenuItem3"
        Me.ToolStripMenuItem3.Size = New System.Drawing.Size(178, 22)
        Me.ToolStripMenuItem3.Text = "Cerrar"
        '
        'Timer1
        '
        '
        'XtraFolderBrowserDialog1
        '
        Me.XtraFolderBrowserDialog1.SelectedPath = "XtraFolderBrowserDialog1"
        '
        'SimpleButton2
        '
        Me.SimpleButton2.Appearance.Font = New System.Drawing.Font("Lucida Sans", 9.0!, System.Drawing.FontStyle.Bold)
        Me.SimpleButton2.Appearance.Image = CType(resources.GetObject("SimpleButton2.Appearance.Image"), System.Drawing.Image)
        Me.SimpleButton2.Appearance.Options.UseFont = True
        Me.SimpleButton2.Appearance.Options.UseImage = True
        Me.SimpleButton2.Appearance.Options.UseTextOptions = True
        Me.SimpleButton2.Appearance.TextOptions.HAlignment = DevExpress.Utils.HorzAlignment.Near
        Me.SimpleButton2.Appearance.TextOptions.VAlignment = DevExpress.Utils.VertAlignment.Center
        Me.SimpleButton2.ButtonStyle = DevExpress.XtraEditors.Controls.BorderStyles.HotFlat
        Me.SimpleButton2.ImageOptions.Image = CType(resources.GetObject("SimpleButton2.ImageOptions.Image"), System.Drawing.Image)
        Me.SimpleButton2.Location = New System.Drawing.Point(386, 260)
        Me.SimpleButton2.Name = "SimpleButton2"
        Me.SimpleButton2.Size = New System.Drawing.Size(122, 38)
        Me.SimpleButton2.TabIndex = 2
        Me.SimpleButton2.Text = "&Cerrar"
        '
        'SimpleButton1
        '
        Me.SimpleButton1.Appearance.Font = New System.Drawing.Font("Lucida Sans", 9.0!, System.Drawing.FontStyle.Bold)
        Me.SimpleButton1.Appearance.Image = CType(resources.GetObject("SimpleButton1.Appearance.Image"), System.Drawing.Image)
        Me.SimpleButton1.Appearance.Options.UseFont = True
        Me.SimpleButton1.Appearance.Options.UseImage = True
        Me.SimpleButton1.Appearance.Options.UseTextOptions = True
        Me.SimpleButton1.Appearance.TextOptions.HAlignment = DevExpress.Utils.HorzAlignment.Near
        Me.SimpleButton1.Appearance.TextOptions.VAlignment = DevExpress.Utils.VertAlignment.Center
        Me.SimpleButton1.ButtonStyle = DevExpress.XtraEditors.Controls.BorderStyles.HotFlat
        Me.SimpleButton1.ImageOptions.Image = CType(resources.GetObject("SimpleButton1.ImageOptions.Image"), System.Drawing.Image)
        Me.SimpleButton1.Location = New System.Drawing.Point(258, 260)
        Me.SimpleButton1.Name = "SimpleButton1"
        Me.SimpleButton1.Size = New System.Drawing.Size(122, 38)
        Me.SimpleButton1.TabIndex = 1
        Me.SimpleButton1.Text = "&Guardar"
        '
        'LinkLabel1
        '
        Me.LinkLabel1.AutoSize = True
        Me.LinkLabel1.Font = New System.Drawing.Font("Lucida Sans", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.LinkLabel1.Location = New System.Drawing.Point(139, 108)
        Me.LinkLabel1.Name = "LinkLabel1"
        Me.LinkLabel1.Size = New System.Drawing.Size(177, 17)
        Me.LinkLabel1.TabIndex = 15
        Me.LinkLabel1.TabStop = True
        Me.LinkLabel1.Text = "www.mmcallmexico.mx"
        '
        'PictureBox1
        '
        Me.PictureBox1.BackColor = System.Drawing.Color.FromArgb(CType(CType(250, Byte), Integer), CType(CType(183, Byte), Integer), CType(CType(2, Byte), Integer))
        Me.PictureBox1.Image = CType(resources.GetObject("PictureBox1.Image"), System.Drawing.Image)
        Me.PictureBox1.Location = New System.Drawing.Point(5, 8)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(503, 102)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage
        Me.PictureBox1.TabIndex = 14
        Me.PictureBox1.TabStop = False
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(8.0!, 16.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.ClientSize = New System.Drawing.Size(520, 308)
        Me.Controls.Add(Me.LinkLabel1)
        Me.Controls.Add(Me.PictureBox1)
        Me.Controls.Add(Me.SimpleButton2)
        Me.Controls.Add(Me.SimpleButton1)
        Me.Controls.Add(Me.GroupControl5)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Reproductor de audios(WAV)"
        CType(Me.GroupControl5, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupControl5.ResumeLayout(False)
        Me.GroupControl5.PerformLayout()
        CType(Me.TextEdit3.Properties, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.TextEdit16.Properties, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ContextMenuStrip1.ResumeLayout(False)
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents GroupControl5 As DevExpress.XtraEditors.GroupControl
    Friend WithEvents SimpleButton8 As DevExpress.XtraEditors.SimpleButton
    Friend WithEvents TextEdit16 As DevExpress.XtraEditors.TextEdit
    Friend WithEvents LabelControl27 As DevExpress.XtraEditors.LabelControl
    Friend WithEvents LabelControl3 As DevExpress.XtraEditors.LabelControl
    Friend WithEvents TextEdit3 As DevExpress.XtraEditors.TextEdit
    Friend WithEvents SimpleButton2 As DevExpress.XtraEditors.SimpleButton
    Friend WithEvents SimpleButton1 As DevExpress.XtraEditors.SimpleButton
    Friend WithEvents NotifyIcon1 As NotifyIcon
    Friend WithEvents ContextMenuStrip1 As ContextMenuStrip
    Friend WithEvents ToolStripMenuItem1 As ToolStripMenuItem
    Friend WithEvents ToolStripSeparator1 As ToolStripSeparator
    Friend WithEvents ToolStripMenuItem3 As ToolStripMenuItem
    Friend WithEvents Timer1 As Timer
    Friend WithEvents XtraFolderBrowserDialog1 As DevExpress.XtraEditors.XtraFolderBrowserDialog
    Friend WithEvents LinkLabel1 As LinkLabel
    Friend WithEvents PictureBox1 As PictureBox
End Class

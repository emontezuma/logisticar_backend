<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class XtraForm1
    Inherits DevExpress.XtraEditors.XtraForm

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then
            components.Dispose()
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(XtraForm1))
        Me.PictureEdit1 = New DevExpress.XtraEditors.PictureEdit()
        Me.TileBar1 = New DevExpress.XtraBars.Navigation.TileBar()
        Me.LabelControl1 = New DevExpress.XtraEditors.LabelControl()
        Me.LabelControl2 = New DevExpress.XtraEditors.LabelControl()
        Me.HyperlinkLabelControl1 = New DevExpress.XtraEditors.HyperlinkLabelControl()
        Me.GroupControl1 = New DevExpress.XtraEditors.GroupControl()
        Me.ComboBoxEdit2 = New DevExpress.XtraEditors.ComboBoxEdit()
        Me.LabelControl4 = New DevExpress.XtraEditors.LabelControl()
        Me.SimpleButton1 = New DevExpress.XtraEditors.SimpleButton()
        Me.SimpleButton3 = New DevExpress.XtraEditors.SimpleButton()
        Me.SimpleButton2 = New DevExpress.XtraEditors.SimpleButton()
        Me.revisaFlag = New System.Windows.Forms.Timer(Me.components)
        Me.escalamiento = New System.Windows.Forms.Timer(Me.components)
        Me.SerialPort1 = New System.IO.Ports.SerialPort(Me.components)
        Me.NotifyIcon1 = New System.Windows.Forms.NotifyIcon(Me.components)
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.VerElLogToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DetenerElMonitorToolStripMenuItem = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ReanudarElMonitorToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ListBoxControl1 = New DevExpress.XtraEditors.ListBoxControl()
        Me.revisarLog = New System.Windows.Forms.Timer(Me.components)
        Me.sinEventos = New System.Windows.Forms.Timer(Me.components)
        Me.BarStaticItem4 = New DevExpress.XtraBars.BarStaticItem()
        Me.BarDockControl1 = New DevExpress.XtraBars.BarDockControl()
        Me.BarManager1 = New DevExpress.XtraBars.BarManager(Me.components)
        Me.Bar3 = New DevExpress.XtraBars.Bar()
        Me.SkinBarSubItem1 = New DevExpress.XtraBars.SkinBarSubItem()
        Me.BarStaticItem3 = New DevExpress.XtraBars.BarStaticItem()
        Me.BarStaticItem1 = New DevExpress.XtraBars.BarStaticItem()
        Me.barDockControlTop = New DevExpress.XtraBars.BarDockControl()
        Me.barDockControlBottom = New DevExpress.XtraBars.BarDockControl()
        Me.barDockControlLeft = New DevExpress.XtraBars.BarDockControl()
        Me.barDockControlRight = New DevExpress.XtraBars.BarDockControl()
        Me.BarStaticItem2 = New DevExpress.XtraBars.BarStaticItem()
        Me.reportes = New System.Windows.Forms.Timer(Me.components)
        Me.arduino = New System.Windows.Forms.Timer(Me.components)
        Me.reenviarMMCALL = New System.Windows.Forms.Timer(Me.components)
        Me.borrarArchivos = New System.Windows.Forms.Timer(Me.components)
        Me.turno = New System.Windows.Forms.Timer(Me.components)
        Me.Timer1 = New System.Windows.Forms.Timer(Me.components)
        CType(Me.PictureEdit1.Properties, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.GroupControl1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupControl1.SuspendLayout()
        CType(Me.ComboBoxEdit2.Properties, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ContextMenuStrip1.SuspendLayout()
        CType(Me.ListBoxControl1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.BarManager1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'PictureEdit1
        '
        Me.PictureEdit1.EditValue = CType(resources.GetObject("PictureEdit1.EditValue"), Object)
        Me.PictureEdit1.Location = New System.Drawing.Point(-1, 30)
        Me.PictureEdit1.Margin = New System.Windows.Forms.Padding(2)
        Me.PictureEdit1.Name = "PictureEdit1"
        Me.PictureEdit1.Properties.Appearance.BackColor = System.Drawing.Color.FromArgb(CType(CType(250, Byte), Integer), CType(CType(183, Byte), Integer), CType(CType(2, Byte), Integer))
        Me.PictureEdit1.Properties.Appearance.Options.UseBackColor = True
        Me.PictureEdit1.Properties.BorderStyle = DevExpress.XtraEditors.Controls.BorderStyles.NoBorder
        Me.PictureEdit1.Properties.ShowCameraMenuItem = DevExpress.XtraEditors.Controls.CameraMenuItemVisibility.[Auto]
        Me.PictureEdit1.Properties.SizeMode = DevExpress.XtraEditors.Controls.PictureSizeMode.Zoom
        Me.PictureEdit1.Size = New System.Drawing.Size(29, 35)
        Me.PictureEdit1.TabIndex = 3
        '
        'TileBar1
        '
        Me.TileBar1.BackColor = System.Drawing.Color.FromArgb(CType(CType(250, Byte), Integer), CType(CType(183, Byte), Integer), CType(CType(2, Byte), Integer))
        Me.TileBar1.Dock = System.Windows.Forms.DockStyle.Top
        Me.TileBar1.DropDownOptions.BeakColor = System.Drawing.Color.Empty
        Me.TileBar1.Location = New System.Drawing.Point(0, 0)
        Me.TileBar1.Margin = New System.Windows.Forms.Padding(2)
        Me.TileBar1.Name = "TileBar1"
        Me.TileBar1.Padding = New System.Windows.Forms.Padding(17, 6, 17, 6)
        Me.TileBar1.ScrollMode = DevExpress.XtraEditors.TileControlScrollMode.ScrollButtons
        Me.TileBar1.Size = New System.Drawing.Size(997, 65)
        Me.TileBar1.TabIndex = 2
        Me.TileBar1.Text = "TileBar1"
        '
        'LabelControl1
        '
        Me.LabelControl1.Appearance.BackColor = System.Drawing.Color.FromArgb(CType(CType(250, Byte), Integer), CType(CType(183, Byte), Integer), CType(CType(2, Byte), Integer))
        Me.LabelControl1.Appearance.Font = New System.Drawing.Font("Lucida Sans", 12.0!, System.Drawing.FontStyle.Bold)
        Me.LabelControl1.Appearance.ForeColor = System.Drawing.Color.Black
        Me.LabelControl1.Appearance.Options.UseBackColor = True
        Me.LabelControl1.Appearance.Options.UseFont = True
        Me.LabelControl1.Appearance.Options.UseForeColor = True
        Me.LabelControl1.Location = New System.Drawing.Point(7, 5)
        Me.LabelControl1.Margin = New System.Windows.Forms.Padding(2)
        Me.LabelControl1.Name = "LabelControl1"
        Me.LabelControl1.Size = New System.Drawing.Size(168, 18)
        Me.LabelControl1.TabIndex = 8
        Me.LabelControl1.Text = "logistiCAR (backend)"
        '
        'LabelControl2
        '
        Me.LabelControl2.Appearance.BackColor = System.Drawing.Color.FromArgb(CType(CType(250, Byte), Integer), CType(CType(183, Byte), Integer), CType(CType(2, Byte), Integer))
        Me.LabelControl2.Appearance.ForeColor = System.Drawing.Color.Black
        Me.LabelControl2.Appearance.Options.UseBackColor = True
        Me.LabelControl2.Appearance.Options.UseForeColor = True
        Me.LabelControl2.Location = New System.Drawing.Point(6, 21)
        Me.LabelControl2.Margin = New System.Windows.Forms.Padding(2)
        Me.LabelControl2.Name = "LabelControl2"
        Me.LabelControl2.Size = New System.Drawing.Size(154, 15)
        Me.LabelControl2.TabIndex = 9
        Me.LabelControl2.Text = "Versión 1.20 (13Abr2020)"
        '
        'HyperlinkLabelControl1
        '
        Me.HyperlinkLabelControl1.Appearance.BackColor = System.Drawing.Color.FromArgb(CType(CType(250, Byte), Integer), CType(CType(183, Byte), Integer), CType(CType(2, Byte), Integer))
        Me.HyperlinkLabelControl1.Appearance.ForeColor = System.Drawing.Color.Tomato
        Me.HyperlinkLabelControl1.Appearance.LinkColor = System.Drawing.Color.Black
        Me.HyperlinkLabelControl1.Appearance.Options.UseBackColor = True
        Me.HyperlinkLabelControl1.Appearance.Options.UseForeColor = True
        Me.HyperlinkLabelControl1.Appearance.Options.UseLinkColor = True
        Me.HyperlinkLabelControl1.AppearanceHovered.ForeColor = System.Drawing.Color.Black
        Me.HyperlinkLabelControl1.AppearanceHovered.LinkColor = DevExpress.LookAndFeel.DXSkinColors.ForeColors.Question
        Me.HyperlinkLabelControl1.AppearanceHovered.Options.UseForeColor = True
        Me.HyperlinkLabelControl1.AppearanceHovered.Options.UseLinkColor = True
        Me.HyperlinkLabelControl1.Location = New System.Drawing.Point(31, 39)
        Me.HyperlinkLabelControl1.Margin = New System.Windows.Forms.Padding(2)
        Me.HyperlinkLabelControl1.Name = "HyperlinkLabelControl1"
        Me.HyperlinkLabelControl1.Size = New System.Drawing.Size(184, 15)
        Me.HyperlinkLabelControl1.TabIndex = 3
        Me.HyperlinkLabelControl1.Text = "Ir al sitio de Cronos Integración"
        '
        'GroupControl1
        '
        Me.GroupControl1.AppearanceCaption.Font = New System.Drawing.Font("Lucida Sans", 9.0!, System.Drawing.FontStyle.Bold)
        Me.GroupControl1.AppearanceCaption.Options.UseFont = True
        Me.GroupControl1.Controls.Add(Me.ComboBoxEdit2)
        Me.GroupControl1.Controls.Add(Me.LabelControl4)
        Me.GroupControl1.Controls.Add(Me.SimpleButton1)
        Me.GroupControl1.Location = New System.Drawing.Point(7, 69)
        Me.GroupControl1.Margin = New System.Windows.Forms.Padding(2)
        Me.GroupControl1.Name = "GroupControl1"
        Me.GroupControl1.Size = New System.Drawing.Size(436, 66)
        Me.GroupControl1.TabIndex = 16
        Me.GroupControl1.Text = "Visualización"
        '
        'ComboBoxEdit2
        '
        Me.ComboBoxEdit2.Location = New System.Drawing.Point(142, 32)
        Me.ComboBoxEdit2.Margin = New System.Windows.Forms.Padding(2)
        Me.ComboBoxEdit2.Name = "ComboBoxEdit2"
        Me.ComboBoxEdit2.Properties.Buttons.AddRange(New DevExpress.XtraEditors.Controls.EditorButton() {New DevExpress.XtraEditors.Controls.EditorButton(DevExpress.XtraEditors.Controls.ButtonPredefines.Combo)})
        Me.ComboBoxEdit2.Properties.Items.AddRange(New Object() {"Normal", "Muy pequeña", "Pequeña", "Grande", "Muy grande", "Extra grande"})
        Me.ComboBoxEdit2.Properties.TextEditStyle = DevExpress.XtraEditors.Controls.TextEditStyles.DisableTextEditor
        Me.ComboBoxEdit2.Size = New System.Drawing.Size(134, 22)
        Me.ComboBoxEdit2.TabIndex = 3
        '
        'LabelControl4
        '
        Me.LabelControl4.Location = New System.Drawing.Point(9, 36)
        Me.LabelControl4.Margin = New System.Windows.Forms.Padding(2)
        Me.LabelControl4.Name = "LabelControl4"
        Me.LabelControl4.Size = New System.Drawing.Size(121, 15)
        Me.LabelControl4.TabIndex = 2
        Me.LabelControl4.Text = "&Tamaño de la fuente"
        '
        'SimpleButton1
        '
        Me.SimpleButton1.ImageOptions.Image = CType(resources.GetObject("SimpleButton1.ImageOptions.Image"), System.Drawing.Image)
        Me.SimpleButton1.ImageOptions.ImageToTextAlignment = DevExpress.XtraEditors.ImageAlignToText.LeftCenter
        Me.SimpleButton1.ImageOptions.Location = DevExpress.XtraEditors.ImageLocation.MiddleLeft
        Me.SimpleButton1.Location = New System.Drawing.Point(290, 30)
        Me.SimpleButton1.Margin = New System.Windows.Forms.Padding(2)
        Me.SimpleButton1.Name = "SimpleButton1"
        Me.SimpleButton1.Size = New System.Drawing.Size(134, 25)
        Me.SimpleButton1.TabIndex = 4
        Me.SimpleButton1.Text = "Inicializar pantalla"
        '
        'SimpleButton3
        '
        Me.SimpleButton3.ImageOptions.ImageToTextAlignment = DevExpress.XtraEditors.ImageAlignToText.LeftCenter
        Me.SimpleButton3.ImageOptions.Location = DevExpress.XtraEditors.ImageLocation.MiddleCenter
        Me.SimpleButton3.ImageOptions.SvgImage = CType(resources.GetObject("SimpleButton3.ImageOptions.SvgImage"), DevExpress.Utils.Svg.SvgImage)
        Me.SimpleButton3.ImageOptions.SvgImageSize = New System.Drawing.Size(48, 48)
        Me.SimpleButton3.Location = New System.Drawing.Point(926, 6)
        Me.SimpleButton3.Margin = New System.Windows.Forms.Padding(2)
        Me.SimpleButton3.Name = "SimpleButton3"
        Me.SimpleButton3.Size = New System.Drawing.Size(58, 54)
        Me.SimpleButton3.TabIndex = 26
        Me.SimpleButton3.ToolTip = "Detiene la aplicación"
        Me.SimpleButton3.ToolTipIconType = DevExpress.Utils.ToolTipIconType.Warning
        Me.SimpleButton3.ToolTipTitle = "Operación delicada"
        '
        'SimpleButton2
        '
        Me.SimpleButton2.ImageOptions.ImageToTextAlignment = DevExpress.XtraEditors.ImageAlignToText.LeftCenter
        Me.SimpleButton2.ImageOptions.Location = DevExpress.XtraEditors.ImageLocation.MiddleCenter
        Me.SimpleButton2.ImageOptions.SvgImage = CType(resources.GetObject("SimpleButton2.ImageOptions.SvgImage"), DevExpress.Utils.Svg.SvgImage)
        Me.SimpleButton2.ImageOptions.SvgImageSize = New System.Drawing.Size(48, 48)
        Me.SimpleButton2.Location = New System.Drawing.Point(926, 5)
        Me.SimpleButton2.Margin = New System.Windows.Forms.Padding(2)
        Me.SimpleButton2.Name = "SimpleButton2"
        Me.SimpleButton2.Size = New System.Drawing.Size(58, 55)
        Me.SimpleButton2.TabIndex = 0
        Me.SimpleButton2.ToolTip = "Reanuda la aplicación"
        Me.SimpleButton2.ToolTipIconType = DevExpress.Utils.ToolTipIconType.Exclamation
        Me.SimpleButton2.Visible = False
        '
        'revisaFlag
        '
        Me.revisaFlag.Enabled = True
        Me.revisaFlag.Interval = 1000
        '
        'escalamiento
        '
        Me.escalamiento.Enabled = True
        Me.escalamiento.Interval = 5000
        '
        'NotifyIcon1
        '
        Me.NotifyIcon1.BalloonTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.NotifyIcon1.BalloonTipText = "Aplicación para monitorer fallas"
        Me.NotifyIcon1.BalloonTipTitle = "Cronos 2019"
        Me.NotifyIcon1.ContextMenuStrip = Me.ContextMenuStrip1
        Me.NotifyIcon1.Icon = CType(resources.GetObject("NotifyIcon1.Icon"), System.Drawing.Icon)
        Me.NotifyIcon1.Text = "Cronos::. Monitor de fallas"
        Me.NotifyIcon1.Visible = True
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.Font = New System.Drawing.Font("Lucida Sans", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.ContextMenuStrip1.ImageScalingSize = New System.Drawing.Size(20, 20)
        Me.ContextMenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.VerElLogToolStripMenuItem, Me.DetenerElMonitorToolStripMenuItem, Me.ToolStripMenuItem1, Me.ReanudarElMonitorToolStripMenuItem})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        Me.ContextMenuStrip1.Size = New System.Drawing.Size(194, 76)
        '
        'VerElLogToolStripMenuItem
        '
        Me.VerElLogToolStripMenuItem.Name = "VerElLogToolStripMenuItem"
        Me.VerElLogToolStripMenuItem.Size = New System.Drawing.Size(193, 22)
        Me.VerElLogToolStripMenuItem.Text = "Ver el log"
        '
        'DetenerElMonitorToolStripMenuItem
        '
        Me.DetenerElMonitorToolStripMenuItem.Name = "DetenerElMonitorToolStripMenuItem"
        Me.DetenerElMonitorToolStripMenuItem.Size = New System.Drawing.Size(190, 6)
        '
        'ToolStripMenuItem1
        '
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        Me.ToolStripMenuItem1.Size = New System.Drawing.Size(193, 22)
        Me.ToolStripMenuItem1.Text = "Detener el monitor"
        '
        'ReanudarElMonitorToolStripMenuItem
        '
        Me.ReanudarElMonitorToolStripMenuItem.Enabled = False
        Me.ReanudarElMonitorToolStripMenuItem.Name = "ReanudarElMonitorToolStripMenuItem"
        Me.ReanudarElMonitorToolStripMenuItem.Size = New System.Drawing.Size(193, 22)
        Me.ReanudarElMonitorToolStripMenuItem.Text = "Reanudar el monitor"
        '
        'ListBoxControl1
        '
        Me.ListBoxControl1.Items.AddRange(New Object() {"2019-Jun-14 23:00:15 Se generó un error", "2019-Jun-14 23:00:15 Se hizo la llamada"})
        Me.ListBoxControl1.Location = New System.Drawing.Point(9, 141)
        Me.ListBoxControl1.Margin = New System.Windows.Forms.Padding(2)
        Me.ListBoxControl1.Name = "ListBoxControl1"
        Me.ListBoxControl1.Size = New System.Drawing.Size(451, 254)
        Me.ListBoxControl1.TabIndex = 31
        '
        'revisarLog
        '
        Me.revisarLog.Enabled = True
        Me.revisarLog.Interval = 3000
        '
        'sinEventos
        '
        Me.sinEventos.Enabled = True
        Me.sinEventos.Interval = 300000
        '
        'BarStaticItem4
        '
        Me.BarStaticItem4.Caption = "210 registros en el visor"
        Me.BarStaticItem4.Id = 5
        Me.BarStaticItem4.ImageOptions.SvgImage = CType(resources.GetObject("BarStaticItem4.ImageOptions.SvgImage"), DevExpress.Utils.Svg.SvgImage)
        Me.BarStaticItem4.Name = "BarStaticItem4"
        Me.BarStaticItem4.PaintStyle = DevExpress.XtraBars.BarItemPaintStyle.CaptionGlyph
        '
        'BarDockControl1
        '
        Me.BarDockControl1.CausesValidation = False
        Me.BarDockControl1.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.BarDockControl1.Location = New System.Drawing.Point(0, 555)
        Me.BarDockControl1.Manager = Nothing
        Me.BarDockControl1.Margin = New System.Windows.Forms.Padding(2)
        Me.BarDockControl1.Size = New System.Drawing.Size(997, 0)
        '
        'BarManager1
        '
        Me.BarManager1.Bars.AddRange(New DevExpress.XtraBars.Bar() {Me.Bar3})
        Me.BarManager1.DockControls.Add(Me.barDockControlTop)
        Me.BarManager1.DockControls.Add(Me.barDockControlBottom)
        Me.BarManager1.DockControls.Add(Me.barDockControlLeft)
        Me.BarManager1.DockControls.Add(Me.barDockControlRight)
        Me.BarManager1.Form = Me
        Me.BarManager1.Items.AddRange(New DevExpress.XtraBars.BarItem() {Me.BarStaticItem1, Me.BarStaticItem3, Me.SkinBarSubItem1})
        Me.BarManager1.MaxItemId = 7
        Me.BarManager1.StatusBar = Me.Bar3
        '
        'Bar3
        '
        Me.Bar3.BarName = "Barra de estado"
        Me.Bar3.CanDockStyle = DevExpress.XtraBars.BarCanDockStyle.Bottom
        Me.Bar3.DockCol = 0
        Me.Bar3.DockRow = 0
        Me.Bar3.DockStyle = DevExpress.XtraBars.BarDockStyle.Bottom
        Me.Bar3.LinksPersistInfo.AddRange(New DevExpress.XtraBars.LinkPersistInfo() {New DevExpress.XtraBars.LinkPersistInfo(Me.SkinBarSubItem1), New DevExpress.XtraBars.LinkPersistInfo(Me.BarStaticItem3), New DevExpress.XtraBars.LinkPersistInfo(Me.BarStaticItem1)})
        Me.Bar3.OptionsBar.AllowQuickCustomization = False
        Me.Bar3.OptionsBar.DrawDragBorder = False
        Me.Bar3.OptionsBar.UseWholeRow = True
        Me.Bar3.Text = "Barra de estado"
        '
        'SkinBarSubItem1
        '
        Me.SkinBarSubItem1.Caption = "Temas"
        Me.SkinBarSubItem1.Id = 5
        Me.SkinBarSubItem1.Name = "SkinBarSubItem1"
        '
        'BarStaticItem3
        '
        Me.BarStaticItem3.Id = 2
        Me.BarStaticItem3.ImageOptions.SvgImage = CType(resources.GetObject("BarStaticItem3.ImageOptions.SvgImage"), DevExpress.Utils.Svg.SvgImage)
        Me.BarStaticItem3.Name = "BarStaticItem3"
        Me.BarStaticItem3.PaintStyle = DevExpress.XtraBars.BarItemPaintStyle.CaptionGlyph
        '
        'BarStaticItem1
        '
        Me.BarStaticItem1.Id = 1
        Me.BarStaticItem1.ImageOptions.SvgImage = CType(resources.GetObject("BarStaticItem1.ImageOptions.SvgImage"), DevExpress.Utils.Svg.SvgImage)
        Me.BarStaticItem1.Name = "BarStaticItem1"
        Me.BarStaticItem1.PaintStyle = DevExpress.XtraBars.BarItemPaintStyle.CaptionGlyph
        '
        'barDockControlTop
        '
        Me.barDockControlTop.CausesValidation = False
        Me.barDockControlTop.Dock = System.Windows.Forms.DockStyle.Top
        Me.barDockControlTop.Location = New System.Drawing.Point(0, 0)
        Me.barDockControlTop.Manager = Me.BarManager1
        Me.barDockControlTop.Margin = New System.Windows.Forms.Padding(2)
        Me.barDockControlTop.Size = New System.Drawing.Size(997, 0)
        '
        'barDockControlBottom
        '
        Me.barDockControlBottom.CausesValidation = False
        Me.barDockControlBottom.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.barDockControlBottom.Location = New System.Drawing.Point(0, 555)
        Me.barDockControlBottom.Manager = Me.BarManager1
        Me.barDockControlBottom.Margin = New System.Windows.Forms.Padding(2)
        Me.barDockControlBottom.Size = New System.Drawing.Size(997, 24)
        '
        'barDockControlLeft
        '
        Me.barDockControlLeft.CausesValidation = False
        Me.barDockControlLeft.Dock = System.Windows.Forms.DockStyle.Left
        Me.barDockControlLeft.Location = New System.Drawing.Point(0, 0)
        Me.barDockControlLeft.Manager = Me.BarManager1
        Me.barDockControlLeft.Margin = New System.Windows.Forms.Padding(2)
        Me.barDockControlLeft.Size = New System.Drawing.Size(0, 555)
        '
        'barDockControlRight
        '
        Me.barDockControlRight.CausesValidation = False
        Me.barDockControlRight.Dock = System.Windows.Forms.DockStyle.Right
        Me.barDockControlRight.Location = New System.Drawing.Point(997, 0)
        Me.barDockControlRight.Manager = Me.BarManager1
        Me.barDockControlRight.Margin = New System.Windows.Forms.Padding(2)
        Me.barDockControlRight.Size = New System.Drawing.Size(0, 555)
        '
        'BarStaticItem2
        '
        Me.BarStaticItem2.Caption = "BarStaticItem1"
        Me.BarStaticItem2.Id = 1
        Me.BarStaticItem2.Name = "BarStaticItem2"
        '
        'reportes
        '
        Me.reportes.Enabled = True
        Me.reportes.Interval = 60000
        '
        'arduino
        '
        Me.arduino.Interval = 1000
        '
        'reenviarMMCALL
        '
        Me.reenviarMMCALL.Interval = 3000
        '
        'borrarArchivos
        '
        Me.borrarArchivos.Enabled = True
        Me.borrarArchivos.Interval = 1000
        '
        'turno
        '
        Me.turno.Enabled = True
        Me.turno.Interval = 5000
        '
        'XtraForm1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(7.0!, 15.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(997, 579)
        Me.Controls.Add(Me.ListBoxControl1)
        Me.Controls.Add(Me.SimpleButton2)
        Me.Controls.Add(Me.SimpleButton3)
        Me.Controls.Add(Me.GroupControl1)
        Me.Controls.Add(Me.HyperlinkLabelControl1)
        Me.Controls.Add(Me.LabelControl2)
        Me.Controls.Add(Me.LabelControl1)
        Me.Controls.Add(Me.PictureEdit1)
        Me.Controls.Add(Me.TileBar1)
        Me.Controls.Add(Me.BarDockControl1)
        Me.Controls.Add(Me.barDockControlLeft)
        Me.Controls.Add(Me.barDockControlRight)
        Me.Controls.Add(Me.barDockControlBottom)
        Me.Controls.Add(Me.barDockControlTop)
        Me.IconOptions.Icon = CType(resources.GetObject("XtraForm1.IconOptions.Icon"), System.Drawing.Icon)
        Me.Margin = New System.Windows.Forms.Padding(2)
        Me.MinimumSize = New System.Drawing.Size(602, 521)
        Me.Name = "XtraForm1"
        Me.Text = "Monitor de eventos"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        CType(Me.PictureEdit1.Properties, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.GroupControl1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupControl1.ResumeLayout(False)
        Me.GroupControl1.PerformLayout()
        CType(Me.ComboBoxEdit2.Properties, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ContextMenuStrip1.ResumeLayout(False)
        CType(Me.ListBoxControl1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.BarManager1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents PictureEdit1 As DevExpress.XtraEditors.PictureEdit
    Friend WithEvents TileBar1 As DevExpress.XtraBars.Navigation.TileBar
    Friend WithEvents LabelControl2 As DevExpress.XtraEditors.LabelControl
    Friend WithEvents LabelControl1 As DevExpress.XtraEditors.LabelControl
    Friend WithEvents HyperlinkLabelControl1 As DevExpress.XtraEditors.HyperlinkLabelControl
    Friend WithEvents GroupControl1 As DevExpress.XtraEditors.GroupControl
    Friend WithEvents SimpleButton1 As DevExpress.XtraEditors.SimpleButton
    Friend WithEvents SimpleButton3 As DevExpress.XtraEditors.SimpleButton
    Friend WithEvents SimpleButton2 As DevExpress.XtraEditors.SimpleButton
    Friend WithEvents ComboBoxEdit2 As DevExpress.XtraEditors.ComboBoxEdit
    Friend WithEvents LabelControl4 As DevExpress.XtraEditors.LabelControl
    Friend WithEvents SerialPort1 As IO.Ports.SerialPort
    Friend WithEvents NotifyIcon1 As NotifyIcon
    Friend WithEvents ContextMenuStrip1 As ContextMenuStrip
    Friend WithEvents VerElLogToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents DetenerElMonitorToolStripMenuItem As ToolStripSeparator
    Friend WithEvents ToolStripMenuItem1 As ToolStripMenuItem
    Friend WithEvents ReanudarElMonitorToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ListBoxControl1 As DevExpress.XtraEditors.ListBoxControl
    Friend WithEvents revisarLog As Timer
    Friend WithEvents sinEventos As Timer
    Friend WithEvents BarStaticItem4 As DevExpress.XtraBars.BarStaticItem
    Friend WithEvents BarDockControl1 As DevExpress.XtraBars.BarDockControl
    Friend WithEvents BarManager1 As DevExpress.XtraBars.BarManager
    Friend WithEvents Bar3 As DevExpress.XtraBars.Bar
    Friend WithEvents BarStaticItem1 As DevExpress.XtraBars.BarStaticItem
    Friend WithEvents barDockControlTop As DevExpress.XtraBars.BarDockControl
    Friend WithEvents barDockControlBottom As DevExpress.XtraBars.BarDockControl
    Friend WithEvents barDockControlLeft As DevExpress.XtraBars.BarDockControl
    Friend WithEvents barDockControlRight As DevExpress.XtraBars.BarDockControl
    Friend WithEvents BarStaticItem2 As DevExpress.XtraBars.BarStaticItem
    Friend WithEvents BarStaticItem3 As DevExpress.XtraBars.BarStaticItem
    Friend WithEvents SkinBarSubItem1 As DevExpress.XtraBars.SkinBarSubItem
    Friend WithEvents escalamiento As Timer
    Friend WithEvents reportes As Timer
    Friend WithEvents arduino As Timer
    Friend WithEvents reenviarMMCALL As Timer
    Friend WithEvents revisaFlag As Timer
    Friend WithEvents borrarArchivos As Timer
    Friend WithEvents turno As Timer
    Friend WithEvents Timer1 As Timer
End Class

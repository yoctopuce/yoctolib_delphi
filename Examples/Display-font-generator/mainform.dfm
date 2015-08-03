object Form1: TForm1
  Left = 351
  Top = 162
  Width = 814
  Height = 223
  VertScrollBar.Visible = False
  Caption = 'Yocto-Font Generator'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  PixelsPerInch = 96
  TextHeight = 13
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 796
    Height = 143
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    TabOrder = 0
    OnMouseMove = ScrollBox1MouseMove
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 9
      Height = 9
      Hint = 'Click to toglle pixel, Ctrl-right/left to shift '
      ParentShowHint = False
      ShowHint = True
      Stretch = True
      OnMouseMove = Image1MouseMove
      OnMouseUp = Image1MouseUp
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 143
    Width = 798
    Height = 22
    Panels = <
      item
        Width = 50
      end
      item
        Width = 100
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object loadfrombmp: TOpenDialog
    Filter = 'bmp files|*.bmp'
    Left = 16
    Top = 64
  end
  object bmpsave: TSaveDialog
    DefaultExt = 'bmp'
    FileName = 'myFont'
    Filter = 'bitmap files|*.bmp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 72
    Top = 8
  end
  object MainMenu1: TMainMenu
    Left = 104
    Top = 8
    object Import1: TMenuItem
      Caption = 'Import'
      object FromSystemFont1: TMenuItem
        Caption = 'From system font...'
        Hint = 'Create a bitmap font from any installed True Type font'
        OnClick = FromSystemFont1Click
      end
      object FromyfFile1: TMenuItem
        Caption = 'From .yfm file'
        Hint = 'Import a font from y Yoctopuce yfm file'
        OnClick = FromyfFile1Click
      end
      object Fromimagefile1: TMenuItem
        Caption = 'From image file'
        Hint = 'Import  from  a previouly exported BMP file'
        OnClick = Fromimagefile1Click
      end
    end
    object ExportMenu: TMenuItem
      Caption = 'Export'
      Enabled = False
      object Toyffile1: TMenuItem
        Caption = 'To .yfm file...'
        Hint = 'Export font to a .yfm file, usable in Yoctopuce displays'
        OnClick = Toyffile1Click
      end
      object Toimagefile1: TMenuItem
        Caption = 'To image file...'
        Hint = 'Export to a BMP file'
        OnClick = Toimagefile1Click
      end
    end
    object ZoomMenu: TMenuItem
      Caption = 'Zoom'
      Enabled = False
      object N1001: TMenuItem
        Caption = 'x1'
        Checked = True
        RadioItem = True
        OnClick = N1001Click
      end
      object N2001: TMenuItem
        Caption = 'x2'
        RadioItem = True
        OnClick = N2001Click
      end
      object N3001: TMenuItem
        Caption = 'x3'
        RadioItem = True
        OnClick = N3001Click
      end
      object N4001: TMenuItem
        Caption = 'x4'
        RadioItem = True
        OnClick = N4001Click
      end
      object N5001: TMenuItem
        Caption = 'x5'
        RadioItem = True
        OnClick = N5001Click
      end
      object N6001: TMenuItem
        Caption = 'x6'
        RadioItem = True
        OnClick = N6001Click
      end
      object x71: TMenuItem
        Caption = 'x7'
        RadioItem = True
        OnClick = x71Click
      end
      object x81: TMenuItem
        Caption = 'x8'
        RadioItem = True
        OnClick = x81Click
      end
      object x91: TMenuItem
        Caption = 'x9'
        RadioItem = True
        OnClick = x91Click
      end
      object x101: TMenuItem
        Caption = 'x10'
        RadioItem = True
        OnClick = x101Click
      end
    end
    object PreviewMenu: TMenuItem
      Caption = 'Preview'
      Enabled = False
      OnClick = PreviewMenuClick
    end
  end
  object YfSave: TSaveDialog
    DefaultExt = 'yfm'
    FileName = 'myfont.yfm'
    Filter = 'Yoctopuce font files|*.yfm'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 16
  end
  object openYf: TOpenDialog
    DefaultExt = 'yfm'
    Filter = 'Yoctopuce font files|*.yfm'
    Left = 16
    Top = 32
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 736
    Top = 112
  end
end

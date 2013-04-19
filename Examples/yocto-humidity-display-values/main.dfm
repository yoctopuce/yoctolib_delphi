object Form1: TForm1
  Left = 320
  Top = 182
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Yocto-humidity'
  ClientHeight = 109
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 54
    Height = 13
    Caption = 'Use device'
  end
  object tempLabel: TLabel
    Left = 40
    Top = 48
    Width = 32
    Height = 24
    Caption = 'N/A'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object HumLabel: TLabel
    Left = 216
    Top = 48
    Width = 32
    Height = 24
    Caption = 'N/A'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 21
    Top = 74
    Width = 60
    Height = 13
    Caption = 'Température'
  end
  object Label3: TLabel
    Left = 189
    Top = 74
    Width = 80
    Height = 13
    Caption = 'Relative humidity'
  end
  object ComboBox1: TComboBox
    Left = 74
    Top = 13
    Width = 215
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 90
    Width = 304
    Height = 19
    Panels = <
      item
        Text = 'Connect any Yocto-Humidity device'
        Width = 50
      end>
    SimplePanel = False
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 296
  end
end

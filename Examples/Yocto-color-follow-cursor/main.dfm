object Form1: TForm1
  Left = 476
  Top = 390
  Width = 325
  Height = 107
  Caption = 'Follow mouse cursor'
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
    Top = 30
    Width = 54
    Height = 13
    Caption = 'Use device'
  end
  object Label2: TLabel
    Left = 8
    Top = 7
    Width = 276
    Height = 13
    Caption = 'Choose a Yocto-Color Device below then move the mouse'
  end
  object ComboBox1: TComboBox
    Left = 74
    Top = 27
    Width = 215
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 50
    Width = 309
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 250
    OnTimer = Timer1Timer
    Left = 8
    Top = 8
  end
end

object Form1: TForm1
  Left = 618
  Top = 292
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'Dual Thermometer'
  ClientHeight = 555
  ClientWidth = 321
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
  object graph: TImage
    Left = 16
    Top = 128
    Width = 297
    Height = 400
  end
  object sensor1: TGroupBox
    Left = 16
    Top = 8
    Width = 145
    Height = 113
    Caption = 'Sensor 1'
    TabOrder = 0
    object temp1: TLabel
      Left = 16
      Top = 40
      Width = 102
      Height = 29
      Caption = 'Tsensor1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -24
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object beacon1: TCheckBox
      Left = 8
      Top = 88
      Width = 97
      Height = 17
      Caption = 'beacon'
      Enabled = False
      TabOrder = 0
      OnClick = beacon1Click
    end
    object ComboBox1: TComboBox
      Left = 8
      Top = 16
      Width = 129
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object GroupBox1: TGroupBox
    Left = 168
    Top = 8
    Width = 145
    Height = 113
    Caption = 'Sensor 2'
    TabOrder = 1
    object temp2: TLabel
      Left = 27
      Top = 40
      Width = 102
      Height = 29
      Caption = 'Tsensor2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -24
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object beacon2: TCheckBox
      Left = 8
      Top = 88
      Width = 97
      Height = 17
      Caption = 'beacon'
      Enabled = False
      TabOrder = 0
      OnClick = beacon2Click
    end
    object ComboBox2: TComboBox
      Left = 8
      Top = 16
      Width = 129
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      TabOrder = 1
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 536
    Width = 321
    Height = 19
    Panels = <
      item
        Text = 'Plug any Yocto-Temperature or Yocto-Humidity device'
        Width = 50
      end>
    SimplePanel = False
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 128
    Top = 88
  end
end

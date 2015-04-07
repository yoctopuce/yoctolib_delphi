object Form1: TForm1
  Left = 1888
  Top = 319
  Width = 564
  Height = 382
  Caption = 'Yocto-Terminal'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 15
    Width = 69
    Height = 13
    Caption = 'Device to use:'
    OnClick = ComboBox1Change
  end
  object Memo1: TMemo
    Left = 8
    Top = 40
    Width = 533
    Height = 257
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyDown = Memo1KeyDown
    OnKeyPress = Memo1KeyPress
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 325
    Width = 548
    Height = 19
    Panels = <
      item
        Text = 'Connect one or more Yoctopuce devices featring a Serial Port'
        Width = 50
      end>
    SimplePanel = False
  end
  object ComboBox1: TComboBox
    Left = 80
    Top = 11
    Width = 381
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 2
    OnChange = ComboBox1Change
  end
  object setupBtn: TButton
    Left = 468
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Setup'
    Enabled = False
    TabOrder = 3
    OnClick = setupBtnClick
  end
  object Edit1: TEdit
    Left = 8
    Top = 300
    Width = 529
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
    OnKeyPress = Edit1KeyPress
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 656
  end
  object Timer2: TTimer
    Interval = 100
    OnTimer = Timer2Timer
    Left = 48
    Top = 656
  end
end

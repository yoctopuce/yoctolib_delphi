object Form2: TForm2
  Left = 592
  Top = 260
  BorderStyle = bsDialog
  Caption = 'Serial Setup'
  ClientHeight = 131
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 16
    Width = 46
    Height = 13
    Caption = 'Baud rate'
  end
  object Label2: TLabel
    Left = 40
    Top = 40
    Width = 45
    Height = 13
    Caption = 'Encoding'
  end
  object Label3: TLabel
    Left = 32
    Top = 64
    Width = 58
    Height = 13
    Caption = 'Flow Control'
  end
  object Button1: TButton
    Left = 185
    Top = 97
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 105
    Top = 97
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 25
    Top = 97
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save'
    TabOrder = 4
    OnClick = Button3Click
  end
  object baudrate: TComboBox
    Left = 112
    Top = 11
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = baudrateChange
    Items.Strings = (
      '110'
      '300'
      '1200'
      '2400'
      '4800'
      '9600'
      '19200'
      '38400'
      '57600'
      '115200')
  end
  object encoding: TComboBox
    Left = 112
    Top = 35
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = baudrateChange
    Items.Strings = (
      '8N1'
      '8E1'
      '8O1'
      '8N2'
      '7N1'
      '7E1'
      '7O1')
  end
  object flowcontrol: TComboBox
    Left = 112
    Top = 59
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = baudrateChange
    Items.Strings = (
      'none'
      'CtsRts'
      'XOnXoff'
      'Snooping'
      'Simplex')
  end
end

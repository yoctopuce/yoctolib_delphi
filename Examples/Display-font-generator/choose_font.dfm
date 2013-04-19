object FontChooser: TFontChooser
  Left = 402
  Top = 187
  Width = 313
  Height = 187
  Caption = 'Import from system font'
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
  object Image1: TImage
    Left = 16
    Top = 63
    Width = 273
    Height = 49
  end
  object ComboBox1: TComboBox
    Left = 16
    Top = 8
    Width = 225
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = ComboBox1Change
  end
  object fontsizeinput: TEdit
    Left = 248
    Top = 8
    Width = 41
    Height = 21
    TabOrder = 1
    Text = '12'
    OnChange = fontsizeinputChange
  end
  object Button1: TButton
    Left = 128
    Top = 119
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 216
    Top = 119
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = Button2Click
  end
  object bold: TCheckBox
    Left = 16
    Top = 32
    Width = 65
    Height = 17
    Caption = 'Bold'
    TabOrder = 4
    OnClick = boldClick
  end
  object italic: TCheckBox
    Left = 88
    Top = 32
    Width = 57
    Height = 17
    Caption = 'Italic'
    TabOrder = 5
    OnClick = italicClick
  end
end

object previewForm: TpreviewForm
  Left = 908
  Top = 197
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'previewForm'
  ClientHeight = 83
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 40
    Top = 8
    Width = 305
    Height = 49
    Alignment = taCenter
    AutoSize = False
    Caption = 'Label1'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 371
    Height = 83
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object Label2: TLabel
      Left = 9
      Top = 11
      Width = 72
      Height = 13
      Caption = 'Display device;'
    end
    object Label3: TLabel
      Left = 8
      Top = 40
      Width = 51
      Height = 13
      Caption = 'To display:'
    end
    object DevList: TComboBox
      Left = 88
      Top = 8
      Width = 273
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = DevListChange
    end
    object messageField: TEdit
      Left = 88
      Top = 32
      Width = 265
      Height = 21
      TabOrder = 1
      Text = 'Hello World!'
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 250
    OnTimer = Timer1Timer
    Left = 336
    Top = 40
  end
end

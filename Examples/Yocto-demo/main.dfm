object Form1: TForm1
  Left = 472
  Top = 243
  BorderStyle = bsDialog
  Caption = 'Yocto-Demo'
  ClientHeight = 386
  ClientWidth = 303
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
    Top = 11
    Width = 39
    Height = 13
    Caption = 'Choose:'
  end
  object deviceimage: TImage
    Left = 0
    Top = 40
    Width = 300
    Height = 300
  end
  object ComboBox1: TComboBox
    Left = 56
    Top = 8
    Width = 244
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 367
    Width = 303
    Height = 19
    Panels = <
      item
        Text = 'Connect a Yocto-Demo device'
        Width = 50
      end>
    SimplePanel = False
  end
  object Beacon: TCheckBox
    Left = 8
    Top = 345
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Beacon'
    TabOrder = 2
    OnClick = BeaconClick
  end
  object led: TCheckBox
    Left = 232
    Top = 345
    Width = 66
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Test Led'
    TabOrder = 3
    OnClick = ledClick
  end
  object InventoryTimer: TTimer
    Enabled = False
    OnTimer = InventoryTimerTimer
    Left = 264
    Top = 304
  end
  object refreshTimer: TTimer
    Enabled = False
    Interval = 200
    OnTimer = refreshTimerTimer
    Left = 232
    Top = 304
  end
end

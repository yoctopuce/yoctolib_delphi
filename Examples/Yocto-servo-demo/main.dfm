object Form1: TForm1
  Left = 434
  Top = 213
  BorderStyle = bsDialog
  Caption = 'Yocto-Servo demo'
  ClientHeight = 298
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
  object Label2: TLabel
    Left = 18
    Top = 59
    Width = 37
    Height = 13
    Caption = 'Servo 1'
  end
  object Label3: TLabel
    Left = 18
    Top = 99
    Width = 37
    Height = 13
    Caption = 'Servo 2'
  end
  object Label4: TLabel
    Left = 18
    Top = 139
    Width = 37
    Height = 13
    Caption = 'Servo 3'
  end
  object Label5: TLabel
    Left = 18
    Top = 179
    Width = 37
    Height = 13
    Caption = 'Servo 4'
  end
  object Label6: TLabel
    Left = 18
    Top = 219
    Width = 37
    Height = 13
    Caption = 'Servo 5'
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
    Top = 279
    Width = 303
    Height = 19
    Panels = <
      item
        Text = 'Connect a Yocto-Demo device'
        Width = 200
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Beacon: TCheckBox
    Left = 8
    Top = 257
    Width = 97
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Beacon'
    TabOrder = 2
    OnClick = BeaconClick
  end
  object TrackBar1: TTrackBar
    Left = 72
    Top = 56
    Width = 229
    Height = 33
    Max = 1000
    Min = -1000
    Orientation = trHorizontal
    PageSize = 250
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 3
    TickMarks = tmBottomRight
    TickStyle = tsManual
  end
  object TrackBar2: TTrackBar
    Left = 72
    Top = 96
    Width = 229
    Height = 33
    Max = 1000
    Min = -1000
    Orientation = trHorizontal
    PageSize = 250
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 4
    TickMarks = tmBottomRight
    TickStyle = tsManual
  end
  object TrackBar3: TTrackBar
    Left = 72
    Top = 136
    Width = 229
    Height = 33
    Max = 1000
    Min = -1000
    Orientation = trHorizontal
    PageSize = 250
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 5
    TickMarks = tmBottomRight
    TickStyle = tsManual
  end
  object TrackBar4: TTrackBar
    Left = 72
    Top = 176
    Width = 229
    Height = 33
    Max = 1000
    Min = -1000
    Orientation = trHorizontal
    PageSize = 250
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 6
    TickMarks = tmBottomRight
    TickStyle = tsManual
  end
  object TrackBar5: TTrackBar
    Left = 72
    Top = 216
    Width = 229
    Height = 33
    Max = 1000
    Min = -1000
    Orientation = trHorizontal
    PageSize = 250
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 7
    TickMarks = tmBottomRight
    TickStyle = tsManual
  end
  object allatonce: TCheckBox
    Left = 200
    Top = 256
    Width = 97
    Height = 17
    Caption = 'All at once'
    TabOrder = 8
  end
  object InventoryTimer: TTimer
    Enabled = False
    OnTimer = InventoryTimerTimer
    Left = 248
    Top = 176
  end
  object refreshTimer: TTimer
    Enabled = False
    Interval = 250
    OnTimer = refreshTimerTimer
    Left = 216
    Top = 176
  end
end

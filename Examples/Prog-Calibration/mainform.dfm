object Form1: TForm1
  Left = 371
  Top = 135
  Anchors = [akLeft, akBottom]
  BorderStyle = bsDialog
  Caption = 'Calibration settings'
  ClientHeight = 295
  ClientWidth = 388
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
    Left = 21
    Top = 39
    Width = 37
    Height = 13
    Caption = 'Device:'
  end
  object Label2: TLabel
    Left = 16
    Top = 59
    Width = 41
    Height = 13
    Caption = 'Function'
  end
  object ValueDisplay: TLabel
    Left = 129
    Top = 98
    Width = 54
    Height = 37
    Alignment = taRightJustify
    Caption = 'N/A'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Arial Narrow'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ValueDisplayUnits: TLabel
    Left = 189
    Top = 98
    Width = 11
    Height = 37
    Caption = '-'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Arial Narrow'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 25
    Top = 210
    Width = 22
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Raw'
  end
  object Label4: TLabel
    Left = 8
    Top = 232
    Width = 47
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Calibrated'
  end
  object Label5: TLabel
    Left = 16
    Top = 8
    Width = 260
    Height = 13
    Caption = 'Connect one or more Yocto-devices featuring a sensor:'
  end
  object Label6: TLabel
    Left = 16
    Top = 152
    Width = 361
    Height = 39
    Caption = 
      'Then enter calibration points in ascending order. Click on save ' +
      'to  store new calibration data in the device. Click cancel to re' +
      'store previous calibration. If you want to disable the calibrati' +
      'on,  clear all fields and click save.'
    WordWrap = True
  end
  object unsupported_warning: TLabel
    Left = 16
    Top = 255
    Width = 323
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 
      'This device does not support calibration,  firmware  upgrade nee' +
      'ded.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object nosensorfunction: TLabel
    Left = 16
    Top = 255
    Width = 208
    Height = 13
    Caption = 'No supported sensor function on this device'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object RawValueDisplay: TLabel
    Left = 264
    Top = 125
    Width = 3
    Height = 13
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowFrame
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object devicesList: TComboBox
    Left = 72
    Top = 35
    Width = 313
    Height = 21
    Hint = 'Choose the device'
    Style = csDropDownList
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnChange = devicesListChange
  end
  object functionsList: TComboBox
    Left = 72
    Top = 59
    Width = 313
    Height = 21
    Hint = 'Choose the function to calibrate'
    Style = csDropDownList
    ItemHeight = 13
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    OnChange = functionsListChange
  end
  object R0: TEdit
    Left = 62
    Top = 205
    Width = 44
    Height = 21
    Anchors = [akLeft, akBottom]
    Enabled = False
    TabOrder = 2
    OnExit = CalibrationChange
  end
  object C0: TEdit
    Left = 62
    Top = 229
    Width = 44
    Height = 21
    Anchors = [akLeft, akBottom]
    Enabled = False
    TabOrder = 3
    OnExit = CalibrationChange
  end
  object R1: TEdit
    Left = 110
    Top = 205
    Width = 44
    Height = 21
    Anchors = [akLeft, akBottom]
    Enabled = False
    TabOrder = 4
    OnExit = CalibrationChange
  end
  object C1: TEdit
    Left = 110
    Top = 229
    Width = 44
    Height = 21
    Anchors = [akLeft, akBottom]
    Enabled = False
    TabOrder = 5
    OnExit = CalibrationChange
  end
  object R2: TEdit
    Left = 158
    Top = 205
    Width = 44
    Height = 21
    Anchors = [akLeft, akBottom]
    Enabled = False
    TabOrder = 6
    OnExit = CalibrationChange
  end
  object C2: TEdit
    Left = 158
    Top = 229
    Width = 44
    Height = 21
    Anchors = [akLeft, akBottom]
    Enabled = False
    TabOrder = 7
    OnExit = CalibrationChange
  end
  object R3: TEdit
    Left = 206
    Top = 205
    Width = 44
    Height = 21
    Anchors = [akLeft, akBottom]
    Enabled = False
    TabOrder = 8
    OnExit = CalibrationChange
  end
  object C3: TEdit
    Left = 206
    Top = 229
    Width = 44
    Height = 21
    Anchors = [akLeft, akBottom]
    Enabled = False
    TabOrder = 9
    OnExit = CalibrationChange
  end
  object R4: TEdit
    Left = 254
    Top = 205
    Width = 44
    Height = 21
    Anchors = [akLeft, akBottom]
    Enabled = False
    TabOrder = 10
    OnExit = CalibrationChange
  end
  object C4: TEdit
    Left = 254
    Top = 229
    Width = 44
    Height = 21
    Anchors = [akLeft, akBottom]
    Enabled = False
    TabOrder = 11
    OnExit = CalibrationChange
  end
  object saveBtn: TButton
    Left = 304
    Top = 229
    Width = 75
    Height = 19
    Hint = 'Store the calibration data in the device flash memory'
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 12
    OnClick = saveBtnClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 276
    Width = 388
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object cancelBtn: TButton
    Left = 304
    Top = 205
    Width = 75
    Height = 19
    Hint = 'Restore previous calibration data'
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    Enabled = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 14
    OnClick = cancelBtnClick
  end
  object InventoryTimer: TTimer
    Enabled = False
    OnTimer = InventoryTimerTimer
    Left = 32
    Top = 100
  end
end

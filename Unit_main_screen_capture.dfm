object FormScreenCapture: TFormScreenCapture
  Left = 0
  Top = 0
  Caption = 'FormScreenCapture'
  ClientHeight = 621
  ClientWidth = 1088
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 18
    Top = 67
    Width = 59
    Height = 13
    Caption = 'Flicker filter:'
  end
  object Label5: TLabel
    Left = 18
    Top = 94
    Width = 70
    Height = 13
    Caption = 'What capture:'
  end
  object LabelWhatCapture: TLabel
    Left = 18
    Top = 124
    Width = 90
    Height = 13
    Caption = 'LabelWhatCapture'
  end
  object MemoMain: TMemo
    Left = 8
    Top = 143
    Width = 1041
    Height = 434
    Lines.Strings = (
      'MemoMain')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object ComboBoxDecklinkCard: TComboBox
    Left = 16
    Top = 8
    Width = 225
    Height = 21
    Style = csDropDownList
    Enabled = False
    TabOrder = 1
    OnChange = ComboBoxDecklinkCardChange
  end
  object ComboBoxDecklinkMode: TComboBox
    Left = 16
    Top = 35
    Width = 225
    Height = 21
    Style = csDropDownList
    Enabled = False
    TabOrder = 2
    OnChange = ComboBoxDecklinkModeChange
  end
  object ButtonStartStop: TButton
    Left = 264
    Top = 8
    Width = 121
    Height = 75
    Caption = 'Start'
    Enabled = False
    TabOrder = 3
    OnClick = ButtonStartStopClick
  end
  object PanelState: TPanel
    Left = 408
    Top = 8
    Width = 649
    Height = 75
    Alignment = taLeftJustify
    TabOrder = 4
    object LabelCaptionD: TLabel
      Left = 8
      Top = 3
      Width = 76
      Height = 13
      Caption = 'Capture buffer:'
    end
    object LabelScalerD: TLabel
      Left = 8
      Top = 22
      Width = 66
      Height = 13
      Caption = 'Scaler buffer:'
    end
    object LabelDecklinkD: TLabel
      Left = 8
      Top = 41
      Width = 75
      Height = 13
      Caption = 'Decklink buffer:'
    end
    object LabelCapture: TLabel
      Left = 104
      Top = 3
      Width = 16
      Height = 13
      Caption = '0/0'
    end
    object LabelScaler: TLabel
      Left = 104
      Top = 22
      Width = 16
      Height = 13
      Caption = '0/0'
    end
    object LabelDecklink: TLabel
      Left = 104
      Top = 41
      Width = 16
      Height = 13
      Caption = '0/0'
    end
    object Label1: TLabel
      Left = 192
      Top = 3
      Width = 72
      Height = 13
      Caption = 'Played frames:'
    end
    object Label2: TLabel
      Left = 192
      Top = 22
      Width = 87
      Height = 13
      Caption = 'Repeated frames:'
    end
    object Label3: TLabel
      Left = 192
      Top = 41
      Width = 64
      Height = 13
      Caption = 'Capture FPS:'
    end
    object LabelFPlayed: TLabel
      Left = 304
      Top = 3
      Width = 6
      Height = 13
      Caption = '0'
    end
    object LabelFRepeated: TLabel
      Left = 304
      Top = 22
      Width = 6
      Height = 13
      Caption = '0'
    end
    object LabelCaptureFPS: TLabel
      Left = 304
      Top = 41
      Width = 6
      Height = 13
      Caption = '0'
    end
  end
  object ComboBoxFlicker: TComboBox
    Left = 112
    Top = 62
    Width = 129
    Height = 21
    Style = csDropDownList
    TabOrder = 5
    OnChange = ComboBoxFlickerChange
    Items.Strings = (
      'Off'
      'Soft'
      'Strong')
  end
  object ComboBoxWhatCapture: TComboBox
    Left = 112
    Top = 89
    Width = 129
    Height = 21
    Style = csDropDownList
    TabOrder = 6
    OnChange = ComboBoxWhatCaptureChange
    Items.Strings = (
      'Full screen'
      'Region'
      'Window (full)'
      'Window (client)')
  end
  object ButtonSelect: TButton
    Left = 264
    Top = 89
    Width = 121
    Height = 21
    Caption = 'Select'
    TabOrder = 7
    OnClick = ButtonSelectClick
  end
  object ComboBoxAR: TComboBox
    Left = 264
    Top = 116
    Width = 121
    Height = 21
    Style = csDropDownList
    TabOrder = 8
    Items.Strings = (
      'AR free'
      'AR 4:3'
      'AR 16:9')
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 448
    Top = 120
  end
end

object FormWindowSelect: TFormWindowSelect
  Left = 0
  Top = 0
  Caption = 'Select window to caption'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 240
    Width = 129
    Height = 13
    Caption = 'Name of selected window: '
  end
  object ListBoxForms: TListBox
    Left = 8
    Top = 8
    Width = 619
    Height = 217
    ItemHeight = 13
    TabOrder = 0
    OnClick = ListBoxFormsClick
  end
  object BitBtn1: TBitBtn
    Left = 223
    Top = 266
    Width = 75
    Height = 25
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 1
  end
  object BitBtnCancel: TBitBtn
    Left = 304
    Top = 266
    Width = 75
    Height = 25
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 2
  end
  object EditSelected: TEdit
    Left = 145
    Top = 236
    Width = 482
    Height = 21
    TabOrder = 3
    Text = 'EditSelected'
  end
end

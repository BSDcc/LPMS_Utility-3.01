object FLPMS_FirstRunMultiCpy: TFLPMS_FirstRunMultiCpy
  Left = 0
  Top = 0
  Caption = 'LPMS - Utility'
  ClientHeight = 66
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 45
    Height = 13
    Caption = 'DBPrefix:'
  end
  object edtDBPrefix: TEdit
    Left = 80
    Top = 8
    Width = 121
    Height = 21
    MaxLength = 6
    TabOrder = 0
    OnChange = edtDBPrefixChange
  end
  object cbMultiCpy: TCheckBox
    Left = 216
    Top = 10
    Width = 145
    Height = 17
    Caption = ' Multi Company Support'
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 271
    Top = 33
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 190
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object jvCipher: TJvVigenereCipher
    Left = 32
    Top = 32
  end
end

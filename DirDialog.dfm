object Form1: TForm1
  Left = 319
  Top = 262
  Width = 257
  Height = 391
  Caption = 'Choose Directory'
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
  object DirCombo: TShellComboBox
    Left = 8
    Top = 8
    Width = 233
    Height = 22
    Root = 'rfDesktop'
    ShellTreeView = DirTree
    UseShellImages = True
    DropDownCount = 8
    TabOrder = 0
    OnClick = DirComboClick
  end
  object DirTree: TShellTreeView
    Left = 8
    Top = 32
    Width = 233
    Height = 249
    ObjectTypes = [otFolders]
    Root = 'rfDesktop'
    ShellComboBox = DirCombo
    UseShellImages = True
    AutoRefresh = False
    Indent = 19
    ParentColor = False
    RightClickSelect = True
    ShowRoot = False
    TabOrder = 1
    OnClick = DirTreeClick
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 320
    Width = 75
    Height = 25
    TabOrder = 2
    OnClick = BitBtn1Click
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 88
    Top = 320
    Width = 75
    Height = 25
    TabOrder = 3
    OnClick = BitBtn2Click
    Kind = bkCancel
  end
  object ResultDir: TEdit
    Left = 8
    Top = 288
    Width = 233
    Height = 21
    TabOrder = 4
    Text = 'ResultDir'
  end
end

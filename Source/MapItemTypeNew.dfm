object MapItemTypeNewForm: TMapItemTypeNewForm
  Left = 35
  Top = 334
  Width = 389
  Height = 277
  Caption = 'Item Type'
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
  object LabelType: TLabel
    Left = 16
    Top = 16
    Width = 137
    Height = 25
    Caption = 'Name'
  end
  object LabelGeometry: TLabel
    Left = 16
    Top = 72
    Width = 137
    Height = 25
    Caption = 'Select one geometry'
  end
  object Label1: TLabel
    Left = 168
    Top = 16
    Width = 201
    Height = 25
    Caption = 'Old ones'
  end
  object NewItemType: TEdit
    Left = 16
    Top = 40
    Width = 137
    Height = 21
    TabOrder = 0
    Text = 'NewItemType'
  end
  object ItemListBox: TListBox
    Left = 168
    Top = 40
    Width = 201
    Height = 193
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
    OnDblClick = ItemListBoxDblClick
  end
  object GeomPoint: TRadioButton
    Left = 16
    Top = 96
    Width = 129
    Height = 25
    Caption = 'Point '
    TabOrder = 2
  end
  object GeomLine: TRadioButton
    Left = 16
    Top = 120
    Width = 137
    Height = 25
    Caption = 'Line'
    TabOrder = 3
  end
  object GeomArea: TRadioButton
    Left = 16
    Top = 168
    Width = 137
    Height = 25
    Caption = 'Area'
    TabOrder = 4
  end
  object GeomCubic: TRadioButton
    Left = 16
    Top = 144
    Width = 137
    Height = 25
    Caption = 'Curve'
    TabOrder = 5
  end
  object ButtonOk: TBitBtn
    Left = 8
    Top = 208
    Width = 65
    Height = 25
    TabOrder = 6
    OnClick = ButtonOKClick
    Kind = bkOK
  end
  object ButtonCancel: TBitBtn
    Left = 80
    Top = 208
    Width = 81
    Height = 25
    TabOrder = 7
    OnClick = ButtonCancelClick
    Kind = bkCancel
  end
end

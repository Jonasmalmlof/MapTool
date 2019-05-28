object PrefForm: TPrefForm
  Left = 232
  Top = 327
  Hint = 'Set preferenses'
  AutoScroll = False
  Caption = 'Preferences'
  ClientHeight = 317
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 481
    Height = 273
    ActivePage = TabFont
    TabOrder = 3
    object TabHighlight: TTabSheet
      Caption = 'Highlighting'
      object HighLight: TGroupBox
        Left = 8
        Top = 8
        Width = 225
        Height = 129
        Hint = 'Attributes for highlighting lines and curves'
        Caption = 'Item Highlight (lines and curves) '
        TabOrder = 0
        object Label1: TLabel
          Left = 8
          Top = 27
          Width = 34
          Height = 13
          Caption = 'Speed '
        end
        object Label2: TLabel
          Left = 152
          Top = 27
          Width = 47
          Height = 13
          Caption = '(mm/sec) '
        end
        object Label3: TLabel
          Left = 149
          Top = 59
          Width = 25
          Height = 13
          Caption = '(mm) '
        end
        object HighLightSpeed: TEdit
          Left = 56
          Top = 24
          Width = 89
          Height = 21
          Hint = 'Set the speed of highlighting lines and curves'
          TabOrder = 0
          Text = 'HighLightSpeed'
        end
        object NiftyCircel: TCheckBox
          Left = 8
          Top = 56
          Width = 73
          Height = 17
          Hint = 'Show a circle'
          Caption = 'Circel '
          TabOrder = 1
        end
        object NiftyLine: TCheckBox
          Left = 8
          Top = 80
          Width = 81
          Height = 17
          Hint = 'Show a increasing line'
          Caption = 'Line '
          TabOrder = 2
        end
        object NiftyMapInCen: TCheckBox
          Left = 8
          Top = 104
          Width = 119
          Height = 17
          Hint = 'Move the map also'
          Caption = 'Map in Middle '
          TabOrder = 3
        end
        object NiftyCircleWdt: TComboBox
          Left = 80
          Top = 56
          Width = 65
          Height = 21
          Hint = 'Set width of circle'
          ItemHeight = 13
          TabOrder = 4
          Text = 'NiftyCircleWdt'
          Items.Strings = (
            '1'
            '2'
            '3'
            '4'
            '5'
            '6'
            '7'
            '8'
            '9')
        end
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 144
        Width = 225
        Height = 89
        Hint = 'Attributes for highlighting points'
        Caption = 'Item Highlight (points) '
        TabOrder = 1
        object Label4: TLabel
          Left = 8
          Top = 27
          Width = 31
          Height = 13
          Caption = 'Width '
        end
        object Label5: TLabel
          Left = 149
          Top = 27
          Width = 25
          Height = 13
          Caption = '(mm) '
        end
        object Label6: TLabel
          Left = 149
          Top = 51
          Width = 25
          Height = 13
          Caption = '(mm) '
        end
        object Label7: TLabel
          Left = 8
          Top = 51
          Width = 30
          Height = 13
          Caption = 'Turns '
        end
        object NiftyPointWdt: TComboBox
          Left = 80
          Top = 24
          Width = 65
          Height = 21
          Hint = 'Set width of circle'
          ItemHeight = 13
          TabOrder = 0
          Text = 'NiftyCircleWdt'
          Items.Strings = (
            '10'
            '20'
            '30'
            '40'
            '60'
            '80'
            '100'
            '200')
        end
        object NiftyPointTurns: TComboBox
          Left = 80
          Top = 48
          Width = 65
          Height = 21
          Hint = 'Set number of turns to draw'
          ItemHeight = 13
          TabOrder = 1
          Text = 'NiftyCircleWdt'
          Items.Strings = (
            '1'
            '2'
            '3'
            '4'
            '5'
            '6')
        end
      end
      object GroupBox2: TGroupBox
        Left = 240
        Top = 8
        Width = 225
        Height = 105
        Hint = 'Attributes for highlighting areas'
        Caption = 'Area Hightlight '
        TabOrder = 2
        object NiftyAreaRotate: TCheckBox
          Left = 16
          Top = 48
          Width = 97
          Height = 17
          Hint = 'Let Area rotate and expand'
          Caption = 'Rotate To'
          TabOrder = 0
        end
        object NiftyAreaRand: TCheckBox
          Left = 112
          Top = 48
          Width = 97
          Height = 17
          Caption = 'Randomize '
          TabOrder = 1
        end
        object NiftyAreaIn: TCheckBox
          Left = 16
          Top = 72
          Width = 97
          Height = 17
          Hint = 'Scale area from 0 to 1.0'
          Caption = 'Scale Out/In'
          TabOrder = 2
        end
        object NiftyPolyOn: TCheckBox
          Left = 16
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Area / Line'
          TabOrder = 3
        end
      end
    end
    object TabTools: TTabSheet
      Caption = 'Tools'
      ImageIndex = 1
    end
    object TabFont: TTabSheet
      Caption = 'Font'
      ImageIndex = 2
      object Label8: TLabel
        Left = 216
        Top = 20
        Width = 49
        Height = 25
        Caption = 'Height'
      end
      object FontList: TListBox
        Left = 16
        Top = 8
        Width = 185
        Height = 225
        Style = lbOwnerDrawVariable
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 16
        ParentFont = False
        TabOrder = 0
        OnClick = FontListClick
        OnDrawItem = FontListDrawItem
        OnMeasureItem = FontListMeasureItem
      end
      object FontEx: TPanel
        Left = 216
        Top = 192
        Width = 249
        Height = 41
        Caption = 'FontEx'
        TabOrder = 1
      end
      object FontComboHeight: TComboBox
        Left = 280
        Top = 16
        Width = 81
        Height = 21
        ItemHeight = 13
        TabOrder = 2
        Text = '10'
        OnClick = FontComboHeightClick
        Items.Strings = (
          '6'
          '7'
          '8'
          '9'
          '10'
          '11'
          '12'
          '14'
          '16'
          '18'
          '20'
          '24'
          '28'
          '32')
      end
      object CheckUnder: TCheckBox
        Left = 216
        Top = 96
        Width = 97
        Height = 17
        Caption = 'Understrike'
        TabOrder = 3
        OnClick = CheckUnderClick
      end
      object CheckStrike: TCheckBox
        Left = 216
        Top = 120
        Width = 97
        Height = 17
        Caption = 'Strike Through'
        TabOrder = 4
        OnClick = CheckStrikeClick
      end
      object CheckItalic: TCheckBox
        Left = 216
        Top = 48
        Width = 97
        Height = 17
        Caption = 'Italic'
        TabOrder = 5
        OnClick = CheckItalicClick
      end
      object CheckBold: TCheckBox
        Left = 216
        Top = 72
        Width = 97
        Height = 17
        Caption = 'Bold'
        TabOrder = 6
        OnClick = CheckBoldClick
      end
    end
  end
  object ButtonOk: TBitBtn
    Left = 8
    Top = 288
    Width = 65
    Height = 25
    TabOrder = 0
    OnClick = ButtonOkClick
    Kind = bkOK
  end
  object ButtonCancel: TBitBtn
    Left = 80
    Top = 288
    Width = 75
    Height = 25
    TabOrder = 1
    OnClick = ButtonCancelClick
    Kind = bkCancel
  end
  object ButtonHint: TBitBtn
    Left = 160
    Top = 288
    Width = 73
    Height = 25
    Hint = 'Turn Hint On/Off'
    Caption = 'Hint'
    TabOrder = 2
    OnClick = ButtonHintClick
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33333333FFFFFFFFFFFF333000000000000F3330FFFFFFFFFF0F3330F77FF7F7
      F70F3330FF7FF7F7FF0F3330FF7FFFF7FF0F3FF0FF77F7F7FF0F0000FFFFFFFF
      FF0F3000000000000003300F3300F333333330000000F3333333300F3300F333
      3333300FF300FF33333300003000033333333333333333333333}
  end
end

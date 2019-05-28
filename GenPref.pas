unit GenPref;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Math,

  MapItem, ComCtrls, ExtCtrls;

type
  TPrefForm = class(TForm)
    ButtonOk: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonHint: TBitBtn;
    PageControl1: TPageControl;
    TabHighlight: TTabSheet;
    HighLight: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    HighLightSpeed: TEdit;
    NiftyCircel: TCheckBox;
    NiftyLine: TCheckBox;
    NiftyMapInCen: TCheckBox;
    NiftyCircleWdt: TComboBox;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    NiftyPointWdt: TComboBox;
    NiftyPointTurns: TComboBox;
    GroupBox2: TGroupBox;
    NiftyAreaRotate: TCheckBox;
    NiftyAreaRand: TCheckBox;
    NiftyAreaIn: TCheckBox;
    NiftyPolyOn: TCheckBox;
    TabTools: TTabSheet;
    TabFont: TTabSheet;
    FontList: TListBox;
    FontEx: TPanel;
    FontComboHeight: TComboBox;
    Label8: TLabel;
    CheckUnder: TCheckBox;
    CheckStrike: TCheckBox;
    CheckItalic: TCheckBox;
    CheckBold: TCheckBox;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonHintClick(Sender: TObject);
    procedure FontListMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure FontListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FontListClick(Sender: TObject);
    procedure FontComboHeightClick(Sender: TObject);
    procedure CheckUnderClick(Sender: TObject);
    procedure CheckStrikeClick(Sender: TObject);
    procedure CheckBoldClick(Sender: TObject);
    procedure CheckItalicClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure PrefInit (const PrefRec : TNiftyRec);
    function  PrefUpdate (var PrefRec : TNiftyRec) : boolean;


    { Public declarations }
  end;

var
  PrefForm: TPrefForm;
  bOK : boolean;

implementation

{$R *.dfm}

//------------------------------------------------------------------------------
// Initiate form
//
procedure TPrefForm.FormCreate(Sender: TObject);
var
  i : integer;
begin
  bOK := false;
  Self.ShowHint := true;

  if Self.ShowHint then
    ButtonHint.Caption := 'Off'
  else
    ButtonHint.Caption := 'On';

  // Poulate Font ListBox

  FontList.Items := Screen.Fonts;

end;
//------------------------------------------------------------------------------
// Initiate alla data
//
procedure TPrefForm.PrefInit(const PrefRec : TNiftyRec);
var
  i : integer;
begin
  Self.HighLightSpeed.Text   := IntToStr(PrefRec.LineSpeed);
  Self.NiftyCircel.Checked   := PrefRec.LineCircleOn;
  Self.NiftyLine.Checked     := PrefRec.LineOn;
  Self.NiftyMapInCen.Checked := PrefRec.LineMapCenOn;
  Self.NiftyCircleWdt.Text   := IntToStr(PrefRec.LineCircleWdt);

  Self.NiftyPointWdt.text    := IntToStr(PrefRec.PointWdt);
  Self.NiftyPointTurns.text  := IntToStr(PrefRec.PointTurns);

  Self.NiftyAreaRotate.Checked := PrefRec.AreaRotateOn;
  Self.NiftyAreaRand.Checked   := PrefRec.AreaRandOn;
  Self.NiftyAreaIn.Checked     := PrefRec.AreaScaleInOn;
  Self.NiftyPolyOn.Checked     := PrefRec.AreaPolyOn;

  
  // Walk the list box and find match

  for i := 0 to FontList.Items.Count - 1 do
    if CompareStr(FontList.Items[i], Self.Font.Name) = 0 then
      begin
        FontList.ItemIndex := i;
        break;
      end;

  FontComboHeight.Text := IntToStr(Self.Font.Size);
  FontComboHeight.Font.Name := Self.Font.Name;

  CheckItalic.Checked := (fsItalic in Self.Font.Style);
  CheckBold.Checked := (fsBold in Self.Font.Style);
  CheckUnder.Checked := (fsUnderline in Self.Font.Style);
  CheckStrike.Checked := (fsStrikeOut in Self.Font.Style);

end;
//------------------------------------------------------------------------------
// Return all data
//
function TPrefForm.PrefUpdate(var PrefRec : TNiftyRec): boolean;
begin
  if bOK then
    begin
      PrefRec.LineSpeed     := StrToInt(Self.HighLightSpeed.Text);
      PrefRec.LineCircleOn  := Self.NiftyCircel.Checked;
      PrefRec.LineOn        := Self.NiftyLine.Checked;
      PrefRec.LineMapCenOn  := Self.NiftyMapInCen.Checked;
      PrefRec.LineCircleWdt := Max(1, StrToInt(Self.NiftyCircleWdt.Text));

      PrefRec.PointWdt   := Max(1,StrToInt(Self.NiftyPointWdt.Text));
      PrefRec.PointTurns := Max(1,StrToInt(Self.NiftyPointTurns.Text));

      PrefRec.AreaRotateOn  := Self.NiftyAreaRotate.Checked;
      PrefRec.AreaRandOn    := Self.NiftyAreaRand.Checked;
      PrefRec.AreaScaleInOn := Self.NiftyAreaIn.Checked;
      PrefRec.AreaPolyOn    := Self.NiftyPolyOn.Checked;
      PrefRec.Font          := FontEx.Font
    end;
  PrefUpdate := bOK;
end;

procedure TPrefForm.ButtonOkClick(Sender: TObject);
begin
  bOK := true;
  Self.Close;
end;


procedure TPrefForm.ButtonCancelClick(Sender: TObject);
begin
  bOK := false;
  Self.Close;
end;

procedure TPrefForm.ButtonHintClick(Sender: TObject);
begin
  Self.ShowHint := not Self.ShowHint;
  if Self.ShowHint then
    ButtonHint.Caption := 'Off'
  else
    ButtonHint.Caption := 'On'
end;

procedure TPrefForm.FontListMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  if Control = FontList then
    if (Index >= 0) and (Index < FontList.Items.Count) then
    begin
      FontList.Canvas.Font.Name := FontList.Items[Index];
      FontList.Canvas.Font.Size := 0;
      Height := FontList.Canvas.TextHeight('Wg') + 2;
    end;
end;


procedure TPrefForm.FontListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  if Control = FontList then
    begin
      FontList.Canvas.FillRect(Rect);
      FontList.Canvas.Font.Name := FontList.Items[Index];
      FontList.Canvas.Font.Size := 0;
      FontList.Canvas.TextOut(Rect.Left+1, Rect.Top+1, FontList.Items[Index]);
    end;
end;

procedure TPrefForm.FontListClick(Sender: TObject);
begin
  FontEx.Font.Name := FontList.Items[FontList.ItemIndex];
  FontEx.Caption := 'Is this good enough?';
end;

procedure TPrefForm.FontComboHeightClick(Sender: TObject);
begin
  FontEx.Font.Size := StrToInt(FontComboHeight.Text);
end;

procedure TPrefForm.CheckUnderClick(Sender: TObject);
begin
  if CheckUnder.Checked then
    FontEx.Font.Style := FontEx.Font.Style + [fsUnderline]
  else
    FontEx.Font.Style := FontEx.Font.Style - [fsUnderline]
end;

procedure TPrefForm.CheckStrikeClick(Sender: TObject);
begin
  if CheckStrike.Checked then
    FontEx.Font.Style := FontEx.Font.Style + [fsStrikeOut]
  else
    FontEx.Font.Style := FontEx.Font.Style - [fsStrikeOut]
end;

procedure TPrefForm.CheckBoldClick(Sender: TObject);
begin
  if CheckBold.Checked then
    FontEx.Font.Style := FontEx.Font.Style + [fsBold]
  else
    FontEx.Font.Style := FontEx.Font.Style - [fsBold]
end;

procedure TPrefForm.CheckItalicClick(Sender: TObject);
begin
  if CheckItalic.Checked then
    FontEx.Font.Style := FontEx.Font.Style + [fsItalic]
  else
    FontEx.Font.Style := FontEx.Font.Style - [fsItalic]
end;

end.

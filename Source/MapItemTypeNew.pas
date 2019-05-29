//------------------------------------------------------------------------------
//
// Modal dialog for creating e new Item Type
//
// Cre 2004-02-17 Pma
//
//------------------------------------------------------------------------------
unit MapItemTypeNew;

interface

uses
  SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, StrUtils, Buttons,

  MapItemType;

type
  TMapItemTypeNewForm = class(TForm)
    NewItemType: TEdit;
    LabelType: TLabel;
    LabelGeometry: TLabel;
    Label1: TLabel;
    ItemListBox: TListBox;
    GeomPoint: TRadioButton;
    GeomLine: TRadioButton;
    GeomArea: TRadioButton;
    GeomCubic: TRadioButton;
    ButtonOk: TBitBtn;
    ButtonCancel: TBitBtn;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ItemListBoxDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    function InqNew           : boolean; // Return true if user pressed OK
    function InqTypeName      : string;  // Return name of the new type
    function InqGeometryName  : string;  // Return geometry of the new type
  end;

var
  MapItemTypeNewForm: TMapItemTypeNewForm;
  MapItemTypeIn : TMapItemType;
  bOK : boolean = false;

implementation

{$R *.dfm}
//------------------------------------------------------------------------------
// User did want this new item
//
procedure TMapItemTypeNewForm.ButtonOKClick(Sender: TObject);
begin
  bOk := true;
  Close;
end;
//------------------------------------------------------------------------------
// User canceled
//
procedure TMapItemTypeNewForm.ButtonCancelClick(Sender: TObject);
begin
  bOk := false;
  Close;
end;
//------------------------------------------------------------------------------
// Create the form
//
procedure TMapItemTypeNewForm.FormCreate(Sender: TObject);
begin

  MapItemTypeIn := nil;
  
  NewItemType.Text := 'New Type';

  GeomPoint.Checked := true;
  GeomLine.Checked  := false;
  GeomCubic.Checked := false;
  GeomArea.Checked  := false;
end;
//------------------------------------------------------------------------------
// Return true if user whants a new item type
//
function TMapItemTypeNewForm.InqNew : boolean;
begin
  InqNew := bOK;
end;
//------------------------------------------------------------------------------
// Return the name of the type
//
function TMapItemTypeNewForm.InqTypeName         : string;
begin
  InqTypeName := NewItemType.Text;
end;
//------------------------------------------------------------------------------
// Return the name of the geometry
//
function TMapItemTypeNewForm.InqGeometryName : string;
begin
  if GeomArea.Checked then
    InqGeometryName := 'Area'
  else if GeomLine.Checked then
    InqGeometryName := 'Line'
  else if GeomCubic.Checked then
    InqGeometryName := 'Curve'
  else
    InqGeometryName := 'Point'

end;

procedure TMapItemTypeNewForm.ItemListBoxDblClick(Sender: TObject);
var
  sName : string;
  sGeom : string;
  i     : integer;
begin

  sName := ItemListBox.Items[ItemListBox.ItemIndex];
  for i := length(sName) downto 1 do
    if sName[i] = '(' then
      begin
        sGeom := AnsiMidStr(sName,i + 1, length(sname) - i - 1);
        sName := AnsiLeftStr(sName,i - 2);

        NewItemType.Text := sname;
        case sGeom[1] of
        'L' : GeomLine.Checked  := true;
        'C' : GeomCubic.Checked := true;
        'A' : GeomArea.Checked  := true;
        else
          GeomPoint.Checked := true;
        end;
      end;
end;

end.

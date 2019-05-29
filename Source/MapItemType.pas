//------------------------------------------------------------------------------
//
// All functions that handles Map Items Types
//
// Cre 2004-02-17 Pma
//
//------------------------------------------------------------------------------
unit MapItemType;

interface

uses
  Math,     // Mathematics
  SysUtils, // String conversions used
  Types,    // TPoint
  Classes,  // TList
  LeafUnit, // Save/Loading
  Graphics; // TColor

const
  // Save/Load

  LeafItemTypeName     = 0;
  LeafItemTypeGeometry = 1;
  LeafItemTypeColor    = 2;


  type

  TMapItemGeometry = (gtPoint, gtLine, gtCubic, gtArea);

  TMapItemIni = record
    TypeName : string;
    TypeGeom : string;
  end;

  // The Map Item Type class

  TMapItemType = class(TObject)
  private
    ItemTypeName     : string;            // Name of Item Type
    ItemTypeGeometry : TMapItemGeometry;  // Geometry shape of item type
    ItemTypeVisible  : boolean;           // True if item type is visible
    ItemTypeColor    : TColor;            // Color for this item
  public
    constructor Create(name : string; gt : TMapItemGeometry); overload;

    // No destructir is neccessary
    
    function    InqName : string;               // Return name of item type
    function    InqGeometry : TMapItemGeometry; // Return the geometry type
    procedure   SetGeometry (gt : TMapItemGeometry);
    function    InqGeometryName : string;       // Return the name of geometry
    function    InqGeometryId (gName : string) : TMapItemGeometry;
    function    InqVisible : boolean;           // Return if type is visible
    procedure   SetVisible (vis : boolean);     // Set the visibility
    function    InqColor : TColor;              // Return color
    procedure   SetColor (col : TColor);        // Set color

    function    SaveToFile (var F : TextFIle) : boolean;
    function    LoadFromFile (var F : TextFile) : boolean;
  end;

implementation

uses
  GenUtils,         // My utils
  GeomUtils;        // Geometrical utilities

//------------------------------------------------------------------------------
// Save / Load Item Types
//
var
  LeafsItemType : TLeafRecordArray;



//------------------------------------------------------------------------------
// Create the Item type
//
constructor TMapItemType.Create (name : string; gt : TMapItemGeometry) ;
begin
  inherited Create;

  ItemTypeName     := name;
  ItemTypeGeometry := gt;
  ItemTypeVisible  := true;
  ItemTypeColor    := clRed;
end;
//------------------------------------------------------------------------------
// Get the name
//
function TMapItemType.InqName : string;
begin
  InqName := ItemTypeName;
end;
//------------------------------------------------------------------------------
// Get the geoemtry type
//
function TMapItemType.InqGeometry : TMapItemGeometry;
begin
  InqGeometry := ItemTypeGeometry;
end;
//------------------------------------------------------------------------------
// Set the geoemtry type
//
procedure TMapItemType.SetGeometry(gt : TMapItemGeometry);
begin
  ItemTypeGeometry := gt;
end;
//------------------------------------------------------------------------------
// Get the geoemtry type name
//
function TMapItemType.InqGeometryName : string;
begin
  Case ItemTypeGeometry of
    gtPoint : InqGeometryName := 'Point';
    gtLine  : InqGeometryName := 'Line';
    gtCubic : InqGeometryName := 'Curve';
    gtArea  : InqGeometryName := 'Area';
  end;
end;
//------------------------------------------------------------------------------
// Get the geoemtry type name
//
function TMapItemType.InqGeometryId (gName : string) : TMapItemGeometry;
begin
  InqGeometryId := gtPoint;

  if length(gName) > 0 then
    case gName[1] of
      'L': InqGeometryId := gtLine;
      'C': InqGeometryId := gtCubic;
      'A': InqGeometryId := gtArea;
    end;
end;
//------------------------------------------------------------------------------
// Return if item is visible
//
function TMapItemType.InqVisible : boolean;
begin
  InqVisible := ItemTypeVisible;
end;
//------------------------------------------------------------------------------
// Set visible
//
procedure TMapItemType.SetVisible (vis :boolean);
begin
  ItemTypeVisible := vis;
end;
//------------------------------------------------------------------------------
// Return color
//
function TMapItemType.InqColor : TColor;
begin
  InqColor := ItemTypeColor;
end;
//------------------------------------------------------------------------------
// Set color
//
procedure TMapItemType.SetColor (col : TColor);
begin
  ItemTypeColor := col;
end;
//------------------------------------------------------------------------------
// Save this item type to file
//
function TMapItemType.SaveToFile (var F : TextFIle) : boolean;
begin
  SaveToFile := true;

  { Save semantics
    The Calling function will wrap it into itemtype

    <itemtypename='string'>
    <itemtypegeom=integer>
    <itemtypecolor=Hex>

  }

  WriteLn(F, '<' + LeafGetName(LeafsItemType,LeafItemTypeName) + '=' +
                   LeafStrToStr (ItemTypeName) +
             '>');

  WriteLn(F, '<' + LeafGetName(LeafsItemType,LeafItemTypeGeometry) + '=' +
                   LeafStrToStr (InqGeometryName()) +
             '>');

  WriteLn(F, '<' + LeafGetName(LeafsItemType,LeafItemTypeColor) + '=' +
                   LeafColorToStr(ItemTypeColor) +
             '>');
end;
//------------------------------------------------------------------------------
// Load this item from to file
//
function TMapItemType.LoadFromFile (var F : TextFile) : boolean;
var
  sBuf : string;
  idA  : integer;
begin
  LoadFromFile := false;

  while not Eof(F) do
    begin
      // Get the first object

      Readln(F, sBuf); // Syntax : <objectname=

      idA := LeafGetId(LeafsItemType, LeafGetObjectName(sBuf));
      case idA of
        LeafObjectAtEnd  : break;
        LeafItemTypeName     : ItemTypeName  := LeafGetValueStr  (sBuf);
        LeafItemTypeGeometry : ItemTypeGeometry  := InqGeometryId(LeafGetValueStr(sBuf));
        LeafItemTypeColor    : ItemTypeColor := LeafColorFromStr(LeafGetValueStr(sBuf));
      else
        // Unknown object, skip it
        LeafSkipObject(F);
      end;
    end;
end;

initialization

// Add all necessary leafs for this object

  LeafAdd (LeafsItemType, LeafItemTypeName,     'Name',     atString);
  LeafAdd (LeafsItemType, LeafItemTypeGeometry, 'Geometry', atInteger);
  LeafAdd (LeafsItemType, LeafItemTypeColor,    'Color',    atColor);

end.
 
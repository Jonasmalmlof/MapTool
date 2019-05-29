//------------------------------------------------------------------------------
//
// Map Item object that handles all information about one Item
//
// Cre 2004-02-17 Pma
//
//------------------------------------------------------------------------------
unit MapItem;

interface

uses
  SysUtils,     // String conversions
  Types,        // TPoint
  Math,         // Mathematics
  Graphics,     // TCanvas
  StrUtils,     // Ansi Strings

  MapImage,         // Map Image
  MapItemType,      // Map Item Type
  MapItemTypeList,  // List with all Map Item Types
  LeafUnit,         // Save/Loading
  GenUtils,         // My utils
  GeomUtils,        // Geometrical utilities
  GeomCurve;        // Cubic Curve

const
  // Save/Load

  LeafItemName      = 0;
  LeafItemType      = 1;
  LeafItemPointList = 2;
  LeafItemWidth     = 3;
  LeafItemDesc      = 4;

  ptWdtDefault  = 30; // Default wdt of a point or trap for a line/area

  msMoveSize    = 8;  // Size of the middle move square
  msSclSize     = 4;  // Size of the corner scale squares
  msRotSize     = 4;  // Size of the rotation square
  msRotLineSize = 50; // Length of the rotation line
  msPointSize   = 2;  // Size of the edit points on lines and areas

  DelmiterChar = ';'; // Delimiter char used for saving map items

  CurveOutPointsMax = 5000; // Max number of curve points

type

  TNiftyRec = record
    Increment     : integer; // Increment at each redraw
    LineSpeed     : integer; // Speed in mm/s to draw the line
    LineCircleOn  : boolean; // Draw a circle along the line
    LineCircleWdt : integer; // Width of circle
    LineOn        : boolean; // Draw a line
    LineMapCenOn  : boolean; // Center Map
    PointWdt      : integer; // Width of circle to draw
    PointTurns    : integer; // Number of turns to draw
    AreaRotateOn  : boolean; // Rotate area
    AreaRandOn    : boolean; // Randomize rotation
    AreaScaleInOn : boolean; // Scale from o to 1.0
    AreaPolyOn    : boolean; // Draw area with polyline
    Font          : TFont;
  end;

  // Selection types when user moves over an Item in Edit move

  TMapItemSelection = (msNone,   // Nothing selected
                       msMove,   // Middle Move box selected
                       msSclLU,  // Scale Left Up selected
                       msSclLD,  // Scale Left Down selected
                       msSclRU,  // Scale Right Up selected
                       msSclRD,  // Scale Right Down selected
                       msRot,    // Rotation selected
                       msPoint,  // Existing Point selected
                       msLine);  // Existing Line selected

  // The Map Item class it self

  TMapItem = class(TObject)
  private
    ItemName      : string;          // Name of the Map Point
    ItemType      : TMapItemType;    // Type of map point (extended one)
    ItemPoints    : TPointArray; // Array of TPoints (area, line, point)
    ItemWdt       : integer;         // Width/trap of the Map Point (search)
    ItemDesc      : string;          // Description of the Map Point
    ItemDirty     : boolean;         // True id changed from loaded

    //-------------  constructors / destructors and such -----------------------

    constructor Create; overload; // Not used outside

  public

    constructor Create(TypeList : TMapItemTypeList;
                       Name     : string;
                       TypeName : string;
                       TypeGeom : TMapItemGeometry;
                       xyPairs  : string; // String with xy pairs x,y,x,y...
                       Wdt      : integer;
                       Desc     : string); overload;

    constructor Create (TypeList : TMapItemTypeList;fileStr : string); overload;

    // Note a destructor is not needed, since all attributes in this class
    // are destroyed automagically (they are managed by delphi, or by me)

    //--------------------------- Read only functions --------------------------

    function InqName       : string; // Return name of point
    function InqType       : TMapItemType;
    function InqTypeName   : string;  // Return name of the point type (City...)

    function InqGeometry   : TMapItemGeometry; // Return internal item type (line, point, area)
    function InqGeometryName : string; // Return internal item type as a name
    function InqVisible      : boolean;
    function InqPointLen   : integer; // Return number of coordinates
    function InqPos (index : integer) : TPoint; // Return xy-coord of index
    function InqMidPoint   : TPoint;    // Return the middle of the object
    function InqWdt        : integer;  // Return the Wdt/trap of item

    function InqDesc       : string;   // Return the description

    function InqAtPos (pMap : TPoint) : boolean; // Calc if point is within XY
    function InqAtPosSelection (pMap : TPoint; scl : real; var index : integer): TMapItemSelection;
    function InqAtPoint (pMap : TPoint) : integer;
    function InqAtLine  (pMap : TPoint) : integer;

    function InqDirty : boolean;  // Return if object is dirty/updated

    // Saving and loading an item

    function SaveToFile   (var F : TextFile) : boolean; // Save to file
    function LoadFromFile (var F : TextFile; pMitl : TMapItemTypeList) : boolean;

    //--- Draw functions -------------------------------------------------------

    function InqExt : TRect; // Return the extent of the item

    procedure Draw (pMap : TMapImage;        // Pointer to Map Image
                    bPnt : boolean = false;  // Draw points at X/Y coordinates
                    bXor : boolean = false;  // Draw using XOR
                    pWdt : integer = 3);     // Draw wdt of lines

    function DrawNifty
      (pMap : TMapImage; bFirst : boolean;
       rand, first, this, last : integer;
       Nifty : TNiftyRec) : TPoint;

    // Move the item

    procedure Move   (pDist : TPoint); overload ;

    // Move a point (index) in the item

    procedure Move   (index : integer; pMap : TPoint); overload ;

    // Scale item around a mid point with proportional scale

    procedure Scale  (pMid : TPoint; scl : real); overload ;

    // Scale item around a mid point with different X / Y scale

    procedure Scale  (pMid : TPoint; sclX,sclY : real); overload;

    // Rotate around a mid point

    procedure Rotate (pMid : TPoint; ang : real);

    //--- Update functions -----------------------------------------------------

    procedure SetName (Name : string); // Set new Name
    procedure SetDesc (desc : string); // Set new description
    procedure SetType (itl : TMapItemTypeList; it : TMapItemType);    // Set new item type

    procedure AddPoint (X,Y : integer); overload; // Add a new coord to point
    procedure AddPoint (p : TPoint); overload;    // Same
    procedure AddPoint (index:integer ; p : TPoint) ; overload; // Add at index

    procedure DelPoint (index:integer); // Delete at index

    procedure SetPoint (index: integer ; p : TPoint);

    procedure SetDirty (d : boolean); // Set the dirty flag

    function  Copy : TMapItem; // Copy item and give a pointer to new item
    procedure CopyProp (cItem : TMapItem); // Copy all the properties from item

    function InqLength : integer;
    function InqPosAtLength (len : integer) : TPoint;

  private

    procedure AddPointsFromStr (xyPairs : string);
    procedure AddPointInternal (x,y :integer);

    function DrawNiftyPoint(pMap : TMapImage; bFirst : boolean;
      rand, first, this, last : integer; Nifty : TNiftyRec) : TPoint;
    function DrawNiftyLine (pMap : TMapImage; bFirst : boolean;
      rand, first, this, last : integer; Nifty : TNiftyRec) : TPoint;
    function DrawNiftyCurve(pMap : TMapImage; bFirst : boolean;
      rand, first, this, last : integer; Nifty : TNiftyRec) : TPoint;
    function DrawNiftyArea (pMap : TMapImage; bFirst : boolean;
      rand, first, this, last : integer; Nifty : TNiftyRec) : TPoint;
    function DrawNiftyArea2(pMap : TMapImage; bFirst : boolean;
      rand, first, this, last : integer; Nifty : TNiftyRec) : TPoint;

end; // End of TMapItem

implementation

var
  LastPos   : TPoint;
  LastIndex : Integer;
  LastTurn  : integer;
  LastScale : real;
  LastAng   : real;
  LastAngPos : boolean = true;

  // Cubic Curve

  CurveOutPoints : Array [0..(CurveOutPointsMax+2)] of TPoint;
  CurveOutPointsLen : integer;

//------------------------------------------------------------------------------
// Save / Load Item Types
//

  LeafsItem : TLeafRecordArray;

//------------------------------------------------------------------------------
//                              Constructors
//------------------------------------------------------------------------------
// Create the Map Data Object without data
//
constructor TMapItem.Create();
begin
  inherited Create;

  // Initialize all data in the object

  ItemName      := '';            // No Name
  ItemType      := nil;           // Unknown type
  ItemWdt       := ptWdtDefault;  // Default wdt/trap
  ItemDesc      := '';            // No description
  ItemDirty     := false;         // Not changed yet
end;
//------------------------------------------------------------------------------
// Create the Map Data Object with data
//
constructor TMapItem.Create (
    TypeList : TMapItemTypeList; // Pointer to list of all types
    Name     : string;           // Name of the Map item
    TypeName : string;           // Map Item Type Name
    TypeGeom : TMapItemGeometry; // Geometry type
    xyPairs  : string;           // String with coordinates
    Wdt      : integer;          // Width of the item point
    Desc     : string);          // Sescription of item
begin
  Create(); // Use the default settings

  // Initialize all data in the object

  ItemName := Name;           // Set the name of the item
  ItemType := TypeList.Add(TypeName, TypeGeom);  // Create with the right code
  AddPointsFromStr (xyPairs); // Add all points
  ItemWdt  := Wdt;            // Set width / trap
  ItemDesc := Desc;           // Set description

  ItemDirty := false;
end;
//------------------------------------------------------------------------------
// Create the Map Data Object with all data contained in a string
//
constructor TMapItem.Create(TypeList : TMapItemTypeList; fileStr : string);
var
  index : integer;
  part  : integer;
  sTmp  : string;
  sIt   : string;
  sGt   : string;
  i     : integer;
  gt    : TMapItemGeometry;
begin
  Create();

  // Start walking the string

  part  := 0;
  index := 1;
  while (index <= Length(fileStr)) do
    begin

      // Get next string

      sTmp := StringNxtDel(index, fileStr, DelmiterChar);
      case part of
        0: ItemName := sTmp;  // Name of the Map point
        1:
          begin
            // Look for ' (comma)

            for i := 1 to length(sTmp) do
              if sTmp[i] = ',' then
                begin
                  sIt := AnsiLeftStr(sTmp,i-1);

                  sGt := AnsiRightStr(sTmp,length(sTmp) - i);

                  case sGt[1] of
                    'L' : gt := gtLine;
                    'C' : gt := gtCubic;
                    'A' : gt := gtArea;
                  else
                    gt := gtPoint;
                  end;

                  ItemType := TypeList.Add (sIt, gt);
                end;
          end;
        2: AddPointsFromStr (sTmp); // Points XY-pairs in a string
        3: ItemWdt := Max(StringToInt(sTmp),10); // Point width / trap
        4: ItemDesc := sTmp; // Description
      end;

      // Step to next line part

      part := part + 1;
    end;
  ItemDirty := false;
end;
//------------------------------------------------------------------------------
// Add points from a string
//
procedure TMapItem.AddPointsFromStr (xyPairs : string);
var
  i     : integer;
  x     : integer;
  y     : integer;
  sTemp : string;  // temp string with number
  xPair : boolean; // xPair means that next number is an X
begin

  // walk all xypairs string and add them to ItemPoints array
  // x1,y1,x2,y2,xN,yN is the simple syntax

  sTemp := '';
  xPair := true;
  x := 0;
  for i := 1 to length(xyPairs) do
    begin
      if xyPairs[i] = ',' then
        begin
          if xPair then
            begin
              if length(sTemp) > 0 then
                x := StrToInt(stemp)
              else
                x := 0;
              xPair := false;
            end
          else
            begin
              if length(sTemp) > 0 then
                y := StrToInt(stemp)
              else
                y := 0;

              // Add this point

              AddPointInternal (x,y);

              xPair := true;
              x := 0;
            end;
          sTemp := '';
        end
      else
        begin
          // Use numbers only

          if (xyPairs[i] >= '0') and (xyPairs[i] <= '9') then
            sTemp := sTemp + xyPairs[i];
        end;
    end;

  // Take care of any leftovers (if complete xy-pair)

  if (not xPair) and (x <> 0) and (length(sTemp) > 0) then
    begin
      // Add this point

      AddPointInternal (x, StrToInt(sTemp));
    end;
end;
//------------------------------------------------------------------------------
// Add one point in map coordinates to the dynamic array
//
procedure TMapItem.AddPointInternal (x,y : integer);
begin
  SetLength(ItemPoints, High(ItemPoints) + 2);
  ItemPoints[High(ItemPoints)].X := x;
  ItemPoints[High(ItemPoints)].Y := y;
end;
//------------------------------------------------------------------------------
//                            Read only functions
//------------------------------------------------------------------------------
// Return the name of the item
//
function TMapItem.InqName : string;
begin
  InqName := ItemName;
end;
//------------------------------------------------------------------------------
// Return the type of the item
//
function TMapItem.InqType : TMapItemType;
begin
  InqType := ItemType;
end;
//------------------------------------------------------------------------------
// Return the type name
//
function TMapItem.InqTypeName : string;
begin
  if ItemType <> nil then
    InqTypeName := ItemType.InqName()
  else
    InqTypeName := '';
end;
//------------------------------------------------------------------------------
// Return the Point Type (extended)
//
function TMapItem.InqGeometry : TMapItemGeometry;
begin
  if ItemType <> nil then
    InqGeometry := ItemType.InqGeometry()
  else
    InqGeometry := gtPoint;
end;
//------------------------------------------------------------------------------
// Return the Point Type (extended)
//
function TMapItem.InqGeometryName : string;
begin
  if ItemType <> nil then
    InqGeometryName := ItemType.InqGeometryName()
  else
    InqGeometryName := '';
end;
//------------------------------------------------------------------------------
// Return if item is visible
//
function TMapItem.InqVisible : boolean;
begin
  if ItemType <> nil then
    InqVisible := ItemType.InqVisible()
  else
    InqVisible := false;
end;
//------------------------------------------------------------------------------
// Return the number of points
//
function TMapItem.InqPointLen : integer;
begin
  InqPointLen := High(ItemPoints);
end;
//------------------------------------------------------------------------------
// Return the xy-coord of a point index
//
function TMapItem.InqPos (index : integer) : TPoint;
begin
  if (index >= 0) and (index <= High(ItemPoints)) then
    begin
      InqPos := ItemPoints[index];
    end
  else
    begin
      InqPos.X := -1;
      InqPos.Y := -1;
    end;
end;
//------------------------------------------------------------------------------
// Set the xy-coord of a point index
//
procedure TMapItem.SetPoint (index : integer; p : TPoint);
begin
  if (index >= 0) and (index <= High(ItemPoints)) then
    begin
      ItemPoints[index] := p;
      ItemDirty := true;
    end;
end;
//------------------------------------------------------------------------------
// Find the middle point X of this item
//
function TMapItem.InqMidPoint : TPoint;
begin
  InqMidPoint := InqMidOfRect(InqExt());
end;
//------------------------------------------------------------------------------
// Return Wdt/Trap
//
function TMapItem.InqWdt : integer;
begin
  InqWdt := ItemWdt;
end;
//------------------------------------------------------------------------------
// Return the description
//
function TMapItem.InqDesc : string;
begin
  if length(ItemDesc) < 1 then
    InqDesc := 'No description available'
  else
    InqDesc := ItemDesc;
end;
//------------------------------------------------------------------------------
// Find out if this item is at a specific coordinate
//
function TMapItem.InqAtPos (pMap : TPoint) : boolean;
begin
  InqAtPos := false; // So far no good

  // Any use to search at all ?

  if High(ItemPoints) >= 0 then
    begin

      // Depending on point type (point, line, area) do

      case ItemType.InqGeometry() of
        gtLine: InqAtPos := IsPtOnPolyLine
                        (ItemPoints, High(ItemPoints)+1, pMap.X, pMap.Y,round(ItemWdt/2));
        gtCubic:
          begin
            // Make the draw polyline buffer

            DrawCubic (ItemPoints, High(ItemPoints) + 1, 4,
                CurveOutPointsMax, CurveOutPoints, CurveOutPointsLen);

            // test the result

            InqAtPos := IsPtOnPolyLine
                        (CurveOutPoints, CurveOutPointsLen,
                          pMap.X, pMap.Y,round(ItemWdt/2));
          end;
        gtArea: InqAtPos := IsPtInArea (ItemPoints, pMap.X, pMap.Y);
      else
        InqAtPos := IsPntOnPnt(ItemPoints[0].X, ItemPoints[0].Y,
                                      pMap.X, pMap.Y, round(ItemWdt/2));
      end;
    end;
end;
//------------------------------------------------------------------------------
// Return if the object has changed
//
function TMapItem.InqDirty : boolean;
begin
  InqDirty := ItemDirty;
end;
//------------------------------------------------------------------------------
// Draw this item
//
procedure TMapItem.Draw (pMap : TMapImage;
    bPnt : boolean = false; bXor : boolean = false; pWdt : integer = 3);
var
  q : integer;

  fm : TBrushStyle;
  pc : TColor;
  pw : integer;
  pm : TPenMode;

begin
  if pMap = nil then exit;
  
  // Set no fill, red line 3 pixel wide

  fm := pMap.DrawSetFillMode(bsClear);
  pc := pMap.DrawSetPenColor(ItemType.InqColor());
  pw := pMap.DrawSetPenWidth(pWdt);
  if bXor then
    pm := pMap.DrawSetPenMode (pmNotXor)
  else
    pm := pMap.DrawSetPenMode (pmCopy);

  // Draw depending on type

  if (ItemType.InqGeometry() = gtCubic) then
    begin
      CurveOutPointsLen := 0;

      if (pWdt = 3) then
        q := 12
      else
        q := 6;

      if bPnt then
        pMap.DrawPline(ItemPoints,0,High(ItemPoints), false, true);

      DrawCubic (ItemPoints, High(ItemPoints) + 1, q,
          CurveOutPointsMax, CurveOutPoints, CurveOutPointsLen);

      // Draw this result

      pMap.DrawPline(CurveOutPoints,0,CurveOutPointsLen-1, true, false);

    end
  else if (ItemType.InqGeometry() = gtLine) then
    begin
      // Draw an polyline

      pMap.DrawPline(ItemPoints,0,High(ItemPoints), true, bPnt);

    end
  else if (ItemType.InqGeometry() = gtArea) then
    begin
      // Draw an polygone

      pMap.DrawArea(ItemPoints,High(ItemPoints), true, bPnt);

    end
  else if High(ItemPoints) >= 0 then
    begin
      // Draw a circle at the first point in item

      pMap.DrawCircle(ItemPoints[0], round(ItemWdt / 2));
    end;

  pMap.DrawSetFillMode(fm);
  pMap.DrawSetPenColor(pc);
  pMap.DrawSetPenWidth(pw);
  pMap.DrawSetPenMode (pm);
end;
//------------------------------------------------------------------------------
// Draw this item
//
function TMapItem.DrawNifty
  (pMap : TMapImage; bFirst : boolean;rand, first, this, last : integer;
    Nifty : TNiftyRec):TPoint;
var
  fm : TBrushStyle;
  pc : TColor;
  pw : integer;
  pm : TPenMode;

begin
  if pMap = nil then exit;
  
  // Set no fill, red line 3 pixel wide and xor

  fm := pMap.DrawSetFillMode(bsClear);
  pc := pMap.DrawSetPenColor(ItemType.InqColor());
  pw := pMap.DrawSetPenWidth(3);
  pm := pMap.DrawSetPenMode (pmNotXor);

  // Draw depending on type

  case ItemType.InqGeometry() of
    gtPoint : DrawNifty := DrawNiftyPoint(pMap, bFirst, rand, first, this, last,Nifty);
    gtArea  : DrawNifty := DrawNiftyArea (pMap, bFirst, rand, first, this, last,Nifty);
    gtLine  : DrawNifty := DrawNiftyLine (pMap, bFirst, rand, first, this, last,Nifty);
    gtCubic : DrawNifty := DrawNiftyCurve(pMap, bFirst, rand, first, this, last,Nifty);
  end;

  pMap.DrawSetFillMode(fm);
  pMap.DrawSetPenColor(pc);
  pMap.DrawSetPenWidth(pw);
  pMap.DrawSetPenMode (pm);
end;
//------------------------------------------------------------------------------
// Draw this gtPoint item in a really nifty way
//
function TMapItem.DrawNiftyPoint (pMap : TMapImage; bFirst : boolean;
       rand, first, this, last : integer;
       Nifty : TNiftyRec) : TPoint;
var
  step : real;
begin
  // Draw away last circle, if not first

  if bFirst and (this > first) then
    pMap.DrawCircle(ItemPoints[0], round(LastScale));


  // Step =  Increment * Turns * Nifty.PointWdt - ItemWdt / NoIteration

  step := (Nifty.PointTurns *  (Nifty.PointWdt - (ItemWdt/2))  /
          (last - first)) * Nifty.Increment;

  // Initiate

  if this = first then
    begin
      LastScale := Nifty.PointWdt;
      LastTurn := 0; // Going In
    end;

  // Test for turning

  if LastScale <= (ItemWdt Div 2) then
    LastTurn := 1
  else if LastScale >= Nifty.PointWdt then
    LastTurn := 0;

  // Draw  

  if LastTurn = 0 then
    begin
      LastScale := LastScale - step;
      pMap.DrawCircle(ItemPoints[0], round(LastScale));
    end
  else if this < last then
    begin
      LastScale := LastScale + step;
      pMap.DrawCircle(ItemPoints[0], round(LastScale));
    end;

  DrawNiftyPoint := ItemPoints[0];
end;
//------------------------------------------------------------------------------
// Draw this gtLine item in a really nifty way
//
function TMapItem.DrawNiftyLine (pMap : TMapImage; bFirst : boolean;
        rand, first, this, last : integer;
        Nifty : TNiftyRec) : TPoint;
var
  LineLength : integer;
  EachStep   : real;
  ThisTime   : integer;
begin
  // Draw iter 0 - 100

  // Find the length of the line

  LineLength := InqLength();

  // Divide this into 100 small steps

  EachStep := LineLength / last;

  // After the first iteration, draw away last pos first

  if bFirst and (this > first) then
    begin
      if Nifty.LineOn then pMap.DrawPline(ItemPoints,0,LastIndex, true, false);
      if Nifty.LineCircleOn then pMap.DrawPoint(LastPos, Nifty.LineCircleWdt);
    end;

  // Dont draw to on the last iteration

  if this < last then
    begin
      ThisTime := round(this * EachStep);
      LastPos := InqPosFromStart(ItemPoints, High(ItemPoints)+1, ThisTime, LastIndex);
      if Nifty.LineOn then pMap.DrawPline(ItemPoints,0,LastIndex, true, false);
      if Nifty.LineCircleOn then pMap.DrawPoint(LastPos, Nifty.LineCircleWdt);
    end;
  DrawNiftyLine := LastPos;
end;
//------------------------------------------------------------------------------
// Draw this gtLine item in a really nifty way
//
function TMapItem.DrawNiftyCurve (pMap : TMapImage; bFirst : boolean;
       rand, first, this, last : integer;
       Nifty : TNiftyRec) : TPoint;
var
  LineLength : integer;
  EachStep   : real;
  ThisTime   : integer;
  i,pw          : integer;
  d : real;
begin
  // Draw iter 0 - 100
                      
  pw := pMap.DrawSetPenWidth(Nifty.LineCircleWdt);

  // Convert curve to pline buffer

  CurveOutPointsLen := 0;

  DrawCubic (ItemPoints, High(ItemPoints) + 1, 3,
          CurveOutPointsMax, CurveOutPoints, CurveOutPointsLen);

  // Find the length of the line

  // Walk all points in item

  d := 0;
  for i := 0 to CurveOutPointsLen - 2 do
    d := d + InqDist(CurveOutPoints[i],CurveOutPoints[i+1]);

  LineLength := round(d);

  // Divide this into 100 small steps

  EachStep := LineLength / last;

  // After the first iteration, draw away last pos first

  if bFirst and (this > first) then
    begin
      if Nifty.LineOn then pMap.DrawPline(CurveOutPoints,0,LastIndex, true, false);
      if Nifty.LineCircleOn then pMap.DrawPoint(LastPos, Nifty.LineCircleWdt);
    end;

  // Dont draw to on the last iteration

  if this < last then
    begin
      ThisTime := round(this * EachStep);
      LastPos := InqPosFromStart(CurveOutPoints, CurveOutPointsLen, ThisTime, LastIndex);
      if Nifty.LineOn then pMap.DrawPline(CurveOutPoints,0,LastIndex, true, false);
      if Nifty.LineCircleOn then pMap.DrawPoint(LastPos, Nifty.LineCircleWdt);
    end;
  DrawNiftyCurve := LastPos;

  
  pMap.DrawSetPenWidth(pw);
end;
//------------------------------------------------------------------------------
// Draw this gtLine item in a really nifty way
//
function TMapItem.DrawNiftyArea (pMap : TMapImage; bFirst : boolean;
       rand, first, this, last : integer;
       Nifty : TNiftyRec) : TPoint;
var
  LineLength : integer;
  EachStep   : real;
  ThisTime   : integer;
begin
  if Nifty.AreaPolyOn then
    begin
      DrawNiftyArea := DrawNiftyArea2 (pMap, bFirst, rand, first, this, last,Nifty);
      Exit;
    end;

  // Draw iter 0 - 100

  // Find the length of the line

  LineLength := InqLength();

  // Divide this into 100 small steps

  EachStep := LineLength / last;

  // After the first iteration, draw away last pos first

  if bFirst and (this > first) then
    pMap.DrawPoint(LastPos, Nifty.LineCircleWdt);

  // Dont draw to on the last iteration

  if this < last then
    begin
      ThisTime := round(this * EachStep);
      LastPos := InqPosAtLength(ThisTime);
      pMap.DrawPoint(LastPos, Nifty.LineCircleWdt);
    end;
  DrawNiftyArea := LastPos;
end;
//------------------------------------------------------------------------------
// Draw this gtLine item in a really nifty way
//
function TMapItem.DrawNiftyArea2 (pMap : TMapImage; bFirst : boolean;
       rand, first, this, last : integer;
       Nifty : TNiftyRec) : TPoint;
var
  ScaleStep : real;
  ScaleAng  : real;
  pnt       : TPoint;
begin
  pnt.X := 0;
  pnt.Y := 0;

  // Draw this iteration from first to last

  if this = first then
    begin
      LastAng := 0.0;
      if Nifty.AreaScaleInOn then
        LastScale := 0.0
      else
        LastScale := 2.0
    end;

  // Divide this into 80 small steps

  ScaleStep := 1.0 / last;

  // Rand will give a number between 0 and 100

  if Nifty.AreaRotateOn then
    if Nifty.AreaRandOn then
      ScaleAng  := (rand div 12) * 2 * pi / last
    else
      ScaleAng  := 4 * pi / last
  else
    ScaleAng := 0.0;

  // After the first iteration, draw away last pos first

  if bFirst and (this > first) then
    begin
      pMap.SetMatrix(LastPos, LastScale, LastScale, LastAng, pnt);
      Draw(pMap, false, true);
      pMap.SetMatrix;
    end;

  // Dont draw to on the last iteration

  if this < last then
    begin
      if Nifty.AreaScaleInOn then
        LastScale := this * ScaleStep
      else
        LastScale := ((2 * last) - this) * ScaleStep;

      //if abs(LastAng) > 0.05 then
      //  LastAngPos := not LastAngPos;

      if Nifty.AreaRotateOn then
        if LastAngPos then
          LastAng := LastAng + ScaleAng
        else
          LastAng := LastAng - ScaleAng;

      //LastAng   := this * ScaleAng;
      
      LastPos := InqMidPoint;
      pMap.SetMatrix(LastPos, LastScale, LastScale, LastAng, pnt);
      Draw(pMap, false, true);
      pMap.SetMatrix;
    end;

  DrawNiftyArea2 := LastPos;
end;
//------------------------------------------------------------------------------
// Get the extent of the point
//
function TMapItem.InqExt : TRect;
var
  i : integer;
  fExt : TRect;
begin

  if High(ItemPoints) < 0 then
    begin
      InqExt.Left   := 0;
      InqExt.Right  := 0;
      InqExt.Top    := 0;
      InqExt.Bottom := 0;
    end
  else
    begin

      // Take the first point always

      fExt := RectIncrement(ItemPoints[0],0);

      // Take the next points if any

      if High(ItemPoints) > 0 then
        begin
          for i := 1 to High(ItemPoints) do
            begin
              fExt := RectIncrement(fExt, ItemPoints[i]);
            end
        end
      else
        begin

          // If only one point add the width / trap

          fExt := RectIncrement(fExt, round(ItemWdt/2));
        end;

      InqExt := fExt;
    end;

end;
//------------------------------------------------------------------------------
// Find length of line
//
function TMapItem.InqLength : integer;
var
  i : integer;
  d : real;
begin
  d := 0.0;

  // Walk all points in item

  for i := 0 to High(ItemPoints) - 1 do
    d := d + InqDist(ItemPoints[i],ItemPoints[i+1]);

  // If Area add the last to the first also

  if ItemType.InqGeometry() = gtArea then
    d := d + InqDist(ItemPoints[High(ItemPoints)],ItemPoints[0]);

  InqLength := round(d);
end;
//------------------------------------------------------------------------------
// Find pos along the line at length
//
function TMapItem.InqPosAtLength (len : integer) : TPoint;
var
  index : integer;
begin
  InqPosAtLength := InqPosFromStart(ItemPoints, High(ItemPoints) + 1, len, index);
end;
//------------------------------------------------------------------------------
// Move the item (all its points)
//
procedure TMapItem.Move (pDist : TPoint);
var
  i : integer;
begin
  for i := 0 to High(ItemPoints) do
    ItemPoints[i] := PntMove(ItemPoints[i],pDist);
  ItemDirty := true;
end;
//------------------------------------------------------------------------------
// Move an item (index) in the item
//
procedure TMapItem.Move (index : integer; pMap : TPoint);
begin
  if (index >= 0) and (index <= High(ItemPoints)) then
    ItemPoints[index] := pMap;
  ItemDirty := true;
end;
//------------------------------------------------------------------------------
// Scale this item. We scale from the middle of the item
//
procedure TMapItem.Scale (pMid : TPoint; scl : real);
var
  i : integer;
begin
  if High(ItemPoints) = 0 then
    begin
      //Scale the wdt instead

      ItemWdt := round(ItemWdt * scl);
    end
  else
    begin
      for i := 0 to High(ItemPoints) do
        ItemPoints[i] := PntSclOnMid (ItemPoints[i],scl,pMid);
    end;
  ItemDirty := true;
end;
//------------------------------------------------------------------------------
// Scale this item. We scale from the middle of the item
//
procedure TMapItem.Scale (pMid : TPoint; sclX,sclY : real);
var
  i : integer;
begin

  if High(ItemPoints) >= 0 then
    begin
      if (High(ItemPoints) = 0) then
        begin
          //Scale the wdt instead

          ItemWdt := round(ItemWdt * sclX);
        end
      else
        begin
          for i := 0 to High(ItemPoints) do
            ItemPoints[i] := PntSclOnMid (ItemPoints[i],sclX,sclY,pMid);
        end;
    end;
  ItemDirty := true;
end;
//------------------------------------------------------------------------------
// Rotate this item. We rotate from the middle of the item
//
procedure TMapItem.Rotate (pMid : TPoint; ang : real);
var
  i : integer;
begin

  if High(ItemPoints) >= 0 then
    begin

      for i := 0 to High(ItemPoints) do
          ItemPoints[i] := PntRotate(pMid, ItemPoints[i], ang);
    end;
  ItemDirty := true;
end;
//------------------------------------------------------------------------------
// Find if point is at a point in MapPoint
//
function TMapItem.InqAtPoint (pMap : TPoint) : integer;
var
  i : integer;
begin
  InqAtPoint := -1;

  for i := 0 to High(ItemPoints) do
    begin
      if IsPntOnPnt (pMap, ItemPoints[i], msPointSize * 3) then
        begin
          InqAtPoint := i;
          Exit;
        end;
    end;
end;
//------------------------------------------------------------------------------
// Find if point is at a line
//
function TMapItem.InqAtLine (pMap : TPoint) : integer;
var
  i : integer;
begin
  InqAtLine := -1;

  // Walk all line segments

  for i := 0 to High(ItemPoints) - 1 do
    begin
      if IsPtOnLine(ItemPoints[i].X,ItemPoints[i].Y,
                    ItemPoints[i+1].X,ItemPoints[i+1].Y,
                    pMap.X,pMap.Y, msPointSize) then
        begin
          InqAtLine := i;
          Exit;
        end;
    end;

  // test also the last to the first if point type = area

  if (ItemType.InqGeometry() = gtArea) and (High(ItemPoints) > 1) then
    if IsPtOnLine(ItemPoints[High(ItemPoints)].X,ItemPoints[High(ItemPoints)].Y,
                    ItemPoints[0].X,ItemPoints[0].Y,
                    pMap.X,pMap.Y, msPointSize) then
      InqAtLine := High(ItemPoints);
end;
//------------------------------------------------------------------------------
// Find out if the cursor is over one of the selection frame squaers
//
function TMapItem.InqAtPosSelection (pMap : TPoint;
                              scl : real; var index : integer) : TMapItemSelection;
var
  p, m : TPoint;
  r    : TRect;
  i    : integer;
  b    : boolean;
  da,db : real;
begin
  InqAtPosSelection := msNone;
  index := -1;
  da := 9999.9;

      m := InqMidPoint();

      // Test if it is one of the points itself

      if High(ItemPoints) > 0 then
        begin
          i := InqAtPoint(pMap);
          if i >= 0 then
            begin
              index := i;
              InqAtPosSelection := msPoint;
              exit;
            end;
        end;

      // Test if it is one of the lines

      if InqGeometry() = gtCubic then
        begin
          // First draw the Cubic Curve to a buffer

          DrawCubic (ItemPoints, High(ItemPoints) + 1, 4,
                CurveOutPointsMax, CurveOutPoints, CurveOutPointsLen);

          // Then test if its on line

          b := false;
          for i := 0 to CurveOutPointsLen - 2 do
            begin
              if IsPtOnLine(CurveOutPoints[i].X,CurveOutPoints[i].Y,
                    CurveOutPoints[i+1].X,CurveOutPoints[i+1].Y,
                       pMap.X,pMap.Y, msPointSize) then
                begin
                  b := true;
                  Break;
                end;
            end;

          // Use this to find the actual index in real curve

          if b then
            begin
              for i := 0 to High(ItemPoints) - 1 do
                begin
                  db := InqDistFromLine(pMap,ItemPoints[i], ItemPoints[i+1]);
                  if i = 0 then
                    begin
                      index := i;
                      da := db;
                    end
                  else
                    begin
                      if db < da then
                        begin
                          index := i;
                          da := db;
                        end;
                    end;
                end;
              InqAtPosSelection := msLine;
              exit;
            end;
        end
      else
        begin
          index := InqAtLine(pMap);
          if index >= 0 then
            begin
              InqAtPosSelection := msLine;
              exit;
            end;
        end;

      // Test the middle move first

      if IsPntInRect(pMap,RectIncrement(m,round(msMoveSize/scl))) then
        begin
          InqAtPosSelection := msMove;
          exit;
        end;

      // Test if on the scale squares

      r := InqExt();
      p.X := r.Left;
      p.Y := r.Top;
      if IsPntInRect(pMap,RectIncrement(p,round(msSclSize/scl))) then
        begin
          InqAtPosSelection := msSclLU;
          exit;
        end;

      p.X := r.Left;
      p.Y := r.Bottom;
      if IsPntInRect(pMap,RectIncrement(p,round(msSclSize/scl))) then
        begin
          InqAtPosSelection := msSclLD;
          exit;
        end;

      p.X := r.Right;
      p.Y := r.Top;
      if IsPntInRect(pMap,RectIncrement(p,round(msSclSize/scl))) then
        begin
          InqAtPosSelection := msSclRU;
          exit;
        end;

      p.X := r.Right;
      p.Y := r.Bottom;
      if IsPntInRect(pMap,RectIncrement(p,round(msSclSize/scl))) then
        begin
          InqAtPosSelection := msSclRD;
          exit;
        end;

      // Test if in the rotation box

      if (ItemType.InqGeometry() = gtArea) or
         (ItemType.InqGeometry() = gtLine) or
         (ItemType.InqGeometry() = gtCubic) then
        begin
          p.X := m.X + round(msRotLineSize/scl);
          p.Y := m.Y;
          if IsPntInRect(pMap,RectIncrement(p,round(msRotSize/scl))) then
            begin
              InqAtPosSelection := msRot;
              exit;
            end;
        end;

end;
//------------------------------------------------------------------------------
//                              Update functions
//------------------------------------------------------------------------------
// Change Name
//
procedure TMapItem.SetName (Name : string);
begin
  ItemName := Name;
  ItemDirty := true;
end;
//------------------------------------------------------------------------------
// Change description
//
procedure TMapItem.SetDesc (Desc : string);
begin
  ItemDesc := Desc;
  ItemDirty := true;
end;
//------------------------------------------------------------------------------
// Set type
//
procedure TMapItem.SetType (itl : TMapItemTypeList; it : TMapItemType);
var
  oldGt, newGt : TMapItemGeometry;
  pnt : TPoint;
  i   : integer;
begin
  if it <> nil then
    begin

      // Remember the old geometrical type

      oldGt := ItemType.InqGeometry;

      // Set the new item type

      ItemType := it;

      // Get the new geometrical type

      newGt := ItemType.InqGeometry();

      // Find out if the geometrical shape has altered

      if (oldGt = gtPoint) and
         ((newGt = gtLine) or (newGt = gtArea) or (newGt = gtCubic)) then
        begin

          //--- Change from point to line or area -------------------------

          // Get the middle (first point)

          pnt := InqPos(0);
          i := round(ItemWdt/2);

          // Build a diamond (standing square)
          //      by moving forst point and add three new point

          pnt.Y := pnt.Y - i;
          SetPoint(0,pnt);

          pnt.X := pnt.X + i;
          pnt.Y := pnt.Y + i;
          AddPoint(pnt);

          pnt.X := pnt.X - i;
          pnt.Y := pnt.Y + i;
          AddPoint(pnt);

          pnt.X := pnt.X - i;
          pnt.Y := pnt.Y - i;
          AddPoint(pnt);
        end
      else if (newGt = gtPoint) and
             ((oldGt = gtLine) or (oldGt = gtCubic) or (oldGt = gtArea)) then
        begin

          //--- Change from area or line to point -------------------------

          // Get the mid of the area or line

          pnt := InqMidPoint();

          // remove all points, but the first

          SetLength(ItemPoints,1);
          //for i := 1 to High(ItemPoints) do
          //  DelPoint(1);

          // Add the middle as new pos

          SetPoint(0,pnt);
          ItemWdt := 30;
        end;
      ItemDirty := true;
    end;
end;
//------------------------------------------------------------------------------
// Add a new point to the object
//
procedure TMapItem.AddPoint (X,Y : integer);
begin
  AddPointInternal (x,y);
  ItemDirty := true;
end;
//------------------------------------------------------------------------------
// Add a new point to the object
//
procedure TMapItem.AddPoint (p : TPoint);
begin
  AddPointInternal(p.X, p.Y);
  ItemDirty := true;
end;
//------------------------------------------------------------------------------
// Add a new point at index + 1 and move all up
//
procedure TMapItem.AddPoint (index : integer; p : TPoint);
var
  i : integer;
begin
  if (index >= 0) and (index <= High(ItemPoints) + 1) then
    begin
      //SetLength(ItemPoints, (ItemPointsLen + 1));

      // Increase the array with one

      SetLength(ItemPoints, High(ItemPoints) + 2);

      // Move all points from top up one

      for i := High(ItemPoints) downto index + 1 do
        ItemPoints[i] := ItemPoints[i-1];

      // Set the new point at index

      ItemPoints[index] := p;

      ItemDirty := true;
    end;
end;
//------------------------------------------------------------------------------
// Add a new point at index + 1 and move all up
//
procedure TMapItem.DelPoint (index : integer);
var
  i : integer;
begin

  if (index >= 0) and (index <= High(ItemPoints)) then
    begin
      // Move all points before index down on index

      for i := index to (High(ItemPoints) - 1) do
        ItemPoints[i] := ItemPoints[i+1];

      SetLength(ItemPoints, High(ItemPoints));
    end;

  ItemDirty := true;
end;
//------------------------------------------------------------------------------
// Copy a point and return the new
//
function TMapItem.Copy : TMapItem;
var
  newP : TMapItem;
  i    : integer;
begin
  Copy := nil;
  newP := TMapItem.Create();
  if newP <> nil then
    begin
      newP.ItemName := ItemName;
      newP.ItemType := ItemType;
      newP.ItemWdt  := ItemWdt;
      newP.ItemDesc := ItemDesc;

      for i := 0 to High(ItemPoints) do
        newP.AddPointInternal(ItemPoints[i].X, ItemPoints[i].Y);

      newP.ItemDirty := true;
      Copy := newP;
    end;
end;
//------------------------------------------------------------------------------
// Copy a point and return the new
//
procedure TMapItem.CopyProp (cItem : TMapItem);
var
  i    : integer;
begin
  if cItem <> nil then
    begin
      ItemName := cItem.ItemName;
      ItemType := cItem.ItemType;
      ItemWdt  := cItem.ItemWdt;
      ItemDesc := cItem.ItemDesc;

      // Remove any old points

      SetLength(ItemPoints,0);

      // Add the new points

      for i := 0 to High(cItem.ItemPoints) do
        AddPointInternal(cItem.ItemPoints[i].X, cItem.ItemPoints[i].Y);

      ItemDirty := true;
    end;
end;
//------------------------------------------------------------------------------
// Set the dirty flag
//
procedure TMapItem.SetDirty (d : boolean);
begin
  ItemDirty := d;
end;
//------------------------------------------------------------------------------
// Return the string representing an item (used for saving)
//
function TMapItem.SaveToFile (var f : TextFile) : boolean;
begin
  SaveToFile := true;

  { Save semantics


    <itemname='string'>
    <itemtypename='string'>
    <itemposlist=x0,y0,...xN,yN>
    <itemwidth=w>
    <itemdesc='string'>

  }

  WriteLn(F, '<' + LeafGetName(LeafsItem,LeafItemName) + '=' +
                   LeafStrToStr (ItemName) +
             '>');

  WriteLn(F, '<' + LeafGetName(LeafsItem,LeafItemType) + '=' +
                   LeafStrToStr (ItemType.InqName()) +
             '>');

  WriteLn(F, '<' + LeafGetName(LeafsItem,LeafItemPointList) + '=' +
                   LeafPointListToStr (ItemPoints) +
             '>');

  WriteLn(F, '<' + LeafGetName(LeafsItem,LeafItemWidth) + '=' +
                   LeafIntToStr (ItemWdt) +
             '>');

  WriteLn(F, '<' + LeafGetName(LeafsItem,LeafItemDesc) + '=' +
                   LeafStrToStr (ItemDesc) +
             '>');

  ItemDirty := false;

end;
//------------------------------------------------------------------------------
// Load this item from to file
//
function TMapItem.LoadFromFile (var F : TextFIle; pMitl : TMapItemTypeList) : boolean;
var
  sBuf : string;
  id   : integer;
begin
  LoadFromFile := false;

  while not Eof(F) do
    begin
      // Get the first object

      Readln(F, sBuf); // Syntax : <objectname=

      id := LeafGetId(LeafsItem, LeafGetObjectName(sBuf));
      case id of
        LeafObjectAtEnd  : break;
        LeafItemName      : ItemName := LeafGetValueStr(sBuf);
        LeafItemType      : ItemType := pMitl.GetItemType(LeafGetValueStr(sBuf));
        LeafItemPointList : LeafPointListFromStr (sBuf, ItemPoints);
        LeafItemWidth     : ItemWdt := LeafIntFromStr(LeafGetValueStr(sBuf));
        LeafItemDesc      : ItemDesc := LeafGetValueStr(sBuf);
      else
        // Unknown object, skip it
        LeafSkipObject(F);
      end;
    end;
end;


initialization

// Add all necessary leafs for this object

  LeafAdd (LeafsItem, LeafItemName,      'ItemName',    atString);
  LeafAdd (LeafsItem, LeafItemType,      'ItemType',    atString);
  LeafAdd (LeafsItem, LeafItemPointList, 'ItemPoints',  atPointList);
  LeafAdd (LeafsItem, LeafItemWidth,     'ItemWidth',   atInteger);
  LeafAdd (LeafsItem, LeafItemDesc,      'ItemDesc',    atString);

//------------------------------------------------------------------------------
end.

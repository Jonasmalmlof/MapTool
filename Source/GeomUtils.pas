//------------------------------------------------------------------------------
//
// General geometric functions that not is part of an object
//
// Cre 2004-02-17 Pma
//
//------------------------------------------------------------------------------
unit GeomUtils;

interface

uses
  SysUtils,     // String conversions
  Types,        // TPoint
  Graphics,     // TCanvas
  Math;         // Mathematics

const
  
  msMoveSize    = 8;  // Size of the middle move square
  msSclSize     = 4;  // Size of the corner scale squares
  msRotSize     = 4;  // Size of the rotation square
  msRotLineSize = 50; // Length of the rotation line
  msPointSize   = 2;  // Size of the edit points on lines and areas

  type TPointArray = Array of TPoint;

  type TMatrix = array[1..3, 1..3] of Real;

  // Set of directions used when scrolling and such

  type TDirections = Set of (drLeft, drDown, drUp, drRight);

  // Calculates if a point (XC,YC) is on a given line (XA,YA to XB, YB)

  function IsPtOnLine(XA,YA,XB,YB,XC,YC, Tolerance : integer): boolean;

  // Calculates if a point (X,Y) is on a given polyline (array TPoint)

  function IsPtOnPolyLine
         (const ap : array of TPoint; const apLen : integer;
          const X,Y,D : integer): boolean;

  function InqPosFromStart (const pBuf : Array of TPoint;
                const pLen, upTo : integer; var index : integer) : TPoint;

  // Calculate if a point (XA,YA) is on a given point (XB,YB)

  function IsPntOnPnt (XA,YA,XB,YB,D : integer) : boolean; overload ;
  function IsPntOnPnt (pa,pb : TPoint; D : integer) : boolean; overload ;

  // Calculate if a point (X,Y) is inside a given area(array TPoint)

  function IsPtInArea (ap : array of TPoint; X, Y : integer) : boolean;

  function IsPntInRect (p : TPoint; r : TRect) : boolean;

  function InqDist (p1,p2 : TPoint) : real;
  function InqDistFromLine (const pos, spos, epos : TPoint) : real;
  FUNCTION InqPosOnLine (const pos, spos, epos : TPoint): TPoint;

  function InqAng (pm,pe : TPoint) : real;

  function InqMidOfRect (r : TRect) : TPoint;

  // Rectangle functions

  function RectIncrement (r : TRect; inc : integer) : TRect; overload;
  function RectIncrement (p : TPoint; inc : integer) : TRect; overload;
  function RectIncrement (X,Y : integer; inc : integer) : TRect; overload;
  function RectIncrement (r : TRect; p : TPoint) : TRect; overload;

  function InqRectSize (r : TRect) : integer;

  function RectMove (r : TRect; p   : TPoint)  : TRect; overload ;
  function RectMove (r : TRect; X,Y : integer) : TRect; overload ;
  function RectScale (r : TRect; scl : real)  : TRect;
  function RectScaleAndMove (r : TRect; scl : real; dist : TPoint)  : TRect; overload;
  function RectScaleAndMove (r : TRect; scl : real; dX, dY : integer) : TRect; overload;

  function RectToStr (r : TRect) : string;

  // Point functions

  function PntToStr (p    : TPoint) : string; overload ;
  function PntToStr (X, Y : integer): string; overload ;

  function PntRotate (m, p : TPoint; ang : real) : TPoint;

  function PntMove (p,d : TPoint) : TPoint;
  function PntSubMove (p,d : TPoint) : TPoint;
  function PntSclAndMove (p : TPoint; scl : real; d: TPoint) : TPoint; overload ;
  function PntSclAndMove (p : TPoint; scl : real;  dX, dY : integer) : TPoint; overload;
  function PntSclAndMove (p : TPoint; sX,sY : real; d: TPoint) : TPoint; overload ;
  function PntSclAndMove (p : TPoint; sX,sY : real;  dX, dY : integer) : TPoint; overload;

  function PntSclOnMid (pnt : TPoint; scl : real; mid: TPoint) : TPoint; overload;
  function PntSclOnMid (pnt : TPoint; sclX,sclY : real; mid: TPoint) : TPoint; overload;

  // Matrix functions

  function  MatrixIdent : TMatrix;
  function  MatrixMlt   (mA, mB : TMatrix) : TMatrix;

  function  MatrixTran (pnt    : TPoint) : TMatrix;
  function  MatrixScl  (scl    : real)   : TMatrix; overload ;
  function  MatrixScl  (sX, sY : real)   : TMatrix; overload ;
  function  MatrixRot  (ang    : real)   : TMatrix;

  function  MatrixInqPos  (m : TMatrix ; p : TPoint)  : TPoint;
  function  MatrixInqSize (m : TMatrix ; s : integer) : integer;
implementation


//------------------------------------------------------------------------------
// Calculates if a point (XC,YC) is on a given line (XA,YA to XB, YB)
// S*L = Distance of point from line.
//
function IsPtOnLine(XA,YA,XB,YB,XC,YC, Tolerance : integer): Boolean;
var
 L,R,S: Double;
begin
  Result := False;
  L := SQRT(((XB-XA)*(XB-XA)+(YB-YA)*(YB-YA)));
  if l <> 0 then
    begin
      R := ((YA-YC)*(YA-YB)-(XA-XC)*(XB-XA))/(L*L);
      S := ((YA-YC)*(XB-XA)-(XA-XC)*(YB-YA))/(L*L);
      if (r > 0) and (r < 1) then
        if Abs(S * L) <= Tolerance then
          Result := True;  //s*l=distance
    end;
end;
//------------------------------------------------------------------------------
// Calculates if a point (X,Y) is on a given polyline (array TPoint)
// Uses IsPtOnLine
//
function IsPtOnPolyLine
    (const ap : array of TPoint; const apLen : integer;
     const X,Y,D : integer): boolean;
var
 i : integer;
 f : boolean;
begin
  i := 0;
  f := false;
  while (i < apLen - 1) and (not f) do
    begin
      f := IsPtOnLine(ap[i].X,ap[i].Y, ap[i+1].X,ap[i+1].Y, X,Y, D);
      i := i + 1;
    end;
  IsPtOnPolyLine := f;
end;
//------------------------------------------------------------------------------
// Calculate if a point (XA,YA) is on a given point (XB,YB)
// within the Distant D  (rect, not round)
//
function IsPntOnPnt (XA,YA,XB,YB, D : integer) : boolean;
begin
  IsPntOnPnt := ((XA + D) > XB) and
              ((XA - D) < XB) and
              ((YA + D) > YB) and
              ((YA - D) < YB);
end;
//------------------------------------------------------------------------------
// Calculate if a point (XA,YA) is on a given point (XB,YB)
// within the Distant D  (rect, not round)
//
function IsPntOnPnt (pa,pb : TPoint; D : integer) : boolean;
begin
  IsPntOnPnt := ((pa.X + D) > pb.X) and
              ((pa.X - D) < pb.X) and
              ((pa.Y + D) > pb.Y) and
              ((pa.Y - D) < pb.Y);
end;
//------------------------------------------------------------------------------
// Calculate if a point (X,Y) is inside a given area
// Area is an array of TPoint with length=L
//
function IsPtInArea (ap : array of TPoint; X, Y : integer) : boolean;
var
  i,j,c : integer;
begin
  IsPtInArea := false; // So far no good

  c:=0;
  j := High(ap);
  for i := 0 to j do
    begin
      if ((((ap[i].Y <= Y) and (Y < ap[j].Y)) or
           ((ap[j].Y <= Y) and (Y < ap[i].Y))) and
            (X < (ap[j].X - ap[i].X) * (Y - ap[i].Y) /
            (ap[j].Y - ap[i].Y) + ap[i].X)) then
        if c = 0 then
          c := 1
        else
          c := 0;

        j := i;
    end;

  if c <> 0 then
    IsPtInArea := true;
end;
//------------------------------------------------------------------------------
// Calculate a new rect by adding increment
//
function RectIncrement (r : TRect; inc : integer) : TRect;
begin
  RectIncrement.Left := r.Left - inc;
  RectIncrement.Right := r.Right + inc;
  RectIncrement.Top := r.Top - inc;
  RectIncrement.Bottom := r.Bottom + inc;
end;
//------------------------------------------------------------------------------
// Expand a point to a swuare
//
function RectIncrement (p : TPoint; inc : integer) : TRect;
begin
  RectIncrement.Left := p.X - inc;
  RectIncrement.Right := p.X + inc;
  RectIncrement.Top := p.Y - inc;
  RectIncrement.Bottom := p.Y + inc;
end;
//------------------------------------------------------------------------------
// Expand a point to a rectangle, i.e. ad size (inc)
//
function RectIncrement (X,Y : integer; inc : integer) : TRect;
begin
  RectIncrement.Left := X - inc;
  RectIncrement.Right := X + inc;
  RectIncrement.Top := Y - inc;
  RectIncrement.Bottom := Y + inc;
end;
//------------------------------------------------------------------------------
// Expand a rect to include a point
//
function RectIncrement (r : TRect ; p : TPoint) : TRect;
begin
  RectIncrement.Left   := Min(r.Left,  p.X);
  RectIncrement.Right  := Max(r.Right, p.X);
  RectIncrement.Top    := Min(r.Top,   p.Y);
  RectIncrement.Bottom := Max(r.Bottom,p.Y);
end;
//------------------------------------------------------------------------------
// Move a Rect
//
function RectMove (r : TRect; p : TPoint) : TRect;
begin
  RectMove.Left   := r.Left   + p.X;
  RectMove.Right  := r.Right  + p.X;
  RectMove.Top    := r.Top    + p.Y;
  RectMove.Bottom := r.Bottom + p.Y;
end;
//------------------------------------------------------------------------------
// Move a Rect
//
function RectMove (r : TRect; X,Y : integer) : TRect;
begin
  RectMove.Left   := r.Left   + X;
  RectMove.Right  := r.Right  + X;
  RectMove.Top    := r.Top    + Y;
  RectMove.Bottom := r.Bottom + Y;
end;
//------------------------------------------------------------------------------
// Scale a Rect
//
function RectScale (r : TRect; scl : real) : TRect;
begin
  RectScale.Left   := round(r.Left   * scl);
  RectScale.Right  := round(r.Right  * scl);
  RectScale.Top    := round(r.Top    * scl);
  RectScale.Bottom := round(r.Bottom * scl);
end;
//------------------------------------------------------------------------------
// Scale a Rect
//
function RectToStr (r : TRect) : string;
begin
  RectToStr := 'L: ' + IntToStr(r.Left) + ' R: ' + IntToStr(r.Right) +
               ' T: ' + IntToStr(r.Top) + ' B: ' + IntToStr(r.Bottom);
end;
//------------------------------------------------------------------------------
// First scale the rect, then move it allso
//
function RectScaleAndMove (r : TRect; scl : real; dist : TPoint) : TRect;
begin
  RectScaleAndMove := RectMove(RectScale(r,scl), dist);
end;
function RectScaleAndMove (r : TRect; scl : real; dX, dY : integer) : TRect;
begin
  RectScaleAndMove := RectMove(RectScale(r,scl), dX, dY);
end;
//------------------------------------------------------------------------------
// Calculate if a point is inside a rectangle
//
function InqRectSize (r : TRect) : integer;
begin
  InqRectSize := round(InqDist(r.TopLeft,r.BottomRight));
end;
//------------------------------------------------------------------------------
// Calculate if a point is inside a rectangle
//
function IsPntInRect (p : TPoint; r : TRect) : boolean;
begin
  IsPntInRect := (p.X >= r.Left) and
                 (p.X <= r.Right) and
                 (p.Y >= r.Top) and
                 (p.Y <= r.Bottom);
end;
//------------------------------------------------------------------------------
// Convert coordinates to string
//
function PntToStr (p : TPoint) : string;
begin
  PntToStr := IntToStr(p.X) + ', ' + IntToStr(p.Y);
end;
function PntToStr (X, Y : integer) : string;
begin
  PntToStr := IntToStr(X) + ', ' + IntToStr(Y);
end;
//------------------------------------------------------------------------------
// find distance between two points
//
function InqDist (p1,p2 : TPoint) : real;
begin
  InqDist := ABS(SQRT(ABS(abs(SQR(p2.X - p1.X)) + abs(SQR(p2.Y - p1.Y)))));
end;
//------------------------------------------------------------------------------
// find nearest distance between a point and a line
//
function InqDistFromLine (const pos, spos,epos : TPoint) : real;
VAR
  length,dx,dy : INTEGER;
  ps           : TPoint;
BEGIN

     {-- test if dist = 0 --}

     IF ((spos.x = pos.x) AND (spos.y = pos.y)) OR
        ((epos.x = pos.x) AND (epos.y = pos.y))
      THEN
       BEGIN
        InqDistFromLine := 0.0;
        exit;
       END;

     {-- test if segment length = 0 --}

     IF (spos.x = epos.x) AND (spos.y = epos.y)
      THEN
       BEGIN
        InqDistFromLine := InqDist(pos,spos);
        exit;
       END;

     {-- it was no special case, get length of line --} 
                     
     dx     := epos.x - spos.x;
     dy     := epos.y - spos.y;
     length := ROUND(SQRT(ABS(SQR(dx)+SQR(dy))));
                  
     {-- test if no length at all --}

     IF length = 0
      THEN
       BEGIN 
        InqDistFromLine := InqDist(pos,spos);
        exit;
       END;   

     {-- find pos on segment --}

     ps := InqPosOnLine (pos, spos, epos);

     {-- make sure ps is on segemnt --}

     IF ps.x > MAX(spos.x,epos.x)
     THEN ps.x := MAX(spos.x,epos.x)
     ELSE
     IF ps.x < MIN(spos.x,epos.x)
     THEN ps.x := MIN(spos.x,epos.x);

     IF ps.y > MAX(spos.y,epos.y)
     THEN ps.y := MAX(spos.y,epos.y)
     ELSE
     IF ps.y < MIN(spos.y,epos.y)
     THEN ps.y := MIN(spos.y,epos.y);

     {-- find dist from ps to pos --}

     InqDistFromLine := InqDist(ps,pos);

END;

FUNCTION InqPosOnLine
           (const pos, spos, epos : TPoint): TPoint;
VAR
  s,f : REAL;
  Dpsx,Dpsy,Desx,Desy : INTEGER;
BEGIN

     Dpsx :=  pos.x  - spos.x ;
     Dpsy :=  pos.y  - spos.y ;
     Desx := epos.x  - spos.x ;
     Desy := epos.y  - spos.y ;

     IF (Desx = 0) AND (Desy = 0)
      THEN
       BEGIN 
        InqPosOnLine := spos;
       END
      ELSE
       BEGIN
        f := (Desx * Desx + Desy * Desy) ;
         
        IF f = 0.0 THEN
          BEGIN
           InqPosOnLine := pos;
          END
         ELSE
          BEGIN

           s := (Dpsx * Desy - Dpsy * Desx) / f;

           InqPosOnLine.X := pos.x - ROUND(Desy * s);

           InqPosOnLine.y := pos.y + ROUND(Desx * s);
          END;

       END;

END;

//------------------------------------------------------------------------------
// Find pos along the line at length
//
function InqPosFromStart (const pBuf : Array of TPoint;
                const pLen, upTo : integer; var index : integer) : TPoint;
var
  i, i1, i2, l  : integer;
  thislength    : real;
  curlength     : real;
  thisiteration : real;
  dist          : TPoint;
begin
  InqPosFromStart.X := 0;
  InqPosFromStart.Y := 0;
  thisiteration := upTo * 1.0;
  index := 0;

  // We start from zero

  curlength := 0.0;

  for i := 0 to pLen - 1 do
    begin
      // Get the indexes to use

      if i < pLen - 1 then
        begin
          i1 := i;
          i2 := i + 1;
        end
      else
        begin
          i1 := i;
          i2 := 0;
        end;

      // Calc the length from this to next point

      thislength := InqDist(pBuf[i2],pBuf[i1]);

      // Has we gotten to long

      if (curlength + thislength) >= thisiteration then
        begin
          // find the distance in x/y from this to next

          dist.X := pBuf[i2].X - pBuf[i1].X;
          dist.Y := pBuf[i2].Y - pBuf[i1].Y;

          // Do we have more to go on this segment

          if (thislength <> 0.0) and (thisiteration > curlength) then
            begin
              // Calc how far in X/Y do we have to go on this segment

              InqPosFromStart.X := pBuf[i1].X + round(dist.X * (thisiteration - curlength)/ thislength);
              InqPosFromStart.Y := pBuf[i1].Y + round(dist.Y * (thisiteration - curlength)/ thislength);
            end
          else
            begin
              InqPosFromStart.X := pBuf[i1].X;
              InqPosFromStart.Y := pBuf[i1].Y;
            end;

          index := i;
          exit;
        end;

      // Calc how far we have gotten with this distance

      curlength := curlength + thislength;
    end;

end;
//------------------------------------------------------------------------------
// find angle on a line between pm (middle) and pe (external)
//
function InqAng (pm,pe : TPoint) : real;
begin
  if (pe.X <> pm.X) then
    InqAng := ArcTan2(pe.Y - pm.Y, pe.X - pm.X)
  else
    if pe.Y > pm.Y then
      InqAng := pi/2
    else if pe.Y < pm.Y then
      InqAng := -pi/2
    else
      InqAng := 0.0;
end;
//------------------------------------------------------------------------------
// Rotate a point (p) around a another point (m)
//
function PntRotate (m, p : TPoint; ang : real) : TPoint;
var
  dx, dy : integer;
begin

  dx := p.X - m.X;
  dy := p.Y - m.Y;

  PntRotate.X := m.X + round(dx * cos(ang) + dy * sin(ang));
  PntRotate.Y := m.Y + round(dy * cos(ang) - dx * sin(ang));
end;
//------------------------------------------------------------------------------
// Get the middle point of an rectangle
//
function InqMidOfRect (r : TRect) : TPoint;
begin
  InqMidOfRect.X := r.Left + round((r.Right  - r.Left)/2);
  InqMidOfRect.Y := r.Top  + round((r.Bottom - r.Top )/2);
end;
//------------------------------------------------------------------------------
// Move a point (p) a distant (d)
//
function PntMove (p, d : TPoint) : TPoint;
begin
  PntMove.X := p.X + d.X;
  PntMove.Y := p.Y + d.Y;
end;
//------------------------------------------------------------------------------
// Move a point (p) a distant (d)
//
function PntSubMove (p, d : TPoint) : TPoint;
begin
  PntSubMove.X := p.X - d.X;
  PntSubMove.Y := p.Y - d.Y;
end;
//------------------------------------------------------------------------------
// First scale (scl) a point (p) and the move it a distant (d)
//
function PntSclAndMove (p : TPoint; scl : real; d: TPoint) : TPoint;
begin
  PntSclAndMove.X := round(p.X * scl) + d.X;
  PntSclAndMove.Y := round(p.Y * scl) + d.Y;
end;
function PntSclAndMove (p : TPoint; scl : real; dX, dY: integer) : TPoint;
begin
  PntSclAndMove.X := round(p.X * scl) + dX;
  PntSclAndMove.Y := round(p.Y * scl) + dY;
end;
function PntSclAndMove (p : TPoint; sX,sY : real; d: TPoint) : TPoint;
begin
  PntSclAndMove.X := round(p.X * sX) + d.X;
  PntSclAndMove.Y := round(p.Y * sY) + d.Y;
end;
function PntSclAndMove (p : TPoint; sX,sY : real; dX, dY: integer) : TPoint;
begin
  PntSclAndMove.X := round(p.X * sX) + dX;
  PntSclAndMove.Y := round(p.Y * sY) + dY;
end;
//------------------------------------------------------------------------------
// First scale (scl) a point (p) and the move it a distant (d)
//
function PntSclOnMid (pnt : TPoint; scl : real; mid: TPoint) : TPoint;
begin
  PntSclOnMid.X := mid.X + round((pnt.X - mid.X) * scl);
  PntSclOnMid.Y := mid.Y + round((pnt.Y - mid.Y) * scl);
end;
function PntSclOnMid (pnt : TPoint; sclX,sclY : real; mid: TPoint) : TPoint;
begin
  PntSclOnMid.X := mid.X + round((pnt.X - mid.X) * sclX);
  PntSclOnMid.Y := mid.Y + round((pnt.Y - mid.Y) * sclY);
end;
//------------------------------------------------------------------------------
//                          Matrix functions
//------------------------------------------------------------------------------
// Return the identity matrix
//
function  MatrixIdent : TMatrix;
begin
  MatrixIdent[1][1] := 1.0;
  MatrixIdent[1][2] := 0.0;
  MatrixIdent[1][3] := 0.0;
  MatrixIdent[2][1] := 0.0;
  MatrixIdent[2][2] := 1.0;
  MatrixIdent[2][3] := 0.0;
  MatrixIdent[3][1] := 0.0;
  MatrixIdent[3][2] := 0.0;
  MatrixIdent[3][3] := 1.0;
end;
//------------------------------------------------------------------------------
// Return the multiplications of two matrixes (mA -> mB)
//
function  MatrixMlt   (mA, mB : TMatrix) : TMatrix;
begin
  MatrixMlt[1,1] := mB[1,1] * mA[1,1] + mB[2,1] * mA[1,2];
  MatrixMlt[2,1] := mB[1,1] * mA[2,1] + mB[2,1] * mA[2,2];
  MatrixMlt[3,1] := mB[1,1] * mA[3,1] + mB[2,1] * mA[3,2] + mB[3,1];

  MatrixMlt[1,2] := mB[1,2] * mA[1,1] + mB[2,2] * mA[1,2];

  MatrixMlt[2,2] := mB[1,2] * mA[2,1] + mB[2,2] * mA[2,2];

  MatrixMlt[3,2] := mB[1,2] * mA[3,1] + mB[2,2] * mA[3,2] + mB[3,2];
end;
//------------------------------------------------------------------------------
// Return a identity matrix with translation
//
function  MatrixTran (pnt : TPoint) : TMatrix;
begin
  MatrixTran[1][1] := 1.0;
  MatrixTran[1][2] := 0.0;
  MatrixTran[1][3] := 0.0;
  MatrixTran[2][1] := 0.0;
  MatrixTran[2][2] := 1.0;
  MatrixTran[2][3] := 0.0;
  MatrixTran[3][1] := pnt.X;
  MatrixTran[3][2] := pnt.Y;
  MatrixTran[3][3] := 1.0;
end;
//------------------------------------------------------------------------------
// Return a identity matrix with scale
//
function  MatrixScl (scl : real) : TMatrix;
begin
  MatrixScl[1][1] := scl;  {x}
  MatrixScl[1][2] := 0.0;
  MatrixScl[1][3] := 0.0;
  MatrixScl[2][1] := 0.0;
  MatrixScl[2][2] := scl;  {y}
  MatrixScl[2][3] := 0.0;
  MatrixScl[3][1] := 0.0;
  MatrixScl[3][2] := 0.0;
  MatrixScl[3][3] := 1.0;
end;
//------------------------------------------------------------------------------
// Return a identity matrix with scale
//
function  MatrixScl (sX, sY : real) : TMatrix;
begin
  MatrixScl[1][1] := sX;  {x}
  MatrixScl[1][2] := 0.0;
  MatrixScl[1][3] := 0.0;
  MatrixScl[2][1] := 0.0;
  MatrixScl[2][2] := sY;  {y}
  MatrixScl[2][3] := 0.0;
  MatrixScl[3][1] := 0.0;
  MatrixScl[3][2] := 0.0;
  MatrixScl[3][3] := 1.0;
end;
//------------------------------------------------------------------------------
// Return a identity matrix with scale
//
function  MatrixRot (ang : real) : TMatrix;
begin
  MatrixRot[1][1] :=  COS(ang);
  MatrixRot[1][2] := -SIN(ang);
  MatrixRot[1][3] :=  0.0;
  MatrixRot[2][1] :=  SIN(ang);
  MatrixRot[2][2] :=  COS(ang);
  MatrixRot[2][3] :=  0.0;
  MatrixRot[3][1] :=  0.0;
  MatrixRot[3][2] :=  0.0;
  MatrixRot[3][3] :=  1.0;
end;
//------------------------------------------------------------------------------
// Return a point through a matrix
//
function  MatrixInqPos  (m : TMatrix ; p : TPoint) : TPoint;
begin
  MatrixInqPos.x := round (p.X * m[1,1] + p.Y * m[2,1] + m[3,1]);
  MatrixInqPos.Y := round (p.X * m[1,2] + p.Y * m[2,2] + m[3,2]);
end;
//------------------------------------------------------------------------------
// Return a size through a matrix
//
function  MatrixInqSize  (m : TMatrix ; s : integer) : integer;
var
  p0,p1 : TPoint;
begin
  p0.X := 0;
  p0.Y := 0;
  p1.X := s;
  p1.Y := s;

  p0 := MatrixInqPos(m,p0);
  p1 := MatrixInqPos(m,p1);

  MatrixInqSize := round(InqDist(p1,p0)/1.414);
end;

end.

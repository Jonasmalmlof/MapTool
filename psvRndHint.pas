{*******************************************************}
{                                                       }
{          Transparent hint for Windows 2000/XP         }
{                                                       }
{ Author:                                               }
{ Serhiy Perevoznyk                                     }
{ serge_perevoznyk@hotmail.com                          }
{                                                       }
{     Use, modification and distribution is allowed     }
{without limitation, warranty, or liability of any kind.}
{                                                       }
{*******************************************************}

unit psvRndHint;

interface

uses Windows, Classes, Controls, Forms, Messages, Graphics, psvTransparentHint;

type

  TRndHint = class(TTransparentHint)
  private
    FRegion: THandle;
    procedure FreeCurrentRegion;
  public
    destructor Destroy; override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

destructor TrndHint.Destroy;
begin
  FreeCurrentRegion;
  inherited Destroy;
end;

procedure TrndHint.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not ws_Border;  
end;

procedure TrndHint.FreeCurrentRegion;
begin
  if FRegion <> 0 then begin           
    SetWindowRgn(Handle, 0, True);     
    DeleteObject(FRegion);             
    FRegion := 0;                     
  end;
end;

procedure TrndHint.ActivateHint(Rect: TRect; const AHint: string);
begin
  with Rect do
    Right := Right + Canvas.TextWidth('WWWW');  
  BoundsRect := Rect;
  FreeCurrentRegion;
  FRegion := CreateRoundRectRgn(0, 0, Width, Height, Width, Height);
  if FRegion <> 0 then
    SetWindowRgn(Handle, FRegion, True);         
  inherited ActivateHint(Rect, AHint);           
end;

procedure TrndHint.Paint;
var
  R: TRect;
begin
  R := ClientRect;                  
  Inc(R.Left, 1);                   
  Canvas.Brush.Color := clInfoBk;   
  FillRgn(Canvas.Handle, FRegion, Canvas.Brush.Handle);
  Canvas.Font.Color := clInfoText;  
  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R,
           DT_NOPREFIX or DT_WORDBREAK or DT_CENTER or DT_VCENTER);
end;

var
  OldHintClass: THintWindowClass;

function SetNewHintClass(AClass: THintWindowClass): THintWindowClass;
var
  DoShowHint: Boolean;
begin
  Result := HintWindowClass;         // return value is old hint window
  DoShowHint := Application.ShowHint;
  if DoShowHint then
    Application.ShowHint := False;   // destroy old hint window
  HintWindowClass := AClass;         // assign new hint window
  if DoShowHint then
    Application.ShowHint := True;    // create new hint window
end;

initialization
  OldHintClass := SetNewHintClass(TrndHint);

finalization
  SetNewHintClass(OldHintClass);

end.

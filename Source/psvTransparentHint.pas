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

Unit psvTransparentHint;

Interface
uses
  Windows, SysUtils, Classes, Forms, Controls, Graphics;

type
  TTransparentHint = class(THintWindow)
  private
    FTransparency : byte;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    property Transparency : byte read FTransparency write FTransparency;
    constructor Create(AOwner : TComponent); override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
  end;


Implementation

const
 WS_EX_LAYERED = $80000;
 LWA_COLORKEY = 1;
 LWA_ALPHA    = 2;

type
 TSetLayeredWindowAttributes = function (
     hwnd : HWND;         // handle to the layered window
     crKey : TColor;      // specifies the color key
     bAlpha : byte;       // value for the blend function
     dwFlags : DWORD      // action
     ): BOOL; stdcall;

procedure SetTransparentForm(AHandle : THandle; AValue : byte = 0);
var
 Info: TOSVersionInfo;
 SetLayeredWindowAttributes: TSetLayeredWindowAttributes;
begin
 //Check Windows version
 Info.dwOSVersionInfoSize := SizeOf(Info);
 GetVersionEx(Info);
 if (Info.dwPlatformId = VER_PLATFORM_WIN32_NT) and
 (Info.dwMajorVersion >= 5) then
   begin
     SetLayeredWindowAttributes := GetProcAddress(GetModulehandle(user32), 'SetLayeredWindowAttributes');
      if Assigned(SetLayeredWindowAttributes) then
       begin
        SetWindowLong(AHandle, GWL_EXSTYLE, GetWindowLong(AHandle, GWL_EXSTYLE) or WS_EX_LAYERED);
        //Make form transparent
        SetLayeredWindowAttributes(AHandle, 0, AValue, LWA_ALPHA);
      end;
   end;
end;


{ TTransparentHint }
procedure TTransparentHint.ActivateHint(Rect: TRect; const AHint: string);
begin
  inherited;
  SetTransparentForm(Handle, FTransparency);
end;

constructor TTransparentHint.Create(AOwner: TComponent);
begin
 inherited;
 FTransparency := 100;
end;

procedure TTransparentHint.CreateParams(var Params: TCreateParams);
begin
 inherited;
 Params.ExStyle := Params.ExStyle or WS_EX_LAYERED;
end;

procedure TTransparentHint.CreateWnd;
begin
 inherited;
 SetTransparentForm(Handle, FTransparency);
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
  OldHintClass := SetNewHintClass(TTransparentHint);

finalization
  SetNewHintClass(OldHintClass);

end.
//------------------------------------------------------------------------------
//
// The About Box
//
// Cre 2004-02-17 Pma
//
//------------------------------------------------------------------------------
unit AboutBox;

interface

uses
  // These are put in by the compiler
  ComCtrls, jpeg, Controls, ExtCtrls, Classes, StdCtrls,

  // These I have to add

  Forms,    // TForm
  SysUtils, // For Extract File names etc
  Math, Buttons;     // Forr Min/Max etc.

type
  TAboutForm = class(TForm)
    TabViews: TPageControl;
    TabSheet1: TTabSheet;
    MapPanel: TPanel;
    BackImage: TImage;
    TabSheet2: TTabSheet;
    MapHelp: TRichEdit;
    TabSheet3: TTabSheet;
    ItemHelp: TRichEdit;
    TabSheet5: TTabSheet;
    MyHelp: TRichEdit;
    TabSheet6: TTabSheet;
    BoringHelp: TRichEdit;
    ButtonOk: TBitBtn;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}
//------------------------------------------------------------------------------
// User wants to close
//
procedure TAboutForm.ButtonOkClick(Sender: TObject);
begin
  Close;
end;
//------------------------------------------------------------------------------
// User creates the dialog
//
procedure TAboutForm.FormCreate(Sender: TObject);
var
  sFile : string;
begin

  sFile := ExtractFilePath(Application.ExeName) + 'MapHelp.rtf';
  if FileExists(sFile) then
    MapHelp.lines.LoadFromFile(sFile)
  else
    Application.MessageBox(PAnsiChar(sFile + ' does not exist'),'Open Help');

  sFile := ExtractFilePath(Application.ExeName) + 'ItemHelp.rtf';
  if FileExists(sFile) then
    ItemHelp.lines.LoadFromFile(sFile)
  else
    Application.MessageBox(PAnsiChar(sFile + ' does not exist'),'Open Help');

  sFile := ExtractFilePath(Application.ExeName) + 'MyHelp.rtf';
  if FileExists(sFile) then
    MyHelp.lines.LoadFromFile(sFile)
  else
    Application.MessageBox(PAnsiChar(sFile + ' does not exist'),'Open Help');

  sFile := ExtractFilePath(Application.ExeName) + 'BoringHelp.rtf';
  if FileExists(sFile) then
    BoringHelp.lines.LoadFromFile(sFile)
  else
    Application.MessageBox(PAnsiChar(sFile + ' does not exist'),'Open Help');
end;
//------------------------------------------------------------------------------
// User rezises the form
//
procedure TAboutForm.FormResize(Sender: TObject);
const
  clr = 4;

begin

  TabViews.Left := clr;
  TabViews.Top  := clr;
  TabViews.Width := ClientWidth - clr * 2;
  TabViews.Height := ClientHeight - ButtonOk.Height - clr * 3;

  MapPanel.Left := 0;
  MapPanel.Top  := 0;
  MapPanel.Width := TabViews.ClientWidth - clr * 2;
  MapPanel.Height := TabViews.ClientHeight - TabViews.TabHeight - clr * 8;

  BackImage.Left := 2;
  BackImage.Top := 2;
  BackImage.Width := MapPanel.ClientWidth - 4;
  BackImage.Height := MapPanel.ClientHeight - 4;
  
  MapHelp.Left := 0;
  MapHelp.Top  := 0;
  MapHelp.Width := TabViews.ClientWidth - clr * 2;
  MapHelp.Height := TabViews.ClientHeight - TabViews.TabHeight - clr * 8;

  ItemHelp.Left := 0;
  ItemHelp.Top  := 0;
  ItemHelp.Width := TabViews.ClientWidth - clr * 2;
  ItemHelp.Height := TabViews.ClientHeight - TabViews.TabHeight - clr * 8;

  MyHelp.Left := 0;
  MyHelp.Top  := 0;
  MyHelp.Width := TabViews.ClientWidth - clr * 2;
  MyHelp.Height := TabViews.ClientHeight - TabViews.TabHeight - clr * 8;

  BoringHelp.Left := 0;
  BoringHelp.Top  := 0;
  BoringHelp.Width := TabViews.ClientWidth - clr * 2;
  BoringHelp.Height := TabViews.ClientHeight - TabViews.TabHeight - clr * 8;

  ButtonOK.Left := clr;
  ButtonOK.Top := ClientHeight - ButtonOK.Height - clr;

end;
//------------------------------------------------------------------------------
// User tries to rizise the form, set the limits
//
procedure TAboutForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  NewWidth := max(NewWidth, ButtonOK.Width * 3);
  NewHeight := max(NewHeight,ButtonOK.Height * 8 );
  Resize := true;
end;

end.

unit DirDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ShellCtrls;

type
  TForm1 = class(TForm)
    DirCombo: TShellComboBox;
    DirTree: TShellTreeView;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ResultDir: TEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure DirComboClick(Sender: TObject);
    procedure DirTreeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    bOk : boolean;

    { Public declarations }
  end;

var
  Form1: TForm1;


implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  bOK := true;
  Close
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  bOK := false;
  Close;
end;

procedure TForm1.DirComboClick(Sender: TObject);
begin
  ResultDir.Text := DirCombo.Path;
end;

procedure TForm1.DirTreeClick(Sender: TObject);
begin
  ResultDir.Text := DirTree.Path;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  bOK := false;
end;


end.

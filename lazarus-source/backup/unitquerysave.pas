unit unitQuerySave;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormQuerySave }

  TFormQuerySave = class(TForm)
    BtnYes: TButton;
    BtnNo: TButton;
    BtnCancel: TButton;
    StaticText1: TStaticText;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnNoClick(Sender: TObject);
    procedure BtnYesClick(Sender: TObject);
  private

  public

  end;

var
  FormQuerySave: TFormQuerySave;

implementation

{$R *.lfm}

Uses UnitMain;

{ TFormQuerySave }

procedure TFormQuerySave.BtnCancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TFormQuerySave.BtnNoClick(Sender: TObject);
begin
  modalResult:=mrNo;
end;

procedure TFormQuerySave.BtnYesClick(Sender: TObject);
begin
  modalResult:=mrYes;

end;


end.


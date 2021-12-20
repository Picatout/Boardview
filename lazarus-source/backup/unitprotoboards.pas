unit unitProtoBoards;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TFrmBoards }

  TFrmBoards = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    RGboards: TRadioGroup;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
  private

  public

  end;

var
  FrmBoards: TFrmBoards;

implementation
uses
  UnitMain;

{$R *.lfm}

{ TFrmBoards }

procedure TFrmBoards.BtnCancelClick(Sender: TObject);
begin
  frmBoards.close;
end;

procedure TFrmBoards.BtnOkClick(Sender: TObject);
begin
  formMain.InstallProtoBoard(RGBoards.itemIndex);
  FrmBoards.close;
end;

end.


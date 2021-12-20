unit unitTag;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormTag }

  TFormTag = class(TForm)
    btnOK: TButton;
    BtnCancel: TButton;
    BtnFont: TButton;
    Edit1: TEdit;
    StaticText1: TStaticText;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnFontClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private

  public

  end;

var
  FormTag: TFormTag;

implementation

{$R *.lfm}
 uses unitMain;

{ TFormTag }

procedure TFormTag.BtnCancelClick(Sender: TObject);
begin
  Edit1.Caption:='';
  formTag.close;
end;

procedure TFormTag.BtnFontClick(Sender: TObject);

begin
  FormMain.FontDialog1.execute;
  with formMain.FontDialog1.Font do
  begin
     Edit1.Font.Color:=color;
     Edit1.Font.Name:=name;
     Edit1.Font.Style:=style;
     Edit1.Font.size:=size;

  end;
end;

procedure TFormTag.btnOKClick(Sender: TObject);
begin
  formTag.close;
end;

end.


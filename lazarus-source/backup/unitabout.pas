unit UnitAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

Const
  MAJOR=1;    // major version
  MINOR=1;    // minor version
  REVISION=1; // code revision

type

  { TFormAbout }

  TFormAbout = class(TForm)
    BtnAboutClose: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure BtnAboutCloseClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private

  public

  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.BtnAboutCloseClick(Sender: TObject);
begin
  FormAbout.close();

end;

procedure TFormAbout.FormActivate(Sender: TObject);
begin
   Memo1.lines[5]:='version '+MAJOR.ToString+'.'+MINOR.ToString+'.'+REVISION.ToString;
end;



end.


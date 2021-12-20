unit UnitAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFormAbout }

  TFormAbout = class(TForm)
    BtnAboutClose: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure BtnAboutCloseClick(Sender: TObject);
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

end.


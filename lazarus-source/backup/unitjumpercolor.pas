unit UnitJumperColor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFormColor }

  TFormColor = class(TForm)
    BtnClose: TButton;
    rgColors: TRadioGroup;
    procedure BtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgColorsSelectionChanged(Sender: TObject);
  private

  public

  end;

var
  FormColor: TFormColor;

implementation

uses unitMain;

{$R *.lfm}

{ TFormColor }

procedure TFormColor.BtnCloseClick(Sender: TObject);
begin
  FormMain.SetJumperColor(rgColors.ItemIndex);
  FormColor.Close;
end;

procedure TFormColor.FormCreate(Sender: TObject);
begin

end;

procedure TFormColor.rgColorsSelectionChanged(Sender: TObject);
begin
  FormColor.Refresh;
end;











end.


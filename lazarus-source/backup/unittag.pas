////////////////////////////////////////////////////////////////////////////////
// Copyright Jacques Deschênes 2021,2022
// This file is part of Boardview
//
//     Boardview is free software: you can redistribute it and/or modify
//     it under the terms of the GNU General Public License as published by
//     the Free Software Foundation, either version 3 of the License, or
//     (at your option) any later version.
//
//     Boardview is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY// without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//     GNU General Public License for more details.
//
//     You should have received a copy of the GNU General Public License
//     along with stm32_eforth.  If not, see <http://www.gnu.org/licenses/>.
////
////////////////////////////////////////////////////////////////////////////////

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
    BtnTagColor: TButton;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    StaticText1: TStaticText;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnFontClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure BtnTagColorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TFormTag.BtnTagColorClick(Sender: TObject);
begin
  ColorDialog1.execute;
  Edit1.font.color:=ColorDialog1.Color;
end;

procedure TFormTag.FormCreate(Sender: TObject);
begin
  {$IFDEF WINDOWS}
//   BtnTagColor.visible := false;
  {$ENDIF}
end;

end.


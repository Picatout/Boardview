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


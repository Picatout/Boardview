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


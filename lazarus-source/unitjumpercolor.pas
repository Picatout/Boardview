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


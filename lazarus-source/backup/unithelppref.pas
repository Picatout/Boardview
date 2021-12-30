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

unit unitHelpPref;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

const
    {$IFDEF WINDOWS}
    DOCS_PATH_EN='DOCS\en\';
    DOCS_PATH_FR='DOCS\fr\';
    {$ELSE}
    DOCS_PATH_EN='DOCS/en/';
    DOCS_PATH_FR='DOCS/fr/';
    {$ENDIF}

type
    enumLang=(English,French);
    enumFileType=(HTML,PDF);
const
    UserMan: array[enumLang,enumFileType] of string=(
    (DOCS_PATH_EN+'manual-en.html',DOCS_PATH_EN+'manual-en.pdf'),
    (DOCS_PATH_FR+'manual-fr.html',DOCS_PATH_FR+'manual-fr.pdf'));

type

  { TFormHelpPref }

  TFormHelpPref = class(TForm)
    btnOK: TButton;
    BtnCancel: TButton;
    rgLanguage: TRadioGroup;
    rgFileType: TRadioGroup;
    procedure BtnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormHelpPref: TFormHelpPref;

implementation

{$R *.lfm}
uses
   iniFiles;


{ TFormHelpPref }

procedure TFormHelpPref.BtnCancelClick(Sender: TObject);
begin
     close;
end;

procedure TFormHelpPref.btnOKClick(Sender: TObject);
var
  AppIni:TIniFile;
begin
    AppIni:=TIniFile.Create('Boardview.ini');
    AppIni.WriteInteger('help preferences','language',rgLanguage.itemIndex);
    AppIni.WriteInteger('help preferences','format',rgFileType.itemIndex);
    AppIni.UpdateFile;
    AppIni.Destroy;
    close;
end;

procedure TFormHelpPref.FormCreate(Sender: TObject);
var
  AppIni:TIniFile;
begin
    AppIni:=TIniFile.Create('Boardview.ini');
    rgLanguage.ItemIndex:=AppIni.ReadInteger('help preferences','language',integer(English));
    rgFileType.ItemIndex:=AppIni.ReadInteger('help preferences','format',integer(HTML));
    AppIni.Destroy;
end;

end.


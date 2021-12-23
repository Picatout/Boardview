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
    UserMan: array[0..1,0..1] of string=(
    ('DOCS\en\manual-en.html','DOCS\en\manual-en.pdf'),
    ('DOCS\fr\manual-fr.html','DOCS\fr\manual-fr.pdf'));

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

const
    cLangEnglish=0;
    cLangFrench=1;
    cFileTypeHTML=0;
    cFileTypePDF=1;

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
    rgLanguage.ItemIndex:=AppIni.ReadInteger('help preferences','language',cLangEnglish);
    rgFileType.ItemIndex:=AppIni.ReadInteger('help preferences','format',CFileTypeHTML);
    AppIni.Destroy;
end;

end.


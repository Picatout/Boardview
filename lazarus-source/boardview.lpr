////////////////////////////////////////////////////////////////////////////////
// Copyright Jacques DeschĂȘnes 2021,2022
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

program boardview;

{$mode objfpc}{$H+}


uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, printer4lazarus, unitMain, unitAbout, UnitJumperColor, unitComponents,
  CompLib, unitProtoBoards, unitTag, unitSaveProject, unitQuerySave,
  unitLibrary, unitHelpPref
  { you can add units after this };

{$IFDEF WINDOWS}
{$R app-icon.rc}
{$ENDIF}
//{$R *.res}


begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormColor, FormColor);
  CompLibrary:=TCompLib.create(defaultLibFile);
  Application.CreateForm(TFormComponents, FormComponents);
  Application.CreateForm(TFrmBoards, FrmBoards);
  Application.CreateForm(TFormTag, FormTag);
  Application.CreateForm(TFormSaveProject, FormSaveProject);
  Application.CreateForm(TFormQuerySave, FormQuerySave);
  Application.CreateForm(TFormLibrary, FormLibrary);
  Application.CreateForm(TFormHelpPref, FormHelpPref);
  application.icon.assign(FormMain.Icon);
  Application.Run;
end.


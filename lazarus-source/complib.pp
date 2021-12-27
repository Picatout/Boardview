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

unit CompLib;
// Components library definitions
{$mode objfpc}{$H+}
//{$M+}
interface

uses
  Classes, SysUtils, iniFiles;

type

  TCompLib=class(TObject)
    private
      libFile:TiniFile;
      Categories:TStrings;
      procedure LoadCategories;
    public
      constructor Create(FileName:string);
      destructor Destroy;  override;
      procedure getCatList(CatList:TStrings);
      Procedure GetComponents(category:string;CompList:TStrings);
      procedure addCategory(name:string);
      procedure deleteCategory(name:string);
      procedure DeleteComponent(category,name:string);
      function getValue(Catname,CompName:string):string;
      procedure setValue(CatName,CompName,Value:string);
    end;
const
    defaultLibFile='components.ini';


var
  CompLibrary: TCompLib;

implementation
uses  dialogs;

      constructor TCompLib.create(FileName:string);
      begin
         try
          libFile:=TIniFile.create(FileName);
          LoadCategories();
         except
           ShowMessage('Error loading components Library.');
         end;
      end;

      destructor TCompLib.destroy;
      begin
         Categories.free;
         libFile.free;
         inherited destroy;
      end;

      procedure TCompLib.getCatList(CatList:TStrings);
      begin
          CatList.AddStrings(categories,true);
      end;

      // get components in category
      procedure TCompLib.getComponents(category:string;CompList:TStrings);
      begin
         CompList.Clear;
         LibFile.ReadSection(category,CompList);
      end;

      procedure TCompLib.LoadCategories;
      begin
         if Categories <> nil then Categories.free;
         Categories:=TStringList.create;
         libFile.ReadSections(categories);
      end;


      procedure TCompLib.addCategory(name:string);
      begin
          LibFile.WriteString(name,'','');
          LibFile.UpdateFile;
          LoadCategories;
      end;

      procedure TCompLib.deleteCategory(name:string);
      begin
         libFile.EraseSection(name);
         libFile.UpdateFile;
         LoadCategories;
      end;

      procedure TCompLib.DeleteComponent(category,name:string);
      begin
           libFile.DeleteKey(category,name);
           libFile.UpdateFile;
      end;

      function TCompLib.getValue(Catname,CompName:string):string;
      begin
         result:=libFile.ReadString(CatName,CompName,'');
      end;

      procedure TCompLib.setValue(CatName,CompName,Value:string);
      begin
          LibFile.WriteString(CatName,CompName,Value);
          LibFile.UpdateFile;
      end;

end.


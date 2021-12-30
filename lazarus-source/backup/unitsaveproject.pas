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

unit unitSaveProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

const
  {$IFDEF WINDOWS}
  PATH_SEP='\';
  {$ELSE}
  PATH_SEP='/';
  {$ENDIF}
type

  { TFormSaveProject }

  TFormSaveProject = class(TForm)
    btnExploreDir: TButton;
    BtnConfirm: TButton;
    btnCancel: TButton;
    EditDirectory: TEdit;
    EditProjectName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure btnCancelClick(Sender: TObject);
    procedure BtnConfirmClick(Sender: TObject);
    procedure btnExploreDirClick(Sender: TObject);
    procedure EditDirectoryChange(Sender: TObject);
    procedure EditProjectNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
     projectName:string;
     directory:string;
end;

var
  FormSaveProject: TFormSaveProject;

implementation

{$R *.lfm}
uses lazFileUtils;

{ TFormSaveProject }

procedure TFormSaveProject.btnExploreDirClick(Sender: TObject);
begin
  if selectDirectoryDialog1.execute and (length(SelectDirectoryDialog1.FileName)>0) then
  begin
    EditDirectory.text:=SelectDirectoryDialog1.FileName+PATH_SEP+EditProjectName.text;
  end
  else
  begin
    EditDirectory.text:='.'+PATH_SEP+EditProjectName.text;
  end;
end;

procedure TFormSaveProject.EditDirectoryChange(Sender: TObject);
begin

end;

procedure TFormSaveProject.EditProjectNameChange(Sender: TObject);
var
  filePath:string;
  nameOnly:string;
begin
  nameOnly:=ExtractFileNameOnly(editProjectName.Text);
  if length(EditDirectory.text)>0 then
  begin
     FilePath:=ExtractFilePath(EditDirectory.text);
     EditDirectory.text:=filePath+nameOnly;
  end
  else
  begin
       EditDirectory.text:='.'+PATH_SEP+nameOnly;
  end;

end;

procedure TFormSaveProject.FormCreate(Sender: TObject);
begin
  directory:='.'+PATH_SEP;
  ProjectName:='';
  SelectDirectoryDialog1.filename:='.';
end;

procedure TFormSaveProject.btnCancelClick(Sender: TObject);
begin
  EditProjectName.text:=ProjectName;
  EditDirectory.text:=directory;
  modalResult:=mrCancel;
end;

procedure TFormSaveProject.BtnConfirmClick(Sender: TObject);
begin
  FormSaveProject.Close;
  directory:=EditDirectory.text;
  ProjectName:=EditProjectName.text;
  modalResult:=mrYes;
end;

end.


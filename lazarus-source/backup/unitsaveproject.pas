unit unitSaveProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

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
uses lazFileUtils,UnitMain;

{ TFormSaveProject }

procedure TFormSaveProject.btnExploreDirClick(Sender: TObject);
begin
  if selectDirectoryDialog1.execute and (length(SelectDirectoryDialog1.FileName)>0) then
  begin
    EditDirectory.text:=SelectDirectoryDialog1.FileName+'\'+EditProjectName.text;
  end
  else
  begin
    EditDirectory.text:='.\'+EditProjectName.text;
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
       EditDirectory.text:='.\'+nameOnly;
  end;

end;

procedure TFormSaveProject.FormCreate(Sender: TObject);
begin
  directory:='.\';
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


unit unitHelpPref;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

const
    UserMan: array[0..1] of array[0..1] of string=(
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


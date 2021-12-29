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

unit unitLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons;

type

  { TFormLibrary }

  TFormLibrary = class(TForm)
    bntFileSelect: TButton;
    BtnAddCat: TButton;
    BtnRemoveCat: TButton;
    BtnAddComp: TButton;
    BtnRemoveComp: TButton;
    BtnClose: TButton;
    EditNewComp: TEdit;
    EditBmpFile: TEdit;
    EditNewCat: TEdit;
    imgComponent: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbCategory: TListBox;
    lbComponent: TListBox;
    procedure bntFileSelectClick(Sender: TObject);
    procedure BtnAddCatClick(Sender: TObject);
    procedure BtnAddCompClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnRemoveCompClick(Sender: TObject);
    procedure BtnRemoveCatClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lbCategoryClick(Sender: TObject);
    procedure lbComponentClick(Sender: TObject);
  private
    selectedBMP:String;
  public

  end;

var
  FormLibrary: TFormLibrary;

implementation

{$R *.lfm}
 uses UnitMain,CompLib,UnitComponents;

{ TFormLibrary }

procedure TFormLibrary.BtnCloseClick(Sender: TObject);
begin
  FormLibrary.Close;
end;

procedure TFormLibrary.BtnRemoveCompClick(Sender: TObject);
begin
   CompLibrary.DeleteComponent(lbCategory.items[lbCategory.itemIndex],
                               lbComponent.items[lbComponent.itemIndex]);
   lbComponent.Items.Delete(lbComponent.ItemIndex);
   lbComponent.ItemIndex:=0;
   lbComponentClick(self);
end;

procedure TFormLibrary.BtnRemoveCatClick(Sender: TObject);
begin
    CompLibrary.deleteCategory(lbCategory.items[lbCategory.itemIndex]);
    lbCategory.items.Delete(lbCategory.itemIndex);
    if lbCategory.count>0 then
    begin
       lbCategory.ItemIndex:=0;
       EditNewCat.text:=lbCategory.items[lbCategory.ItemIndex];
       lbCategoryclick(self);
       formComponents.reloadCategory;
    end
    else
    begin
      lbComponent.Items.Clear;
      editNewComp.text:='';
      editbmpFile.text:='';
      imgComponent.canvas.clear;
    end;
end;

procedure TFormLibrary.bntFileSelectClick(Sender: TObject);
begin
  formMain.OpenDialog1.Filter:='bitmaps files|*.bmp|all files|*.*';
  if FormMain.OpenDialog1.execute then
  begin
     EditBmpFile.Caption:=ExtractFileName(FormMain.OpenDialog1.Filename);
  end;
end;

procedure TFormLibrary.BtnAddCatClick(Sender: TObject);
begin
   CompLibrary.AddCategory(EditNewCat.text);
   lbCategory.items.add(EditNewCat.text);
   lbCategory.itemIndex:=lbCategory.Count-1;
   lbComponent.Items.Clear;
   lbComponent.ItemIndex:=-1;
   EditNewComp.text:='';
   EditbmpFile.text:='';
   imgComponent.canvas.Clear;
end;

procedure TFormLibrary.BtnAddCompClick(Sender: TObject);
begin
   CompLibrary.SetValue(lbCategory.Items[lbCategory.ItemIndex],EditNewComp.Text,EditBmpFile.text);
   lbComponent.ItemIndex:=lbComponent.Items.Add(EditNewComp.text);
   ImgComponent.Picture.LoadFromFile(BMP_PATH+EditBmpFile.text);
end;

procedure TFormLibrary.FormActivate(Sender: TObject);
begin
     CompLibrary.getCatList(lbCategory.items);
     lbCategory.ItemIndex:=0;
     LbCategoryClick(self);
end;

procedure TFormLibrary.lbCategoryClick(Sender: TObject);
begin
     EditNewCat.text:=lbCategory.items[lbCategory.ItemIndex];
     CompLibrary.getComponents(lbCategory.Items[lbCategory.ItemIndex],lbComponent.Items);
     if lbComponent.count>0 then lbComponent.itemIndex:=0;
     lbComponentClick(self);
end;

procedure TFormLibrary.lbComponentClick(Sender: TObject);
begin
  if lbComponent.Count>0 then
  begin
      EditNewcomp.text:= lbComponent.Items[lbComponent.ItemIndex];
      selectedBMP:=CompLibrary.getValue(lbCategory.Items[lbCategory.ItemIndex],lbComponent.Items[lbComponent.ItemIndex]);
      imgComponent.Picture.LoadFromFile(BMP_PATH+SelectedBMP);
      EditBmpFile.text:=SelectedBMP;
  end
  else
  begin
     SelectedBMP:='';
     imgComponent.canvas.clear;
     EditBmpFile.Text:='';
     EditNewComp.text:='';
  end;
end;

end.


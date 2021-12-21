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

unit unitComponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, compLib
  ;

type

  { TFormComponents }

  TFormComponents = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    BtnTagFont: TButton;
    BtnRot90: TButton;
    BtnRot180: TButton;
    BtnRotMinus90: TButton;
    BtnFreeze: TButton;
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    ImgComponent: TImage;
    lblTag: TLabel;
    lbCategory: TListBox;
    lbComponent: TListBox;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    stTag: TStaticText;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnFreezeClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnRot180Click(Sender: TObject);
    procedure BtnRot90Click(Sender: TObject);
    procedure BtnRotMinus90Click(Sender: TObject);
    procedure BtnTagFontClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImgComponentClick(Sender: TObject);
    procedure ImgComponentDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ImgComponentEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure lbCategoryClick(Sender: TObject);
    procedure lbComponentClick(Sender: TObject);
    procedure Rotate(angle:integer);
    procedure stTagDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure stTagEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure stTagMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure stTagMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
  private
    selectedBmp:string;
    cTagleft,cTagTop:integer;
  public
     procedure reloadCategory;
  end;



var
  FormComponents: TFormComponents;

implementation

{$R *.lfm}

uses
  UnitMain;

{ TFormComponents }

procedure TFormComponents.reloadCategory;
begin
  CompLibrary.getCatList(lbCategory.items);
  lbCategory.ItemIndex:=0;
end;

procedure TFormComponents.BtnCancelClick(Sender: TObject);
begin
  FormComponents.close;
end;

procedure TFormComponents.BtnFreezeClick(Sender: TObject);
var
  x,y:integer;
begin
  stTag.visible:=false;
  stTag.Caption:='';
  x:=stTag.left-imgComponent.left;
  y:=stTag.top-imgcomponent.top;
  ImgComponent.canvas.Brush.Style:=bsClear;
  ImgComponent.Canvas.TextOut(x,y,Edit1.caption);
  edit1.Caption:='';
  stTag.left:=imgComponent.left;
  stTag.top:=ImgComponent.top;
end;


procedure TFormComponents.BtnOKClick(Sender: TObject);
begin
   formMain.AddComponent(ImgComponent.Picture);
   FormComponents.close;
end;

procedure TFormComponents.BtnRot180Click(Sender: TObject);
begin
  rotate(180);
end;

procedure TFormComponents.BtnRot90Click(Sender: TObject);
begin
    rotate(90);
end;

procedure TFormComponents.BtnRotMinus90Click(Sender: TObject);
begin
  rotate(-90);
end;

procedure TFormComponents.BtnTagFontClick(Sender: TObject);
begin
    formMain.FontDialog1.execute;
    with formMain.FontDialog1.font do
    begin
      imgComponent.canvas.font.Size:=size;
      Edit1.font.size:=size;
      stTag.font.size:=size;
      imgComponent.canvas.font.style:=Style;
      Edit1.Font.Style:=style ;
      stTag.font.Style:=style;
      imgComponent.canvas.font.Name:=name;
      Edit1.font.name:=name;
      stTag.font.name:=name;
      imgComponent.canvas.font.color:=color;
      Edit1.font.color:=color;
      stTag.font.color:=color;
    end;
end;

procedure TFormComponents.Edit1Change(Sender: TObject);
begin
  stTag.Visible:=false;
  if length(edit1.Caption)>0 then
  begin
       stTag.caption:=Edit1.caption;
       stTag.Visible:=true;
  end;
end;

procedure TFormComponents.FormActivate(Sender: TObject);
begin

end;

procedure TFormComponents.FormCreate(Sender: TObject);
var
  i:integer;
begin
     CompLibrary.getCatList(lbCategory.items);
     lbCategory.ItemIndex:=0;
     LbCategoryClick(self);
end;

procedure TFormComponents.ImgComponentClick(Sender: TObject);
var
  cursorPos:TPoint;
  pixelcolor:TColor;
begin
   cursorPos:=ScreenToClient(mouse.CursorPos);
   CursorPos.X:=CursorPos.X-imgComponent.left;
   cursorpos.Y:=CursorPos.Y-imgComponent.top;
   pixelcolor:=ImgComponent.canvas.Pixels[cursorPos.x,cursorPos.Y];
   if pixelcolor=clBLACK2 then
   begin
        ColorDialog1.execute;
        imgComponent.Canvas.Brush.color:=ColorDialog1.Color;
        ImgComponent.canvas.FloodFill(CursorPos.x,cursorPos.y,clBLACK2,fsSurface);
   end;
end;

procedure TFormComponents.ImgComponentDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  accept:=true;
end;

procedure TFormComponents.ImgComponentEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin

end;

procedure TFormComponents.lbCategoryClick(Sender: TObject);
begin
  CompLibrary.getComponents(lbCategory.Items[lbCategory.ItemIndex],lbComponent.Items);
  if lbComponent.count>0 then lbComponent.ItemIndex:=0;
  lbComponentClick(self);
end;

procedure TFormComponents.lbComponentClick(Sender: TObject);

begin
  if lbComponent.itemIndex>-1 then
  begin
      selectedBMP:=CompLibrary.getValue(lbCategory.Items[lbCategory.ItemIndex],lbComponent.Items[lbComponent.ItemIndex]);
      imgComponent.Picture.LoadFromFile('bitmaps\'+SelectedBMP);
      Edit1.Caption:='';
      StTag.left:=imgComponent.left;
      stTag.Top:=imgComponent.top;
      with formMain.FontDialog1.font do
      begin
        imgComponent.canvas.font.Size:=size;
        Edit1.font.size:=size;
        stTag.font.size:=size;
        imgComponent.canvas.font.style:=Style;
        Edit1.Font.Style:=style ;
        stTag.font.Style:=style;
        imgComponent.canvas.font.Name:=name;
        Edit1.font.name:=name;
        stTag.font.name:=name;
        imgComponent.canvas.font.color:=color;
        Edit1.font.color:=color;
        stTag.font.color:=color;
      end;
  end
  else
  begin
      imgComponent.Canvas.clear;
  end;
end;


procedure TFormComponents.Rotate(angle:integer);
var
  bmp:TBitmap;
  x,y:integer;
  bmprect:TRect;
begin
  bmp:=TBitmap.create;
  case angle of
    90: // +90 deg
    begin
      bmp.width:=ImgComponent.canvas.Height;
      bmp.Height:=ImgComponent.canvas.width;
      bmprect:=rect(0,0,bmp.width,bmp.height);
      for y:=0 to ImgComponent.Height-1 do
          for x:= 0 to ImgComponent.Width-1 do
          begin
             bmp.Canvas.Pixels[bmp.width-1-y,x]:=ImgComponent.canvas.pixels[x,y];
          end;
    end;
    180: // 180 deg
    begin
      bmp.width:=ImgComponent.canvas.width;
      bmp.height:=ImgComponent.canvas.Height;
      bmprect:=rect(0,0,bmp.width,bmp.height);
      for y:=0 to ImgComponent.Height-1 do
          for x:= 0 to ImgComponent.Width-1 do
          begin
             bmp.Canvas.Pixels[bmp.width-1-x,bmp.height-1-y]:=ImgComponent.canvas.pixels[x,y];
          end;

    end;
    -90: // -90 deg
    begin
      bmp.width:=ImgComponent.canvas.Height;
      bmp.Height:=ImgComponent.canvas.width;
      bmprect:=rect(0,0,bmp.width,bmp.height);
      for y:=0 to ImgComponent.Height-1 do
          for x:= 0 to ImgComponent.Width-1 do
          begin
             bmp.Canvas.Pixels[y,x]:=ImgComponent.canvas.pixels[ImgComponent.canvas.width-1-x,y];
          end;
    end;
  end;
  Imgcomponent.Picture.Bitmap.SetSize(bmprect.Width,bmprect.height);
  ImgComponent.Canvas.CopyRect(bmprect,bmp.canvas,bmprect);
  ImgComponent.refresh;
  bmp.Destroy;
end;

procedure TFormComponents.stTagDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  accept:=true;
end;

procedure TFormComponents.stTagEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  with stTag do
  begin
    visible:=false;
    left:=imgComponent.left+x;
    top:=imgComponent.Top+y;
    visible:=true;
  end;
end;

procedure TFormComponents.stTagMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TFormComponents.stTagMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  stTag.visible:=false;
  x:=imgComponent.left;
  Y:=ImgComponent.top;
  if x>(imgComponent.Left+imgComponent.width-1) then x:= (imgComponent.Left+imgComponent.width-1);
  if Y>(imgComponent.top+imgComponent.Height-1) then y:=(imgComponent.top+imgComponent.Height-1);
  stTag.Left:=X;
  stTag.top:=Y;
  stTag.Visible:=true;;
end;


end.


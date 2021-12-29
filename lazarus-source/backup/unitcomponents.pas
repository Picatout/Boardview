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
    ColorDialog1: TColorDialog;
    EditTag: TEdit;
    ImgComponent: TImage;
    lblTag: TLabel;
    lbCategory: TListBox;
    lbComponent: TListBox;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnRot180Click(Sender: TObject);
    procedure BtnRot90Click(Sender: TObject);
    procedure BtnRotMinus90Click(Sender: TObject);
    procedure BtnTagFontClick(Sender: TObject);
    procedure EditTagChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImgComponentClick(Sender: TObject);
    procedure ImgComponentMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImgComponentMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImgComponentMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImgComponentPaint(Sender: TObject);
    procedure lbCategoryClick(Sender: TObject);
    procedure lbComponentClick(Sender: TObject);
    procedure Rotate(angle:integer);
  private
    picComponent:TPicture;
    selectedBmp:string;
    rotation:integer; // bitmap rotation
    ctagLeft,ctagTop:integer;
    draggingTag:boolean;
    startX,startY:integer; // tag drag mouse start position
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
  if lbCategory.count>0 then lbCategory.ItemIndex:=0;
end;

procedure TFormComponents.BtnCancelClick(Sender: TObject);
begin
  FormComponents.close;
end;

procedure TFormComponents.BtnOKClick(Sender: TObject);
begin
   imgComponent.Picture.bitmap.canvas.clear;
   ImgComponentPaint(self);
   formMain.AddComponent(imgComponent.picture);
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
      EditTag.font.size:=size;
      EditTag.Font.Style:=style ;
      EditTag.font.name:=name;
      EditTag.font.color:=color;
    end;
    imgComponent.Refresh;
end;

procedure TFormComponents.EditTagChange(Sender: TObject);
begin
     imgComponent.refresh;
end;

procedure TFormComponents.FormActivate(Sender: TObject);
begin
    rotation:=0;
    imgComponent.refresh;
end;

procedure TFormComponents.FormClick(Sender: TObject);
begin

end;


procedure TFormComponents.FormCreate(Sender: TObject);

begin

     picComponent:=TPicture.create;
     imgComponent.canvas.Brush.color:=clWhite;
     imgComponent.Canvas.clear;
     CompLibrary.getCatList(lbCategory.items);
     lbCategory.ItemIndex:=0;
     LbCategoryClick(self);
end;

procedure TFormComponents.ImgComponentClick(Sender: TObject);
var
  pixelcolor:TColor;
begin
   pixelcolor:=PicComponent.bitmap.canvas.Pixels[startX,StartY];
   if pixelcolor=clBLACK2 then
   begin
        ColorDialog1.execute;
        PicComponent.bitmap.Canvas.Brush.color:=ColorDialog1.Color;
        PicComponent.bitmap.canvas.FloodFill(startX,startY,clBLACK2,fsSurface);
        imgComponent.refresh;
   end;
end;


procedure TFormComponents.ImgComponentMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  startX:=X;
  startY:=y;
  draggingTag:=true;
end;

procedure TFormComponents.ImgComponentMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if draggingTag then
   begin
     ctagLeft:=cTagLeft+X-startX;
     ctagTop:=cTagTop+Y-startY;
     startX:=X;
     startY:=Y;
     imgComponent.refresh;
   end;
end;

procedure TFormComponents.ImgComponentMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if draggingTag then
   begin
        ctagLeft:=cTagLeft+X-StartX;
        ctagTop:=cTagTop+Y-startY;
        imgComponent.refresh;
        draggingTag:=false;
   end;
end;

procedure TFormComponents.ImgComponentPaint(Sender: TObject);
var
  picrect:Trect;
begin
  with ImgComponent.Canvas do
  begin
    clear;
    imgComponent.Picture.Bitmap.SetSize(picComponent.Width,picComponent.Height);
    picRect:=rect(0,0,picComponent.bitmap.width,picComponent.bitmap.height);
    copyRect(picRect,picComponent.Bitmap.Canvas,picRect);
    with EditTag.Font do
    begin
      font.color:=color;
      font.Style:=style;
      font.Size:=size;
      font.name:=name;
      font.Canvas.Brush.style:=bsClear;
      font.Orientation:=-rotation*10;
    end;
    if length(EditTag.text)>0 then TextOut(ctagLeft,ctagTop,editTag.text);
  end;
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
      picComponent.Clear;
      picComponent.LoadFromFile('bitmaps\'+SelectedBMP);
      imgComponent.Picture.Bitmap.SetSize(picComponent.Width,picComponent.Height);
      EditTag.Caption:='';
      rotation:=0;
      cTagLeft:=0;
      cTagTop:=0;
      with formMain.FontDialog1.font do
      begin
        imgComponent.canvas.font.Size:=size;
        EditTag.font.size:=size;
        imgComponent.canvas.font.style:=Style;
        EditTag.Font.Style:=style ;
        imgComponent.canvas.font.Name:=name;
        EditTag.font.name:=name;
        imgComponent.canvas.font.color:=color;
        EditTag.font.color:=color;
      end;
  end
  else
  begin
      imgComponent.Canvas.clear;
  end;
  imgComponent.refresh;
end;



procedure TFormComponents.Rotate(angle:integer);
var
  bmp:TBitmap;
  x,y,tmp:integer;
  bmprect:TRect;
begin
  bmp:=TBitmap.create;
  tmp:=cTagLeft;
  case angle of
    -90: // -90 clockwise rotation
    begin
      rotation:=rotation+90;
      bmp.width:=picComponent.bitmap.Height;
      bmp.Height:=picComponent.bitmap.width;
      bmprect:=rect(0,0,bmp.width,bmp.height);
      cTagLeft:=bmp.width-1-cTagTop;
      cTagTop:=tmp;
      for y:=0 to picComponent.Height-1 do
          for x:= 0 to picComponent.Width-1 do
          begin
             bmp.Canvas.Pixels[bmp.width-1-y,x]:=picComponent.bitmap.canvas.pixels[x,y];
          end;
    end;
    180: // 180 deg
    begin
      rotation:=rotation+180;
      bmp.width:=picComponent.bitmap.width;
      bmp.height:=picComponent.bitmap.Height;
      bmprect:=rect(0,0,bmp.width,bmp.height);
      cTagLeft:=bmp.width-1-cTagLeft;
      cTagTop:=bmp.height-1-cTagTop;
      for y:=0 to bmp.height-1 do
          for x:= 0 to bmp.width-1 do
          begin
             bmp.Canvas.Pixels[bmp.width-1-x,bmp.height-1-y]:=picComponent.bitmap.canvas.pixels[x,y];
          end;

    end;
    +90: // +90 conterclockwise rotation
    begin
      rotation:=rotation-90;
      bmp.width:=picComponent.bitmap.Height;
      bmp.Height:=picComponent.bitmap.width;
      bmprect:=rect(0,0,bmp.width,bmp.height);
      cTagLeft:=cTagTop;
      cTagTop:=bmp.height-1-tmp;
      for y:=0 to picComponent.bitmap.Height-1 do
          for x:= 0 to picComponent.bitmap.Width-1 do
          begin
             bmp.Canvas.Pixels[y,x]:=picComponent.bitmap.canvas.pixels[picComponent.bitmap.canvas.width-1-x,y];
          end;
    end;
  end;
  rotation:=rotation mod 360;
  piccomponent.Bitmap.SetSize(bmprect.Width,bmprect.height);
  picComponent.bitmap.Canvas.CopyRect(bmprect,bmp.canvas,bmprect);
  ImgComponent.refresh;
  bmp.Destroy;
end;




end.


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

unit Unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, ComCtrls, Buttons, UnitAbout, LCLType, Grids, PrintersDlgs,printers,
  osPrinters,FPImage,iniFiles,
  CompLib,unitComponents, UnitJumperColor, unitProtoboards,unitTag
  ;

type
   // prototyping board
   TBoard = record
      name:string; // board name
      fileName:string; // image file
      left,top:integer; // position in client area.
   end;
   PTBoard = ^TBoard;

   TenumCE = (ceNone,ceJumper,ceComponent,ceTag);

   // jumper installed on prototyping board
   TJumper = Record
    StartPt:TPoint;   // jumper start point
    EndPt:TPoint;     // jumper end point
    color:TColor;     // jumper color
  end;

   PTJumper = ^TJumper;

   // Label added to circuit
   TTag = record
    left,top:integer; // top left Corner
    text:string;
    color:TColor;
    FontSize:integer;
    FontName:string;
    FontStyle:TFontStyles;
   end;

   PTTag=^TTag;

   // component installed on prototyping board
   TComponent = Record
    name:string;  // component name
    category:string;  // component category
    image:TPicture;     // object holding component image.
    left,top:integer;   // component position
   end;

   PTComponent = ^TComponent ;

   // objects installed on proto board
   TCircuitElement = Record
      case kind:TEnumCE of // kind of object: jumper or component
      ceJumper:
        (wire:PTJumper);
      ceComponent:
        (component:PTComponent);
      ceTag:
        (tag:PTTag);
   end;

   PTCircuitElement = ^TCircuitElement;

  { TFormMain }

  TFormMain = class(TForm)
    DlgCustomJumper: TColorDialog;
    FontDialog1: TFontDialog;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    mDelete: TMenuItem;
    mColor: TMenuItem;
    mTagFont: TMenuItem;
    N1: TMenuItem;
    mMove: TMenuItem;
    mClone: TMenuItem;
    N2: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemComponent: TMenuItem;
    MenuItemTag: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemJumperColor: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuLibrary: TMenuItem;
    MenuItemLibrary: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemJumpers: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemSavebmp: TMenuItem;
    MenuItemPrint: TMenuItem;
    MenuItemQuit: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItemAdd: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemBoardSelect: TMenuItem;
    MenuItemManual: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    PrintDialog1: TPrintDialog;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButtonSaveAs: TToolButton;
    ToolButtonTag: TToolButton;
    ToolButtonAdd: TToolButton;
    ToolButtonJumper: TToolButton;
    ToolButtonSep2: TToolButton;
    ToolButtonUndo: TToolButton;
    ToolButtonSep1: TToolButton;
    ToolButtonOpen: TToolButton;
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mCloneClick(Sender: TObject);
    procedure mColorClick(Sender: TObject);
    procedure mDeleteClick(Sender: TObject);
    procedure MenuItemLibraryClick(Sender: TObject);
    procedure MenuItemManualClick(Sender: TObject);
    procedure MenuLibraryClick(Sender: TObject);
    procedure MenuItemPrintClick(Sender: TObject);
    procedure mTagFontClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemBoardSelectClick(Sender: TObject);
    procedure MenuItemComponentClick(Sender: TObject);
    procedure MenuItemNewClick(Sender: TObject);
    procedure MenuItemPredefColorClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemLargeBoardClick(Sender: TObject);
    procedure MenuItemMediumBoardClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    procedure MenuItemSavebmpClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
    procedure MenuItemSmallBoardClick(Sender: TObject);
    procedure mMoveClick(Sender: TObject);
    procedure ToolButtonCmpntClick(Sender: TObject);
    procedure SetJumperColor(index:integer);
    procedure ToolButtonJumperClick(Sender: TObject);
    procedure ToolButtonOpenClick(Sender: TObject);
    procedure ToolButtonTagClick(Sender: TObject);
    procedure ToolButtonUndoClick(Sender: TObject);
  private
    BoardImage:TPicture;
    Board:TBoard; // prototyping board in usage
    StartPt,EndPt:TPoint; // installing jumper position
    ceDragging:TenumCE;   // installing a circuit element
    ceDragidx:integer; // index of component being drag
    CircuitList:TFPList;   // list of jumper and components on board.
    modified:boolean; // design modified
    procedure AddJumper; // add a jumper to the board
    procedure undo; // cancel last operation
    procedure MousePosition(X,Y:integer); // get mouse position in client area.
    procedure SaveDesign; // save actual design
    procedure SaveAsDesign; // save actual desing under specified name.
    procedure OpenDesign; // open an existing Design
    procedure SaveBitmap; // save actual design as bitmap.
    procedure PrintDesign; // print acctual design.
    procedure FreeCircuitItem(i:integer); // delete item from list
    procedure FreeCircuitList; // free Items in CircuitList
    procedure MoveComponent(comp:PTComponent;X,Y:integer); // dragging component on board
    function OverComponent(cursorPos:TPoint;compIdx:integer):boolean; // check for 1 Circuit element
    function MouseOverElement(CursorPos:Tpoint):integer; // scans CircuitList
    procedure UpdateCaption;
    procedure UpdateStatusBar(p1text:string);
    procedure SaveProject(DirName,ProjectName:string);
    function QuerySaveModified:boolean;
    procedure NewProject;
    procedure LoadProject(ProjectName:string);
    function BoardIndexFromName(BoardName:string):integer;
  public
    JumperColor:TColor;  // current jumper color
    procedure InstallProtoBoard(index:integer);
    procedure AddComponent(cPic:TPicture); // add a component to board.
    procedure AddcTag(ctext:string);
  end;


var
  FormMain: TFormMain;
const
  // special colors
  clBLACK1=$010101;// tie point central dot
  clBLACK2=$020202; // resistor fill zone
  clGRAY1=$C3C3C3; // tie point inner shadow
  clGRAY2=$E6E6E6; // tie point outer shadow
  clWHITE1=$FFFFFF; // transparent white
  clWHITE2=$FEFEFE; // visible white
  clBrown=$4080;
  clOrange=$80ff;
  clFushia=$FF00FF;

  // predefined jumper colors
  PredefColors:array[0..9] of TColor=(clBlack,clBrown,clRed,clOrange,clYellow,clGreen,clBlue,
  clPurple,clGray,clFushia);

  PredefColorName:array[0..9] of string=('Black','Brown','Red','Orange','Yellow','Green',
  'Blue','Purple','Gray','Fushia');

  // prototyping   boards indexes.
  bsSMALL=0;
  bsMEDIUM=1;
  bsLARGE=2;
  // prototyping boards list.
  Boards:array[0..2] of TBoard=(
  (name:'small';fileName: 'bitmaps\protoboard-small.bmp';left:0;top:0),
  (name:'medium';filename:'bitmaps\protoboard-medium.bmp';left:0;top:0),
  (name:'large';filename: 'bitmaps\protoboard-large.bmp';left:0;top:0)
  );

implementation

{$R *.lfm}

uses lazfileutils, unitSaveProject, UnitQuerySave,LCLintf,unitLibrary;

const
  defaultBmpPath='bitmaps\';

var
   bmpPath:string; // bitmaps files path.
   LibraryIni:string; // components file.

{ TFormMain }

// initialize a new project
procedure TFormMain.newProject;
begin
   FormSaveProject.ProjectName:='';
   InstallProtoBoard(bsMEDIUM);
end;

// return true to proceed with next operation else cancel.
function TFormMain.QuerySaveModified:boolean;
begin
    if not Modified then
    begin
      result:=true;
      exit;
    end;
    case FormQuerySave.showModal of
       mrYes:
       begin
          MenuItemSaveClick(self);
          result:=true;
       end;
       mrNo:
       begin
          modified:=false;
          result:=true;
       end;
       mrCancel:
       begin
          result:=false;
       end;
    end;
end;


procedure TFormMain.UpdateCaption;
begin
  caption := 'Boardview - '+ formSaveProject.ProjectName;
  if modified then caption := caption + '*';
end;

procedure TFormMain.UpdateStatusbar(p1text:string);
begin
    statusbar1.Panels[0].text:='Circuit elements count:'+CircuitList.count.ToString;
    statusbar1.Panels[1].text:=p1text;
    statusbar1.refresh;
end;


function TFormMain.BoardIndexFromName(BoardName:string):integer;
var i:integer;
begin
     result:=-1;
     for i:=0 to length(Boards)-1 do
        if (Boards[i].name=BoardName) then
        begin
          result:= i;
          exit;
        end;
end;

procedure TFormMain.InstallProtoBoard(index:integer);
begin
     if circuitList.Count>0 then
     begin
        FreeCircuitList;
     end;
     cedragging:=ceNone;
     ceDragIdx:=-1;
     JumperColor:=clBlack;
     Canvas.Pen.Color:=JumperColor;
     self.board:=Boards[index];
     if BoardImage<>nil then BoardImage.Destroy;
     BoardImage:=TPicture.create;
     BoardImage.LoadFromFile(board.fileName);
     Canvas.Clear;
     self.board.left:=(ClientWidth-BoardImage.Width)div 2;
     self.board.top:=(ClientHeight-Toolbar1.Height-BoardImage.height) div 2;
     Canvas.draw(board.left,board.top,BoardImage.Graphic);
     formMain.Refresh;
     StatusBar1.SimpleText:='jumper color: Black';
     Modified:=false;
     if (formSaveProject)<>nil then
     begin
        FormSaveProject.ProjectName:='';
        UpdateCaption;
     end;
end;
{
  check if the mouse cursor is over circuit element.
}
function TFormMain.OverComponent(cursorPos:TPoint;compIdx:integer):boolean;
var
   node:PTCircuitElement;
   comp:PTComponent;
   ctag:PTTag;
   x1,x2,y1,y2,xc,yc,dx,dy:integer;
   h1,h2,h3:real;
   rect:TRect;
begin // return CircuitList itemIndex if mouse cursor over a circuit element
        result:=false;
        node:=CircuitList.items[compIdx];
        case node^.kind of
          ceJumper:
          begin
             x1:=PTJumper(node^.wire)^.StartPt.x;
             y1:=PTJumper(node^.wire)^.StartPt.y;
             x2:=PTJumper(node^.wire)^.EndPt.x;
             y2:=PTJumper(node^.wire)^.EndPt.y;
             xc:=CursorPos.x;
             yc:=cursorPos.y;
             dx:=abs(x2-x1);
             dy:=abs(y2-y1);
             h1:=sqrt(dx*dx+dy*dy);
             dx:=abs(xc-x1);
             dy:=abs(yc-y1);
             h2:=sqrt(dx*dx+dy*dy);
             dx:=abs(xc-x2);
             dy:=abs(yc-y2);
             h3:=sqrt(dx*dx+dy*dy);
             if (h1+1)>=(h2+h3) then
             begin
                result:=true;
             end;
          end;
          ceComponent:
          begin
             comp:=node^.component;
             rect.left:=comp^.left;
             rect.top:=comp^.top;
             rect.width:=comp^.image.Width;
             rect.height:= comp^.image.Height;
             if rect.contains(CursorPos) then
            begin
                 result:=true;
            end;
          end;
          ceTag:
          begin
            ctag:=node^.tag;
            canvas.Font.size:=ctag^.FontSize;
            canvas.Font.Style:=ctag^.FontStyle;
            canvas.Font.Color:=ctag^.color;
            canvas.Font.Name:=ctag^.FontName;
            Canvas.GetTextSize(ctag^.text, x1, y1);
            rect.Left:=ctag^.left;
            rect.top:=ctag^.top;
            rect.width:=x1;
            rect.height:=y1;
            if  rect.contains(cursorPos) then
            begin
                 result:=true;
            end;
          end;
        end;
end;

function TFormMain.MouseOverElement(CursorPos:Tpoint):integer; // scans CircuitList
var
   i:integer;
begin
     result:=-1;
     if CircuitList.count=0 then exit;
     for i:=0 to CircuitList.count-1 do
     begin
       if OverComponent(CursorPos,i) then
       begin
            result:=i;
            exit;
       end;
     end;
end;

procedure TFormMain.ToolButtonUndoClick(Sender: TObject);
begin
  undo;
end;


procedure TFormMain.SetJumperColor(index:integer);
var
  s:string;
begin
  s:=format('jumper color: %S',[FormColor.rgColors.items[index]]);
  StatusBar1.SimpleText:=s;
  JumperColor:=PredefColors[index];
  formMain.Canvas.Pen.color:=JumperColor;
  StatusBar1.Refresh;
end;

procedure TFormMain.AddJumper;
var
  Jumper:PTJumper;
  Circuit:PTCircuitElement;
begin
  new(Jumper);
  Jumper^.color:=JumperColor;
  Jumper^.startPt:=startPt;
  Jumper^.EndPt:=EndPt;
  new(Circuit);
  circuit^.kind:=ceJumper; // jumper
  circuit^.wire:=jumper;
  CircuitList.Add(circuit);
  FormMain.Refresh;
  cedragging:=ceJumper;
  ceDragIdx:=CircuitList.Count-1;
  modified:=true;
  UpdateStatusBar('Adding '+ FormColor.rgColors.Items[FormColor.rgColors.ItemIndex]+' wire');
  UpdateCaption;
end;

procedure TFormMain.AddComponent(cPic:TPicture);
var
  compPic:TPicture;
  Component:PTComponent;
  Circuit:PTCircuitElement;
  bmpRect:TRect;
begin
   bmpRect:=rect(0,0,cPic.bitmap.width,cPic.bitmap.height);
   compPic:=TPicture.create;
   compPic.Bitmap.SetSize(cPic.Bitmap.width,cPic.Bitmap.height);
   CompPic.Bitmap.canvas.CopyRect(bmpRect,cPic.Bitmap.canvas,bmpRect);
   new(Component);
   Component^.category:=formComponents.lbCategory.items[formComponents.lbCategory.itemIndex];
   Component^.name:=formComponents.lbComponent.Items[formComponents.lbComponent.ItemIndex];
   Component^.image:=compPic;
   new(Circuit);
   circuit^.kind:=ceComponent;// component
   circuit^.component:=Component;
   CircuitList.Add(circuit);
   FormMain.Refresh;
   cedragging:=ceComponent;
   ceDragIdx:=CircuitList.count-1;
   modified:=true;
   UpdateCaption;
   UpdateStatusBar('Adding component');
   formMain.cursor:=crCross;
end;

procedure TFormMain.AddcTag(ctext:String); // add a tag to circuit board
var
  ctag:PTtag;
  ce:PTCircuitElement;
begin
  new(ctag);
  ctag^.text:=ctext;
  ctag^.left:=0;
  ctag^.top:=toolbar1.height;
  ctag^.Color:=FontDialog1.Font.color;
  ctag^.FontName:=FontDialog1.font.name;
  ctag^.FontStyle:=FontDialog1.font.Style;
  ctag^.FontSize:=FontDialog1.font.Size;
  new(ce);
  ce^.kind:=ceTag;
  ce^.tag:=ctag;
  CircuitList.Add(ce);
  formMain.refresh;
  cedragging:=ceTag;
  ceDragIdx:=CircuitList.count-1;
  modified:=true;
  UpdateCaption;
  UpdateStatusBar('Adding tag');
  formMain.cursor:=crCross;
end;

procedure TFormMain.SaveDesign; // save actual design
begin
end;

procedure TFormMain.SaveAsDesign; // save actual desing under specified name.
begin
end;

procedure TFormMain.OpenDesign; // open an existing Design
begin
end;

procedure TFormMain.SaveBitmap; // save actual design as bitmap.
begin
end;

procedure TFormMain.PrintDesign; // print acctual design.
begin
end;

procedure TFormMain.Undo; // remove last item from list
begin
     if CircuitList.count>0 then
     begin
         FreeCircuitItem(CircuitList.count-1);
         Statusbar1.Panels[0].text:='Circuit elements'+CircuitList.count.toString;
         formMain.refresh;
         UpdateStatusBar('');
     end;
end;

procedure TFormMain.FreeCircuitItem(i:integer); // delete item from list
var
   node:PTCircuitElement;
begin
  node:=circuitList.items[i];
  case node^.kind of
     ceJumper:
     begin
       dispose(PTJumper(node^.wire));
     end;
     ceComponent:
     begin
       PTComponent(node^.component)^.image.free;
       dispose(PTComponent(node^.component));
     end;
     ceTag:
     begin
       dispose(PTtag(node^.tag));
     end;
  end; // case
  dispose(node);
  circuitList.delete(i);
  Modified:=true;
  updateCaption;
end;

procedure TFormMain.FreeCircuitList; // free Items in CircuitList
var
  i:integer;
begin
  if circuitList.count>0 then // free allocated memory
  begin
    i:=circuitList.count-1;
    while i>-1 do
    begin
      FreeCircuitItem(i);
      i:=i-1;
    end; // while
  end; // if
  UpdateStatusBar('');
end; // procedure

procedure TFormMain.MousePosition(X,Y:integer);
begin
  UpdateStatusBar(format('Jumper Color: %S, mouse: %0.D, %0.D',
       [FormColor.rgColors.items[FormColor.rgColors.Itemindex],X,Y]));
  StatusBar1.Refresh;
end;

procedure TFormMain.ToolButtonJumperClick(Sender: TObject);
begin
  FormColor.Visible:=true;
end;

procedure TFormMain.LoadProject(ProjectName:string);
var
  node:PTCirCuitElement;
  jumper:PTJumper;
  Comp:PTComponent;
  cTag:PTTag;
  ProjectFile:TIniFile;
  i:integer;
  nodeName,kind:string;
  loop:boolean;
  pic:TPicture;
  filePath:string;
begin
     newProject;
     filepath:=ExtractFilePath(ProjectName);
     ProjectFile:=TiniFile.Create(ProjectName);
     InstallProtoBoard(BoardIndexFromName(ProjectFile.ReadString('Proto board','name','medium')));
     i:=0;
     loop:=true;
     while loop do
     begin
       nodeName:='node'+i.ToString;
       kind:=ProjectFile.ReadSTring(nodeName,'kind','');
       node:=nil;
       case kind of
         '': loop:=false;
         'jumper':
              begin
                node:=new(PTCircuitElement);
                node^.kind:=ceJumper;
                jumper:=new(PTJumper);
                jumper^.color:=ProjectFile.ReadInteger(nodeName,'color',0);
                jumper^.StartPt.X:=ProjectFile.ReadInteger(nodeName,'startPt.X',0);
                jumper^.startPt.Y:=ProjectFile.ReadInteger(nodeName,'startPt.Y',0);
                jumper^.EndPt.x:=ProjectFile.ReadInteger(nodeName,'endPt.X',0);
                jumper^.EndPt.Y:=ProjectFile.ReadInteger(nodeName,'endPt.Y',0);
                node^.wire:=jumper;
              end;
          'component':
              begin
                 node:=new(PTCircuitElement);
                 node^.kind:=ceComponent;
                 comp:=new(PTComponent);
                 pic:=TPicture.create;
                 pic.LoadFromFile(FilePath+ProjectFile.ReadString(nodeName,'image',''));
                 comp^.image:=pic;
                 comp^.name:=ProjectFile.ReadString(nodeName,'name','');
                 comp^.category:=ProjectFile.ReadString(nodeName,'category','');
                 comp^.left:=ProjectFile.ReadInteger(nodeName,'left',0);
                 comp^.top:=ProjectFile.ReadInteger(nodeName,'top',0);
                 node^.component:=comp;
              end;
          'tag':
              begin
                 node:=new(PTCircuitElement);
                 node^.kind:=ceTag;
                 cTag:=new(PTTag);
                 cTag^.text:=ProjectFile.ReadString(nodeName,'text','');
                 ctag^.color:=ProjectFile.ReadInteger(nodeName,'Font color',0);
                 ctag^.FontName:=ProjectFile.ReadString(nodeName,'Font name','');
                 ctag^.FontSize:=ProjectFile.ReadInteger(nodeName,'Font size',0);
                 ctag^.FontStyle:=TFontStyles(ProjectFile.ReadInteger(nodeName,'Font style',0));
                 cTag^.left:=ProjectFile.ReadInteger(nodeName,'left',0);
                 cTag^.top:=ProjectFile.ReadInteger(nodeName,'top',0);
                 node^.tag:=cTag;
              end;
          end; // case
          i:=i+1;
          if loop and (node<>nil) then circuitList.add(node);
     end; // while loop
     ProjectFile.Destroy;
     FormSaveProject.directory:=filePath;
     formSaveProject.projectName:=ExtractFileNameOnly(ProjectName);
     UpdateCaption;
     UpdateStatusBar('Project loaded');
     formMain.formResize(self);
     formMain.refresh;
end;

procedure TFormMain.ToolButtonOpenClick(Sender: TObject);
begin
  if QuerySaveModified then
  begin
      if OpenDialog1.execute then LoadProject(OpenDialog1.fileName);
  end;
end;

procedure TFormMain.ToolButtonTagClick(Sender: TObject);
begin
  formTag.showModal;
  if length(formTag.Edit1.caption)>0 then AddcTag(String(formTag.Edit1.caption));
end;



procedure TFormMain.FormClick(Sender: TObject);
var
  mousePos:TPoint;
  node:PTCircuitElement;
  CompIdx:integer;
procedure ShowPopupMenu;
var
  i:integer;
begin
  startPt:=mousePos;
  for i:=0 to PopupMenu1.Items.Count-1 do PopupMenu1.Items[i].Enabled:=false;
  case node^.kind of
     ceJumper:
     begin
       PopupMenu1.Items[0].Enabled:=true;
       PopupMenu1.Items[1].Enabled:=true;
     end;
     ceComponent:
     begin
        PopupMenu1.Items[0].Enabled:=true;
        PopupMenu1.Items[5].Enabled:=true;
        PopupMenu1.Items[6].Enabled:=true;
       end;
     ceTag:
     begin
        PopupMenu1.Items[0].Enabled:=true;
        PopupMenu1.Items[3].Enabled:=true;
        PopupMenu1.Items[5].Enabled:=true;
        PopupMenu1.Items[6].Enabled:=true;
     end;
   end;
   PopupMenu1.PopUp(Mouse.CursorPos.x,Mouse.CursorPos.Y);
end;

procedure EndOperation;
begin
  ceDragging:=ceNone;
  ceDragIdx:=-1;
  formMain.Cursor:=crDefault;
  Modified:=true;
  UpdateCaption;
  UpdateStatusBar('');
  formMain.refresh;
end;

begin
   mousePos:=ScreenToClient(Mouse.CursorPos);
   mousePos.x:=MousePos.x-board.left;
   mousePos.y:=MousePos.y-board.top;
   if (CircuitList.count>0) and (ceDragIdx>-1) then
           node:=CircuitList.items[ceDragIdx];
   case cedragging of
      ceNone:
      begin
        Compidx:= MouseOverElement(mousePos);
         if CompIdx >-1 then
         begin  // component edition
           node:=CircuitList.items[compIdx];
           startPt:=MousePos;
           showPopupMenu;
         end
         else // begin new wire dragging
         begin
            startPt:=MousePos;
            endPt:=MousePos;
            AddJumper;
            formMain.Cursor:=crCross;
         end
      end;
      ceJumper:  //ending new wire installation
      begin
         node:=CircuitList.items[CircuitList.Count-1];
         node^.wire^.EndPt:=mousePos;
         node^.wire^.EndPt:=mousePos;
         EndOperation;
      end;
      ceComponent: // ending new component installation
      begin
         node^.component^.left:=MousePos.X;
         node^.component^.top:=MousePos.Y;
         EndOperation;
      end;
      ceTag: // ending  new tag installation
      begin
          node^.tag^.left:=MousePos.X;
          node^.tag^.top:=MousePos.Y;
          EndOperation;
      end;
   end;
end;


procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeCircuitList;
end;

procedure TFormMain.MoveComponent(comp:PTComponent;X,Y:integer);
begin
  comp^.Left:=X;
  comp^.top:=Y;
end;


procedure TFormMain.FormCreate(Sender: TObject);

var
  iniFile: TiniFile;

begin
  CircuitList:=TFPList.Create;
  StartPt.X:=0;
  StartPt.Y:=0;
  EndPt.X:=0;
  EndPt.Y:=0;
  Canvas.Pen.Width:=2;
  Canvas.Brush.Style := bsClear;
  InstallProtoBoard(bsMEDIUM);
end;




procedure TFormMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  MousePos:TPoint;
  i:integer;
  ce:PTCircuitElement;
  Comp:PTComponent;

begin
  if (button=mbRight) and (ceDragging=ceJumper) then
  begin
       ceDragging:=ceNone;
       ceDragIdx:=-1;
       freeCircuitItem(CircuitList.count-1);
       UpdateStatusbar('');
       refresh;
       exit;
  end;
  MousePos.X:=X-Board.left;
  MousePos.Y:=Y-Board.top;
  i:=MouseOverElement(MousePos);
  if (i=-1) or (PTCircuitElement(CircuitList[i])^.kind<>ceComponent) then exit;
  StartPt:=MousePos;

end;


procedure TFormMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  node:PTCircuitElement;
begin
  X:=X-self.board.left;
  Y:=Y-self.board.top;
  if (CircuitList.count>0) and (ceDragIdx>-1) then node:=CircuitList.items[ceDragIdx];
  case ceDragging of
    ceNone:
    begin
         MousePosition(X,Y);
    end;
    ceComponent:
    begin
      node^.component^.left:=X;
      node^.component^.top:=Y;
      formMain.refresh;
    end;
    ceJumper:
    begin
       node^.wire^.EndPt.X:=X;
       node^.wire^.Endpt.Y:=Y;
       formMain.refresh;
    end;
    ceTag:
    begin
       node^.tag^.left:=X;
       node^.tag^.top:=Y;
       formMain.refresh;
       end;
    end;
end;

procedure TFormMain.FormPaint(Sender: TObject);
var
  node:PTCircuitElement;
  cTag:PTTag;
  i,ctagLeft,ctagTop:integer;
  startLn,endLn:TPoint;
begin
  Canvas.Clear;
  Canvas.draw((ClientWidth-BoardImage.Width)div 2,(ClientHeight-Toolbar1.Height-BoardImage.height) div 2,BoardImage.graphic);
  if CircuitList.count>0 then
  begin
      for i:=0 to CircuitList.Count-1 do
      begin
         node:=CircuitList.items[i];
         case node^.kind of
           ceJumper:
           begin
             startLn:=node^.wire^.startpt;
             startln.X:=startln.x+self.board.left;
             startln.y:=startln.y+self.board.top;
             endLn:= node^.wire^.endpt;
             endln.X:=endln.x+self.board.left;
             endln.y:=endln.y+self.board.top;
             formMain.Canvas.Pen.color:=node^.wire^.color;
             formMain.canvas.Line(startLn,endLn);
           end;
           ceComponent:
           begin
              node^.component^.image.Bitmap.TransparentColor:=clWHITE;
              node^.component^.image.Bitmap.Transparent:=TRUE;
              formMain.Canvas.Draw(node^.component^.left+self.board.left,node^.component^.top+self.board.top,node^.component^.image.Graphic);
           end;
           ceTag:
           begin
              canvas.font.size:=node^.tag^.FontSize;
              canvas.font.style:=node^.tag^.FontStyle;
              canvas.font.name:=node^.tag^.FontName;
              canvas.font.color:=node^.tag^.color;
              canvas.TextOut(node^.tag^.left+self.board.left,node^.tag^.top+self.board.top,node^.tag^.text);
           end;
         end;

      end;
  end;
  if ceDragging=ceJumper then
  begin
    Canvas.Line(StartPt,endPt);
  end;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  self.board.left:=(self.ClientWidth-self.BoardImage.width) div 2;
  self.board.top:=(self.ClientHeight-Self.BoardImage.height) div 2;
end;

procedure TFormMain.mCloneClick(Sender: TObject);
var
  node:PTCircuitElement;
  newTag:PTTag;
  newComp:PTComponent;
  newPic:TPicture;
  picrect:Trect;
  i:integer;
begin
  i:=MouseOverElement(StartPt);
  if (i>-1) then
  begin
    node:=PTCircuitElement(CircuitList.Items[i]);
    case node^.kind of
        ceComponent:
        begin
          UpdateStatusbar('Cloning component');
          newComp:=new(PTComponent);
          newPic:=TPicture.create;
          with node^.Component^ do
          begin
             newPic.Bitmap.SetSize(image.bitmap.width,image.Bitmap.height);
             picRect:=rect(0,0,image.bitmap.width,image.Bitmap.height);
             newPic.bitmap.canvas.CopyRect(picRect,image.bitmap.canvas,picRect);
             newComp^.image:=newPic;
             newComp^.name:=name;
             newComp^.category:=category;
             newComp^.Left:=StartPt.x;
             newComp^.Top:=StartPt.y;
          end;
          node:=new(PTCircuitElement);
          node^.kind:=ceComponent;
          node^.component:=newComp;
          CircuitList.add(node);
          ceDragging:=ceComponent;
          ceDragIdx:=CircuitList.count-1;
          cursor:=crCross;
        end;
        ceTag:
        begin
           UpdateStatusbar('Cloning tag');
           newTag:=new(PTTag);
           with node^.tag^ do
           begin
              newTag^.text:=text;
              newTag^.FontSize:=FontSize;
              newTag^.Color:=Color;
              newTag^.FontStyle:=FontStyle;
              newTag^.left:=StartPt.x;
              newTag^.top:=StartPt.y;
           end;
           node:=new(PTCircuitElement);
           node^.kind:=ceTag;
           node^.tag:=newTag;
           ceDragging:=ceTag;
           CircuitList.add(node);
           ceDragIdx:=CircuitList.count-1;
           cursor:=crCross;
        end;
    end;
  end;
end;

procedure TFormMain.mColorClick(Sender: TObject);
var
  i:integer;
  node:PTCircuitElement;
begin
   i:=MouseOverElement(StartPt);
   if i>-1 then
   begin
     node:=CircuitList.Items[i];
     if node^.kind=ceJumper then
     begin
        UpdateStatusbar('Changing wire color');
        FormColor.showmodal;
        node^.wire^.color:=self.JumperColor;
        UpdateStatusBar('');
        formMain.refresh;
     end;
   end;
end;

procedure TFormMain.mDeleteClick(Sender: TObject);
var
  node:PTCircuitElement;
  i:integer;
begin
   i:=0;
   while i < CircuitList.Count do
   begin
      if OverComponent(StartPt,i) then
      begin
         FreeCircuitItem(i);
         UpdateStatusBar('Component deleted');
      end
      else
      begin
         i:=i+1;
      end;
   end;
   ceDragging:=ceNone;
   formMain.refresh;
end;

procedure TFormMain.MenuItemLibraryClick(Sender: TObject);
begin

end;

procedure TFormMain.MenuItemManualClick(Sender: TObject);
begin
  OpenDocument('.\DOCS\en\manual-en.html');
end;

procedure TFormMain.MenuLibraryClick(Sender: TObject);
begin
    FormLibrary.showmodal;
end;

{
  Printing Main form client area where the design is drawn.
  As Modern screen are wider than heigher the printing is set to
  landscape and the bitmap is scaled to occupy the largest space on the
  sheet giving 1 inch margins.
}
procedure TFormMain.MenuItemPrintClick(Sender: TObject);
var
  srect,PrintRect:TRect;
  leftMargin,TopMargin:integer; // margins
  PrintWidth,PrintHeight:integer; // printer image width,height
begin
  if PrintDialog1.execute then
  begin
       // client area rectangle
       srect:=rect(0,Toolbar1.height,ClientWidth,Clientheight-Toolbar1.height);
       with printer do
       begin
          try
            orientation:=poLandscape; // print landscape.
            BeginDoc;
            // printing area size
            LeftMargin:=XDPI;
            // left and right margins  1 inch
            PrintWidth:=PageWidth-(2*LeftMargin);
            // keep height/width ratio.
            PrintHeight:=trunc(PrintWidth*srect.height/srect.width);
            // center vertically on page.
            TopMargin:=(PageHeight-PrintHeight) div 2;
            PrintRect:=rect(LeftMargin,TopMargin,PrintWidth+LeftMargin,PrintHeight+TopMargin);
            canvas.CopyRect(PrintRect,self.canvas,srect);
          finally
            EndDoc;
          end;
       end;
  end;
end;

procedure TFormMain.mTagFontClick(Sender: TObject);
var
  i:integer;
  cTag:PTTag;
begin
  i:=MouseOverElement(StartPt);
  if i>-1 then
  begin
     UpdateStatusbar('Editing tag');
     ctag:=PTCircuitElement(CircuitList.items[i])^.tag;
     if FontDialog1.execute then
     begin
          ctag^.color:=FontDialog1.font.color;
          ctag^.FontName:=FontDialog1.font.Name;
          ctag^.FontSize:=FontDialog1.font.Size;
          ctag^.FontStyle:=FontDialog1.font.Style;
          self.refresh;
     end;
     UpdateStatusbar('');
  end;
end;


procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout.visible:=TRUE;
end;

procedure TFormMain.MenuItemBoardSelectClick(Sender: TObject);
begin
  frmBoards.Show;
  InstallProtoboard(frmBoards.RGboards.ItemIndex);
end;

procedure TFormMain.MenuItemComponentClick(Sender: TObject);
begin
  FormComponents.showModal;
end;

procedure TFormMain.MenuItemNewClick(Sender: TObject);
begin
  if (not Modified) or QuerySaveModified then  NewProject;
end;


procedure TFormMain.MenuItemPredefColorClick(Sender: TObject);
begin
  FormColor.Visible:=true;
end;



procedure TFormMain.MenuItemQuitClick(Sender: TObject);
begin
  FormMain.close();
  Application.Destroy;
end;

procedure TFormMain.MenuItemLargeBoardClick(Sender: TObject);
begin
  InstallProtoBoard(bsLARGE);
end;

procedure TFormMain.MenuItemMediumBoardClick(Sender: TObject);
begin
  InstallProtoBoard(bsMEDIUM);
end;

procedure TFormMain.SaveProject(DirName,ProjectName:string);
var
  projectFile:TIniFile;
  node:PTCircuitElement;
  i:integer;
  nameOnly:string;
begin
  nameOnly:=ExtractFileNameOnly(ProjectName);
  if not DirectoryExists(dirName) then MkDir(dirName);
  dirName:=DirName+'\';
  ProjectName:=nameOnly+'.bvp';
  projectFile:=TiniFile.Create(DirName+ProjectName);
  projectFile.WriteString('Proto board','board',FormMain.board.name);
  projectFile.WriteString('Proto board','board file',FormMain.board.fileName);
  i:=0;
  while i<CircuitList.count do
  begin
     node:=CircuitList.items[i];
     case node^.kind of
       ceJumper:
       begin
          projectFile.WriteString('node'+i.ToString,'kind','jumper');
          projectFile.WriteString('node'+i.ToString,'color',integer(node^.wire^.color).ToString);
          projectFile.WriteString('node'+i.ToString,'startPt.X',node^.wire^.StartPt.X.ToString);
          projectFile.WriteString('node'+i.ToString,'startPt.Y',node^.wire^.StartPt.Y.ToString);
          projectFile.WriteString('node'+i.ToString,'endPt.X',node^.wire^.endPt.X.ToString);
          projectFile.WriteString('node'+i.ToString,'endPt.Y',node^.wire^.endPt.Y.ToString);
       end;
       ceComponent:
       begin
          projectFile.WriteString('node'+i.ToString,'kind','component');
          ProjectFile.WriteString('node'+i.ToString,'category',node^.component^.category);
          ProjectFile.WriteString('node'+i.ToString,'name',node^.component^.name);
          ProjectFile.WriteString('node'+i.ToString,'left',node^.component^.left.ToString);
          ProjectFile.WriteString('node'+i.ToString,'top',node^.component^.top.ToString);
          ProjectFile.WriteString('node'+i.ToString,'image','component-'+i.ToString+'.bmp');
          node^.component^.image.SaveToFile(DirName+'component-'+i.ToString+'.bmp');
       end;
       ceTag:
       begin
          projectFile.WriteString('node'+i.ToString,'kind','tag');
          projectFile.WriteString('node'+i.ToString,'text',node^.tag^.text);
          projectFile.WriteString('node'+i.ToString,'left',node^.tag^.left.ToSTring);
          projectFile.WriteString('node'+i.ToString,'top',node^.tag^.top.ToString);
          projectFile.WriteString('node'+i.ToString,'Font name',node^.tag^.FontName);
          projectFile.WriteString('node'+i.ToString,'Font color',integer(node^.tag^.color).ToString);
          projectFile.WriteString('node'+i.ToString,'Font size',node^.tag^.FontSize.ToString);
          projectFile.WriteString('node'+i.ToString,'Font Style',integer(node^.tag^.FontStyle).ToString);
       end;
     end;
     i:=i+1;
  end;
  projectFile.UpdateFile;
  ProjectFile.Destroy;
  Modified:=false;
  UpdateCaption;
end;


procedure TFormMain.MenuItemSaveAsClick(Sender: TObject);
begin
     If FormSaveProject.showModal=mrYes then
        SaveProject(formSaveProject.directory,formSaveProject.projectName);
end;

procedure TFormMain.MenuItemSavebmpClick(Sender: TObject);
var
  ProjectImg:TBitmap; // use TPortableNetworkGraphic to save as PNG.
  RectFrom,RectTo:TRect;
  x,y:integer;
begin
  if SaveDialog1.execute then
  begin
     ProjectImg:=TBitmap.Create;
     ProjectImg.width:=FormMain.width;
     ProjectImg.height:=FormMain.Clientheight-toolbar1.height;
     RectFrom:=rect(0,toolbar1.height,FormMain.Clientwidth,FormMain.Clientheight-toolbar1.height);
     RectTo:=rect(0,0,FormMain.Clientwidth,FormMain.Clientheight-toolbar1.height);
     ProjectImg.canvas.copyRect(rectTo,formMain.Canvas,rectFrom);
     ProjectImg.SaveToFile(SaveDialog1.FileName);
     ProjectImg.free;
  end;
end;

procedure TFormMain.MenuItemSaveClick(Sender: TObject);
begin
   if length(formSaveProject.ProjectName)=0 then
      MenuItemSaveAsClick(self)
   else
      SaveProject(FormSaveProject.Directory,formSaveProject.ProjectName);
end;

procedure TFormMain.MenuItemSmallBoardClick(Sender: TObject);
begin
  InstallProtoBoard(bsSMALL);
end;

procedure TFormMain.mMoveClick(Sender: TObject);
var
  i:integer;
  node:PTCircuitElement;
begin
  i:=MouseOverElement(StartPt);
  if i>-1 then
  begin
    UpdateStatusbar('Moving component');
    node:=PTCircuitElement(CircuitList.items[i]);
    if  (node^.kind=ceComponent) or (node^.kind=ceTag) then
    begin
       ceDragIdx:=i;
       ceDragging:=node^.kind;
       if ceDragging=ceTag then UpdateStatusbar('Moving tag');
       cursor:=crCross;
    end;
  end;
end;


procedure TFormMain.ToolButtonCmpntClick(Sender: TObject);
begin

end;



end.


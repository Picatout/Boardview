### 2021-12-29

* In popup menu changed item **Tag Font...** by **Tag edit...** text can also be changed.

* Adapted code for Linux. 
	
	* Added conditional compiling directive for documentation path in [unithelppref.pas](unithelppref.pas). 
	```
	const
    {$IFDEF WINDOWS}
    DOCS_PATH_EN='DOCS\en\';
    DOCS_PATH_FR='DOCS\fr\';
    {$ELSE}
    DOCS_PATH_EN='DOCS/en/';
    DOCS_PATH_FR='DOCS/fr/';
    {$ENDIF}
  
	```
   
    *  canvas.FloodFill doesn't work in Linux so I added my own **FloodFill** in conditional compile for Linxux.
    ```
    // canvas.FloodFill doesn't work in Linux.
	// fill surface with canvas.brush.color remplacing color
	// The beauty of recursivity
	procedure SolidColorFloodFill(canvas:TCanvas;X,Y,color:TColor);
	begin
      with canvas do
      begin
          if (x<0)or(x>=width)or(y<0)or(y>=height)or(pixels[x,y]<>color) then exit;
          pixels[x,y]:=brush.color;
          SolidColorFloodFill(canvas,x-1,y,color);
          SolidColorFloodFill(canvas,x+1,y,color);
          SolidColorFloodFill(canvas,x,y-1,color);
          SolidColorFloodFill(canvas,x,y+1,color);
      end;
	end;
    
    ```

	*  Linux file system is case sensitive contrary to Windows. components.ini was edited to change uppercase **BMP** to lowercase.
	
	*  winres doesn't work in linux. Added Conditional compiling in boardview.lpr remove **{$R app-icon.rc}** when compiling in Linux.
	
	*  Unbutu Font dialog as no color selection for the font.  Added a **color...** button in Components and tag dialogs. This button 
	is visible only in Linux compiled version.  
	
	*  The **jumper color...** item in popup menu is now actived both for **jumper** and **tag**.
	
	* Updated manuals for revision 1.1.2
	
### 2021-12-28

*  Found How to change the application taskbar icon.  I create an [app-icon.rc](app-icon.rc) and [boardview.lpr](boardview.lpr)
added **{$R app-icon.rc}** and commented out **{$R *.res}**.  It doesn't work if the **.rc** has not the same name as project name.

*  Modified to ask save change before selecting a new board.

*  Modified project to compile on Linux as well as Windows.

*  **UnitComponents.TformComponents.rotate**  cleanup unsused code.

### 2021-12-27

* working on components dialog. Inverted rotation angle behavior. +90 is conterclockwise and -90 is clockwise.

* Modified code UnitComponents to rotate tag with component.

* Removed freeze  button un Components dialog. Now it is automatic when **OK** button is clicked. 

* Updated version 1.1 and documentation accordingly. 

* Created an installation package for version 1.1

### 2021-12-26 

* Now elements are dragged keeping the left button down instead of click to start and click again to stop. 

* Modified Element popup menu activation. Now actived by right click instead of left click.

* Modified  unitComponents.TFormComponents.lbComponentClick to add line __imgComponent.Picture.Bitmap.SetSize(picComponent.Width,picComponent.Height);__.
otherwise the whole size of the picComponent box was copied to prototyping board.


### 2021-12-25

* Code review and cleanup.

* Added code to query for save modified when quitting application.

### 2021-12-22

* Mofidief unitHelpPref to create 2 enumerated types, enumLang and enumFileType.

* Added menu item **Help-preferences** and dialog box **help preferences**.

* Created **dist** subdirectory and installation package for windows.

### 2021-12-21

* Replaced lazarus-source/DOCS/resources/Boardview-main-window.png

* Renamed this file.

* Revised lazarus-source/DOCS/en/manual.en.html

* Changed the shortcut key for **Add-Component** menu item. Now it is **&lt;CTRL+E&gt;**.

* Written french user manual.

* printed both manual in pdf files.
	
### 2021-12-20 
	
* working on [user manual](DOCS/en/manual-en.html).

* Changed application icon that appear in Main window title bar.

* Divided statusbar in 2 panels.

* Changed some behavior.
    
    

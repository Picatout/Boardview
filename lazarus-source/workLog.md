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
    
    

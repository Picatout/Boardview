# Boardview

A Windows application to design solderless prototyping board circuit.

There was an original version written in [open euphoria](https://openEuphoria.org) the source code of which was lost and 
a [new version](lazarus-source)  written using [Lazarus/free pascal IDE](https://www.lazarus-ide.org/).

![Boardview main window](lazarus-source/DOCS/resources/Boardview-main-window.png)

## 2021-12-22

I prepared a Windows setup package for the new [lazarus/free pascal version v1.0](lazarus-source/boardview-setup.exe). 

At first dialog of installation choose the option **install only for me** so the application will be installed in 
**%homepath\AppData\Local\Programs\Boardview-laz**.  Otherwise it will be install in **C:\program (x86) files\boardview-laz**  and write access will be denied. The application require write access 
in the application directory for **boardview.ini** and **component.ini** and if you want to add components in **bitmaps** subdir.
  
### français

J'ai préparé un paquet d'installation Windows pour la nouvelle version de boardview créée avec [Lazarus/free pascal IDE](https://www.lazarus-ide.org/).

Lorsque le programme d'installation démarre choississez l'option **installer seulement pour moi**. Avec cette option l'application est installée dans
  **%homepath%\AppData\Local\Programs\boardview-laz**.  Autrement l'application sera installée dans le dossier 
  **C:\Program Files (x86)\boarview-laz**. Le problème est qu'il faut des droits d'administrateur pour écrire dans ce dossier.

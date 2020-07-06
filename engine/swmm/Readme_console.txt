INSTRUCTIONS FOR COMPILING SWMM5.EXE USING MICROSOFT VISUAL C++ 2005
=====================================================================

1. Open the file SWMM5.C in a text editor and make sure that the line
       #define DLL
   is commented out.

2. Create a sub-directory named VC2005_CON under the directory where
   the SWMM 5 Engine source code files are stored and copy
   VC2005-CON.VCPROJ to it.

3. Launch Visual C++ 2005 and use the File >> Open command to open
   the VC2005-CON.VCPROJ file.

4. Issue the Build >> Configuration Manager command and select the
   Release configuration.

5. Issue the Build >> Build VC2005-CON command to build SWMM5.EXE
   (which will appear in the Release subdirectory underneath the
   VC2005-CON directory).



{ ##                  
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UImageList.pas
  @COMMENTS                 Unit that provides constants for indexing into
                            image lists and helper routines to assist in usage
                            of image lists.
  @OTHER_NAMES              + Original file name was ImageList.inc: this file
                              provided a few constants as indexes into small
                              images image list.
                            + Changed to UImageList.pas at v2.0.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 28/08/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 29/12/2002
      @COMMENTS             Changed from an include file containing constants to
                            a unit that contains an extended number of constants
                            plus a helper function for working with image lists.
                            Renamed from ImageList.inc to UImageList.pas.
                            Details of main changes are:
                            + Added new "raw" indexes into small images image
                              list: one per item. Numerous glyphs were added to
                              list.
                            + Added constants for new treeview images on run
                              programs tab sheet.
                            + Added function to return image list indices for
                              various file types.
    )
  )
}


{
 * ***** BEGIN LICENSE BLOCK *****
 * 
 * Version: MPL 1.1
 * 
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 * 
 * The Original Code is UImageList.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000-2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UImageList;


interface


uses
  // Project
  UCommonTypes;

const

  // Indices in image list for tree node glyphs

  // raw glyph indices: represent offset in small bitmap resource
  cFolderImg = 0;
  cDataFileImg = 1;
  cErrRegImg = 2;
  cIntRegImg = 3;
  cStrRegImg = 4;
  cExpStrRegImg = 5;
  cBinStrRegImg = 6;
  cExeFileImg = 7;
  cParamImg = 8;
  cTickFolderImg = 9;
  cCrossFolderImg = 10;
  {index 11 unused}
  cBatFileImg = 12;
  cErrFileImg = 13;
  cDLLFileImg = 14;
  cDOSFileImg = 15;
  {index 16 unused}
  {index 17 unused}
  {index 18 unused}
  cBombImg = 19;

  // for group tree view
  cGroupNodeImageIndex = cFolderImg;          // group node (contains files)
  cErrFileNodeImageIndex = cBombImg;          // error file node

  // for registry tree view
  cRegKeyNodeImageIndex = cFolderImg;         // registry key node
  cNameImageMap : array[0..4] of Byte = (     // various registry data nodes
    cErrRegImg,                               //   errors
    cStrRegImg,                               //   string
    cExpStrRegImg,                            //   expand string
    cIntRegImg,                               //   integer
    cBinStrRegImg                             //   binary
  );

  // for run programs tree view
  cInstallNodeImageIndex = cTickFolderImg;    // top level install node
  cUninstallNodeImageIndex = cCrossFolderImg; // top level uninstall node
  cParamNodeImageIndex = cParamImg;           // valid parameter nodes
  cBadParamNodeImageIndex = cBombImg;         // bad parameter nodes
  cBadProgNodeImageIndex = cBombImg;          // bad program nodes
  {valid program nodes use image depending on file kind: use FileKindImageIndex}


function FileKindImageIndex(const FileKind: TFileKind): Integer;
  {Returns the index of the image (small images list) representing a file of the
  given kind}


implementation


function FileKindImageIndex(const FileKind: TFileKind): Integer;
  {Returns the index of the image (small images list) representing a file of the
  given kind}
const
  cImgMap: array[TFileKind] of Integer =
  (
    -1,               // unknown file kind
    cErrFileImg,      // error file kind (used for files that don't exist)
    cDataFileImg,     // data files (non-executable)
    cBatFileImg,      // batch files: *.bat extension
    cDOSFileImg,      // DOS executable
    cExeFileImg,      // 32 bit executable
    cExeFileImg,      // 16 bit executable
    cDLLFileImg,      // 32 bit DLL
    cDLLFileImg,      // 16 bit DLL
    cDLLFileImg       // 32 bit COM DLL
  );
begin
  // Return image index of required type
  Result := cImgMap[FileKind];
end;

end.

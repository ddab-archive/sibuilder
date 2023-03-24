{ ##                      
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UEmbeddedData.pas
  @COMMENTS                 Declare the structure of header records used to
                            describe data embedded in SITools install programs.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
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
 * The Original Code is UEmbeddedData.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UEmbeddedData;


interface


type

  {
  TEmbeddedFileHeader:
    Header record used to preceed data embedded in an SITools install program.
    The record provides information about the following data.
  }
  TEmbeddedFileHeader = packed record
    FileName: string[24];     // name of file data is to be extracted to
    DataSize: LongWord;       // size of following data
    IsCompressed: Boolean;    // whether data is compressed or not
  end;


implementation


end.

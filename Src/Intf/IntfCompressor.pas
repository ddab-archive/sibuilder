{ ##                 
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     IntfCompressor.pas
  @COMMENTS                 Defines interface supported by all compressor
                            libraries used by SIBuilder.
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
 * The Original Code is IntfCompressor.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit IntfCompressor;

interface

uses
  // Delphi
  ActiveX;

type

  {
  ICompressor:
    Interface supported by all objects used by SIBuilder to compress data: the
    interface must be supported by all plug-in compressor libraries.

    Inheritance: ICompressor -> [IUnknown]
  }
  ICompressor = interface
    ['{1A68B9B3-9DB6-4F80-911C-82EEEF44A08B}']
    function Compress(const SrcStm, DestStm: IStream;
      const Count: Integer): Integer; stdcall;
      {Compress count bytes from SrcStm onto DestStm and return size of
      compressed stream}
  end;


implementation

end.

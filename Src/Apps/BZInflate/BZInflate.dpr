{ ##
  @FILE                     BZInflate.dpr
  @COMMENTS                 Main project file for DLL used to inflate project
                            data that compressed was compressed using BZLib.
  @DEPENDENCIES             Requires a modified version of the BZLib interface
                            unit and BZLib compression/decompression softrware
                            (v0.9.5d code by Julian Seward).
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 24/11/2005
      @COMMENTS             Updated to use revised unit that interfaces to BZip2
                            compression library: unit is now BZip2Mod rather
                            than BZip2Intf.
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
 * The Original Code is BZInflate.dpr.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2005 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


library BZInflate;

uses
  // Delphi
  Windows,
  // 3rd Party
  BZip2Mod;

{$Resource VBZInflate.res}  // Resource file containing version information

function BZip2InflateToFile(const FileName: PChar; CompBuf: Pointer;
  CompBufSize: Integer): BOOL; cdecl;
  {Inflates the BZip2 compressed data pointed to by CompBuf of size CompBufSize
  and stores the inflated data in the file whose name is pointed to by FileName.
  Returns true if inflation and file creation succeeds and false on failure}
var
  OutFile: File;          // Pascal untyped file used to write data
  DecompBuf: Pointer;     // pointer to decompressed data
  DecompBufSize: Integer; // size of decompressed buffer
begin
  // Create output file
  AssignFile(OutFile, String(FileName));
  try
    try
      Rewrite(OutFile, 1);
      // Decompress data into DecompBuf (BZDecompressBuf allocates it)
      BZip2Mod.BZDecompressBuf(
        CompBuf, CompBufSize, 0, DecompBuf, DecompBufSize
      );
      try
        // Write data to file
        BlockWrite(OutFile, DecompBuf^, DecompBufSize);
      finally
        // Free memory used for decompression buffer
        FreeMem(DecompBuf, DecompBufSize);
      end;
    finally
      // Close the file
      CloseFile(OutFile);
    end;
    // Everything's OK: return true
    Result := True;
  except
    // There was an error: return false
    Result := False;
  end;
end;

exports
  // We export BZip inflator function as InflateToFile function expected by
  // callers
  BZip2InflateToFile name 'InflateToFile';

begin
end.

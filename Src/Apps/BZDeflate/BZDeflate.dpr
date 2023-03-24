{ ##
  @FILE                     BZDeflate.dpr
  @COMMENTS                 Main project file for DLL used to compress data
                            using BZip2 compression. Exports a function that
                            creates a compressor object that supports
                            ICompressor.
  @DEPENDENCIES             Requires a modified version of the BZLib interface
                            unit and BZLib compression / decompression software
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
 * The Original Code is BZDeflate.dpr.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2005 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


library BZDeflate;

uses
  // Delphi
  Windows, ActiveX,
  // 3rd Party
  BZip2Mod,
  // Project files
  IntfCompressor in '..\..\Intf\IntfCompressor.pas';

{$Resource VBZDeflate.res}  // Resource file containing version information

type

  {
  TBZip2Compressor:
    Class that provides ability to compress a stream using BZip2. Implements
    ICompressor for exporting from DLL.

    Inheritance: TBZip2Compressor -> [TInterfacedObject] -> [TObject]
  }
  TBZip2Compressor = class(TInterfacedObject, ICompressor)
  protected
    { ICompressor }
    function Compress(const SrcStm, DestStm: IStream;
      const Count: Integer): Integer; stdcall;
      {Try to compress SrcStm onto DestStm and return size of compressed stream}
  end;


{ TBZip2Compressor }

function TBZip2Compressor.Compress(const SrcStm, DestStm: IStream;
  const Count: Integer): Integer;
  {Try to compress SrcStm onto DestStm and return size of compressed stream}
var
  SrcBuf: Pointer;  // pointer to buffer used to store contents of source stream
  DestBuf: Pointer; // pointer to buffer used to received compressed data
begin
  // Allocate buffer to hold data from source stream
  GetMem(SrcBuf, Count);
  try
    // Read from source stream into source buffer
    SrcStm.Read(SrcBuf, Count, nil);
    // Compress stream into DestBuf (CompressBuf allocates)
    BZip2Mod.BZCompressBuf(SrcBuf, Count, DestBuf, Result);
    try
      // Write compressed data to destination stream
      DestStm.Write(DestBuf, Result, nil);
    finally
      // Free buffers
      FreeMem(DestBuf);
    end;
  finally
    FreeMem(SrcBuf);
  end;
end;

function CreateIntfObject(IID: TGUID; out Obj): HResult;
  {Creates an object that implements the interface given given IID and,
  providing IID is ICompressor, sets Obj to the object and returns S_OK. If IID
  is not ICompressor then E_NOINTERFACE is returned and Obj is set to nil}
var
  Compressor: ICompressor; // object instance
begin
  // Set Obj to nil in case of failure
  Pointer(Obj) := nil;
  if IsEqualIID(IID, ICompressor) then
  begin
    // We support ICompressor: so create instance and return in Obj
    Compressor := TBZip2Compressor.Create;
    Result := Compressor.QueryInterface(IID, Obj);
  end
  else
    // Don't support given interface
    Result := E_NOINTERFACE;
end;

exports
  // Create instance of compressor object
  CreateIntfObject;

begin
end.

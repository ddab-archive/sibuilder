{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UStreamWrapper.pas
  @COMMENTS                 This unit defines the TStreamWrapper class. This is
                            a base class for descendant classes that "wrap" a
                            TStream class to provide some form of filter or
                            additional functionality. The wrapped TStream is
                            used to do physical i/o. TStreamWrapper simply
                            replicates the facilities in the wrapped stream - it
                            is for descendant classes to add functionality.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
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
 * The Original Code is UStreamWrapper.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UStreamWrapper;

interface

uses
  // VCL
  Classes;

type
  TStreamWrapper = class(TStream)
    {Base class for descendant classes that "wrap" a TStream class to provide
    some form of filter or additional functionality. The wrapped TStream is used
    to do physical i/o. This base class simply replicates the facilities in the
    wrapped stream - it is for descendant classes to add functionality.
    TStreamWrapper and its decendants can optionally free the wrapped class when
    they themselves are freed.
    Wrapping a TStream rather than adding functionality by extending the class
    means that the functionality provided by the wrapper class can be applied to
    any TStream descendant}
  private
    FBaseStream: TStream;
      {The stream which the typed stream operates on}
    FCloseStream: Boolean;
      {Flag giving whether base stream is freed when this class is freed}
  protected
    procedure SetSize(NewSize: Longint); override;
      {Sets the size of the stream to the given value if the operation is
      supported by the underlying stream}
    property BaseStream: TStream read FBaseStream;
      {Gives access to the underlying stream to descended classes}
  public
    constructor Create(const Stream: TStream;
      const CloseStream: Boolean = False); virtual;
      {Creates a TWrapperStream object that uses the given stream. If
      CloseStream is true the given underlying stream is freed when this object
      is freed}
    destructor Destroy; override;
      {Frees underlying stream if user specified CloseStream as True when this
      object was created}
    function Read(var Buffer; Count: Longint): Longint; override;
      {Reads Count bytes from underlying stream into given buffer}
    function Write(const Buffer; Count: Longint): Longint; override;
      {Writes Count bytes from Buffer to underlying stream}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
      {Moves the underlying stream's position to Offset bytes from the given
      origin - see TStream for values of Origin. This operation may cause an
      exception if the underlying stream does not support Seek}
  end;

implementation

{ TStreamWrapper }

constructor TStreamWrapper.Create(const Stream: TStream;
  const CloseStream: Boolean);
  {Creates a TWrapperStream object that uses the given stream. If CloseStream is
  true the given underlying stream is freed when this object is freed}
begin
  inherited Create;
  // Record reference to underlying base stream - used for i/o operations
  FBaseStream := Stream;
  // Record whether base stream is to be freed when this object is freed
  FCloseStream := CloseStream;
end;

destructor TStreamWrapper.Destroy;
  {Frees underlying stream if user specified CloseStream as True when this
  object was created}
begin
  // Close base stream if user specified this should be done
  if FCloseStream then
    FBaseStream.Free;
  inherited Destroy;
end;

function TStreamWrapper.Read(var Buffer; Count: Integer): Longint;
  {Reads Count bytes from underlying stream into given buffer}
begin
  // Simply call underlying stream's Read method
  Result := FBaseStream.Read(Buffer, Count);
end;

function TStreamWrapper.Seek(Offset: Integer; Origin: Word): Longint;
  {Moves the underlying stream's position to Offset bytes from the given origin
  - see TStream for values of Origin. This operation may cause an exception if
  the underlying stream does not support Seek}
begin
  // Simply call the equivalent method in the underlying stream
  Result := FBaseStream.Seek(Offset, Origin);
end;

procedure TStreamWrapper.SetSize(NewSize: Integer);
  {Sets the size of the stream to the given value if the operation is supported
  by the underlying stream}
begin
  // Set the size property of the underlying stream
  FBaseStream.Size := NewSize;
end;

function TStreamWrapper.Write(const Buffer; Count: Integer): Longint;
  {Writes Count bytes from Buffer to underlying stream}
begin
  // Simply call the same method in the underlying stream
  Result := FBaseStream.Write(Buffer, Count);
end;

end.

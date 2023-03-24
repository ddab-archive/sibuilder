{ ##                       
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UInstExcept.pas
  @COMMENTS                 This unit hooks into Delphi's exception handling
                            code and provides a lightweight exception class for
                            use by install programs.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 29/12/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 23/02/2008
      @COMMENTS             Replaced string literals with resource strings.
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
 * The Original Code is UInstExcept.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UInstExcept;

interface

type

  {
  TError:
    Lighweight class used for exceptions raised by installer programs. This
    class is used to prevent the SysUtils overhead that is required to use the
    Exception class.
  }
  TError = class(TObject)
  private // properties
    fMsg: string;
  public
    constructor Create(const Msg: string);
      {Class constructor: records given message}
    property Msg: string read fMsg;
      {The message associated with the exception}
  end;

  {
  TFatalErrorMessageProc:
    Type of procedure used by exception handler to display fatal error messages:
    if there is no such procedure then exception handler simply closes program
    without a message.
  }
  TFatalErrorMessageProc = procedure(const Msg: string);

var

  {
  Global variable stores pointer to routine used to display fatal error messages
  by the default exception handler.
  }
  FatalErrorMessageProc: TFatalErrorMessageProc = nil;


implementation


resourcestring
  // Error messages
  sRunTimeError = 'Run time error: ';
  sIOError = 'I/O error: ';
  sUnknownException = 'Unknown exception';


procedure ErrorHandler(ErrorCode: Integer; ErrorAddr: Pointer);
  {Handles RTL errors and converts them into TError exceptions}
var
  IORes: Integer; // the current IOResult for that type of error
  Msg: string;    // used to build up error message
  ErrNum: string; // string representation of error number
begin
  case ErrorCode of
    1..23:
    begin
      // Actual RTL errors: create message including number
      Str(ErrorCode, ErrNum);
      Msg := sRunTimeError + ErrNum;
    end;
    else
    begin
      // IO error: create message including IO error number
      IORes := IOResult;
      Str(IORes, ErrNum);
      Msg := sIOError + ErrNum;
    end;
  end;
  // Raise exception with the message we've constructed
  raise TError.Create(Msg) at ErrorAddr;
end;

procedure ExceptHandler(ExceptObject: TObject; ExceptAddr: Pointer); far;
  {Handles untrapped exceptions for programs where this unit is used: displays
  error message using any assigned error display routine and halts program}
begin
  if Assigned(FatalErrorMessageProc) then
  begin
    if ExceptObject is TError then
      // we have a TError exception: display its message
      FatalErrorMessageProc(TError(ExceptObject).Msg)
    else
      // we have an unknown exception: so so
      FatalErrorMessageProc(sUnknownException);
  end;
  // Close the program
  Halt(1);
end;

{ TError }

constructor TError.Create(const Msg: string);
  {Class constructor: records given message}
begin
  inherited Create;
  fMsg := Msg;
end;

initialization

// Hook in our exception and run time error handlers into Delphi
ExceptProc := @ExceptHandler;
ErrorProc := @ErrorHandler;

finalization

// Remove our custom handlers
ExceptProc := nil;
ErrorProc := nil;

end.


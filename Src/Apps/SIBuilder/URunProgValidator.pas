{ ##                 
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     URunProgValidator.pas
  @COMMENTS                 Implements a class that can validate both the
                            programs to be run by installer and the parameters
                            passed to the programs. Ensures that no temporary
                            files are referenced or run by the uninstaller.
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
 * The Original Code is URunProgValidator.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2002 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit URunProgValidator;


interface


uses
  // Delphi
  Classes,
  // Project
  UCommonTypes, URunProgs;


type

  {
  TRunProgValidator:
    Class that can validate both the programs to be run by installer and the
    parameters passed to the programs. Ensures that no temporary files are
    referenced or run by the uninstaller.

    Inheritance: TRunProgValidator -> [TObject]
  }
  TRunProgValidator = class(TObject)
  private
    fTempFiles: TStrings;
      {Reference to list of temporary files currently in installation project.
      Contains pointers to related TFileInfo object in Objects[] field}
  public
    constructor Create(TempFiles: TStrings);
      {Class constructor: records reference to given temporary files list}
    function ValidateProg(RunProg: TRunProg; HaveUninstaller: Boolean): Boolean;
      {Validate the given program and its parameters, taking into account if
      an uninstaller is installed in the project and returns true if program and
      parameters are valid}
    function ValidateProgOnly(RunProg: TRunProg;
      HaveUninstaller: Boolean): Boolean; overload;
      {Validates the given program, taking into account if an uninstaller is
      included in the project without checking the parameters and returns true
      if program is valid}
    function ValidateProgOnly(const Prog: string;
      WhenRun: TRunSchedule; HaveUninstaller: Boolean): Boolean; overload;
      {Validates the given program, taking account of when it is run (installer
      or uninstaller or both) and if an uninstaller is included in project
      but ignores program's parameters. Returns true is program is valid}
    function ValidateParam(RunProg: TRunProg; ParamIdx: Integer): Boolean;
      overload;
      {Validates the parameter at the given index in the parameter list of the
      given program and returns true if OK}
    function ValidateParam(const Param: string; WhenRun: TRunSchedule): Boolean;
      overload;
      {Validates the given program parameter that is passed to a program that is
      executed at given time(s) - i.e. by installer, uninstaller or both.
      Returns true if parameter is valid}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UFileProcs;


{ TRunProgValidator }

constructor TRunProgValidator.Create(TempFiles: TStrings);
  {Class constructor: records reference to given temporary files list}
begin
  inherited Create;
  fTempFiles := TempFiles;
end;

function TRunProgValidator.ValidateParam(RunProg: TRunProg;
  ParamIdx: Integer): Boolean;
  {Validates the parameter at the given index in the parameter list of the given
  program and returns true if OK}
begin
  // Simply pass off processing to overloaded method
  Result := ValidateParam(RunProg.Params[ParamIdx], RunProg.WhenRun);
end;

function TRunProgValidator.ValidateParam(const Param: string;
  WhenRun: TRunSchedule): Boolean;
  {Validates the given program parameter that is passed to a program that is
  executed at given time(s) - i.e. by installer, uninstaller or both. Returns
  true if parameter is valid}
var
  UpParam: string;        // parameter in upper case
  TempFileIdx: Integer;   // loops thru all temporary files
  TempFileName: string;   // name of a temporary file
  StartPos: Integer;      // start position of temp file name in parameter
begin
  // Assume all is OK
  Result := True;
  // Check we're running on uninstaller: all OK if just installer
  if WhenRun in [rsdOnUninstall, rsdBoth] then
  begin
    // Store upper case version of param for later checking
    UpParam := AnsiUpperCase(Param);
    // Loop thru all temporary files, check to see if contained in parameter
    for TempFileIdx := 0 to Pred(fTempFiles.Count) do
    begin
      // Check of file name occurs in param
      TempFileName := AnsiUpperCase(fTempFiles[TempFileIdx]);
      StartPos := AnsiPos(TempFileName, UpParam);
      if StartPos > 0 then
      begin
        // Temp file name is param string:
        // we consider it to be a true file reference if it is either:
        //    last item in parameter
        //    separated from following text by some suitable separator
        if StartPos + Length(TempFileName) - 1 = Length(Param) then
          Result := False       // file name ends at end of string
        else
        begin
          case UpParam[StartPos + Length(TempFileName)] of
            #0..#32, ',', '.', ';', ':', '|', '/', '?', '@', '!', '$', '%', '^',
            '&', '*', '+', '{', '[', '<', '>':
              // found one of likely delimiters so we recognise file name
              Result := False;
            else
              // followed by another char considered not to be valid separator
              //   this is probably not file name
              {Do nothing};
          end;
        end;
        if not Result then
          // we found file: once is enough so get out of loop
          Break;
      end;
    end;
  end;
end;

function TRunProgValidator.ValidateProgOnly(RunProg: TRunProg;
  HaveUninstaller: Boolean): Boolean;
  {Validates the given program, taking into account if an uninstaller is
  included in the project without checking the parameters and returns true if
  program is valid}
begin
  // Call another overloaded method to do validation
  Result := ValidateProgOnly(
    MakePathName(RunProg.ExecPath) + RunProg.ProgName,
    RunProg.WhenRun,
    HaveUninstaller
  );
end;

function TRunProgValidator.ValidateProg(RunProg: TRunProg;
  HaveUninstaller: Boolean): Boolean;
  {Validate the given program and its parameters, taking into account if
  an uninstaller is installed in the project and returns true if program and
  parameters are valid}
var
  Idx: Integer; // loops thru all parameters in program
begin
  // Validate the program
  Result := ValidateProgOnly(RunProg, HaveUninstaller);
  if Result then
  begin
    // Validate all parameters
    for Idx := 0 to Pred(RunProg.Params.Count) do
    begin
      Result := ValidateParam(RunProg, Idx);
      if not Result then
        Break;
    end;
  end
  else
    Result := False;
end;

function TRunProgValidator.ValidateProgOnly(const Prog: string;
  WhenRun: TRunSchedule; HaveUninstaller: Boolean): Boolean;
  {Validates the given program, taking account of when it is run (installer
  or uninstaller or both) and if an uninstaller is included in project but
  ignores program's parameters. Returns true is program is valid}
begin
  // Assume OK
  Result := True;
  // Check if program is run by unstallaer
  if WhenRun in [rsdOnUninstall, rsdBoth] then
  begin
    // Run by uninstaller so program must not be temporary file
    if not HaveUninstaller or (fTempFiles.IndexOf(Prog) >= 0) then
      Result := False;
  end;
end;

end.

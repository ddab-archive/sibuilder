{
 * UOS.pas
 *
 * Unit provides routines for checking the underlying OS, encoding it, checking
 * if underlying OS matches a given code and describing the OS. It also declares
 * some useful constants for manipulating OS encodings.
 *
 * v1.0 of 29 Dec 2002  - Original version.
 * v1.1 of 10 Apr 2008  - Removed Win32s and added Vista.
 *
 *
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
 * The Original Code is UOS.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UOS;


interface


const

  // Operating system identification constants

  // Windows 9x
  cWin95                = $00000100;  // Windows 95
  cWin95OSR2            = $00000200;  // Windows 95 OSR2
  cWin98                = $00000400;  // Windows 98
  cWin98SE              = $00000800;  // Windows 98 SE
  cWinMe                = $00001000;  // Windows Me
  cUnknownWin9x         = $0000FF00;  // Unknown Windows 9x OS
  cAnyWin9xPlatform     = $0000FF00;  // Any Windows 9x platform OS

  // Windows NT
  cWinNT351             = $00010000;  // Windows NT 3.51
  cWinNT4               = $00020000;  // Windows NT 4
  cWin2000              = $00040000;  // Windows 2000
  cWinXP                = $00080000;  // Windows XP
  cWinVista             = $00100000;  // Windows Vista
  cUnknownWinNT         = $00FF0000;  // Unknown Windows NT OS
  cAnyWinNTPlatform     = $00FF0000;  // Any Windows NT platform OS

  // Any OS
  cAnyOS                = $00FFFFFF;  // Any Windows 32 bit OS

  // We use $00000000 to represent any OS at all


{ Public functions }

function EncodeSystemOS: LongWord;
  {Checks underlying operating system
    @return Relevant OS code to identify.
  }

function IsMatchingOS(SystemOS, PermittedOSs: LongWord): Boolean;
  {Returns true if an OS belongs to a set of permitted OSs.
    @param SystemOS [in] OS to be tested.
    @param PermittedOS [in] Set of permitted OSs.
    @return True if SystemOS is included in PermittedOSs.
  }

function OSCodeToStr(Code: LongWord): string;
  {Describes the identified operating system.
    @param Code [in] Identifies the operating system.
    @return Name of operating system.
  }

function DescribeOSs(OSs: LongWord): string;
  {Returns a comma separated list of operating systems/platforms.
    @param OSs [in] Identifies set of OSs.
    @return Comma separated list of operating systems.
  }


implementation


uses
  // Delphi
  SysUtils, Windows;

resourcestring
  // OS descriptions
  sAnyWin9xPlatform   = 'Windows 9x Platform';
  sWin95              = 'Windows 95';
  sWin95OSR2          = 'Windows 95 OSR2';
  sWin98              = 'Windows 98';
  sWin98SE            = 'Windows 98 SE';
  sWinMe              = 'Windows Me';
  sAnyWinNTPlatform   = 'Windows NT Platform';
  sWinNT351           = 'Windows NT 3.51';
  sWinNT4             = 'Windows NT 4';
  sWin2000            = 'Windows 2000';
  sWinXP              = 'Windows XP';
  sWinVista           = 'Windows Vista';
  sAnyOS              = 'Any Windows OSs';
  // Error strings
  sUnknownOS          = 'Unknown OS code';


const
  // Lookup table of OS codes and there associated descriptions
  cOSDescs: array[1..13] of record
    Code: LongWord;   // operating system code
    Desc: string;     // description of operating system
  end =
  (
    (Code: cWin95;              Desc: sWin95;),
    (Code: cWin95OSR2;          Desc: sWin95OSR2;),
    (Code: cWin98;              Desc: sWin98;),
    (Code: cWin98SE;            Desc: sWin98SE;),
    (Code: cWinMe;              Desc: sWinMe;),
    (Code: cWinNT351;           Desc: sWinNT351;),
    (Code: cWinNT4;             Desc: sWinNT4;),
    (Code: cWin2000;            Desc: sWin2000;),
    (Code: cWinXP;              Desc: sWinXP;),
    (Code: cWinVista;           Desc: sWinVista),
    (Code: cAnyOS;              Desc: sAnyOS;),
    (Code: cAnyWin9xPlatform;   Desc: sAnyWin9xPlatform;),
    (Code: cAnyWinNTPlatform;   Desc: sAnyWinNTPlatform;)
  );

function EncodeSystemOS: LongWord;
  {Checks underlying operating system
    @return Relevant OS code to identify.
  }
begin
  case SysUtils.Win32Platform of
    VER_PLATFORM_WIN32_NT:
    begin
      // We have Windows NT platform: identify OS
      if (SysUtils.Win32MajorVersion = 3)
        and (SysUtils.Win32MinorVersion = 51) then
        Result := cWinNT351
      else if (SysUtils.Win32MajorVersion = 4) then
        Result := cWinNT4
      else if (SysUtils.Win32MajorVersion = 5)
        and (SysUtils.Win32MinorVersion = 0) then
        Result := cWin2000
      else if (SysUtils.Win32MajorVersion = 5)
        and (SysUtils.Win32MinorVersion = 1) then
        Result := cWinXP
      else if (SysUtils.Win32MajorVersion = 6) then
        Result := cWinVista
      else
        Result := cUnknownWinNT;
    end;
    VER_PLATFORM_WIN32_WINDOWS:
    begin
      // We have Windows 9x platform: identify OS
      if (SysUtils.Win32MajorVersion = 4)
        and (SysUtils.Win32MinorVersion = 0) then
      begin
        // a Win 95 release: check if original or OSR2
        if (SysUtils.Win32CSDVersion <> '')
          and (
            (SysUtils.Win32CSDVersion[1] = 'C')
            or (SysUtils.Win32CSDVersion[1] = 'B')
          ) then
          Result := cWin95OSR2
        else
          Result := cWin95;
      end
      else if (SysUtils.Win32MajorVersion = 4)
        and (SysUtils.Win32MinorVersion = 10) then
      begin
        // a Win 98 release: check if original or Windows 98 SE
        if (SysUtils.Win32CSDVersion <> '')
          and (SysUtils.Win32CSDVersion[1] = 'A') then
          Result := cWin98SE
        else
          Result := cWin98;
      end
      else if (SysUtils.Win32MajorVersion = 4)
        and (SysUtils.Win32MinorVersion = 90) then
        // Win Me
        Result := cWinMe
      else
        // unknown
        Result := cUnknownWin9x;
    end;
    else
      Result := 0;
  end;
end;

function IsMatchingOS(SystemOS, PermittedOSs: LongWord): Boolean;
  {Returns true if an OS belongs to a set of permitted OSs.
    @param SystemOS [in] OS to be tested.
    @param PermittedOS [in] Set of permitted OSs.
    @return True if SystemOS is included in PermittedOSs.
  }
begin
  if PermittedOSs = 0 then
    Result := True
  else
    Result := SystemOS and PermittedOSs = SystemOS;
end;

function OSCodeToStr(Code: LongWord): string;
  {Describes the identified operating system.
    @param Code [in] Identifies the operating system.
    @return Name of operating system.
  }
var
  Idx: Integer; // loops thru OS descriptions
begin
  // Assume failure
  Result := sUnknownOS;
  // Search table of OS descriptions
  for Idx := Low(cOSDescs) to High(cOSDescs) do
    if Code = cOSDescs[Idx].Code then
    begin
      Result := cOSDescs[Idx].Desc;
      Break;
    end;
end;

function DescribeOSs(OSs: LongWord): string;
  {Returns a comma separated list of operating systems/platforms.
    @param OSs [in] Identifies set of OSs.
    @return Comma separated list of operating systems.
  }

  // ---------------------------------------------------------------------------
  procedure AppendToResult(const Str: string);
    {Appends a string to parent function's result.
      @param Str [in] String to be appended.
    }
  begin
    if Result = '' then
      Result := Str
    else
      Result := Result + ', ' + Str;
  end;
  // ---------------------------------------------------------------------------

var
  Idx: Integer;     // loops thru OS description table
  ExcOSs: LongWord; // bitmask of operating systems to be excluded from output
                    // this bitmask is used to prevent individual OSs being
                    // named when the whole of their platform is being used
begin
  // Initialise result ready for list to be built up
  Result := '';
  ExcOSs := 0;
  if (OSs = 0) or (OSs = cAnyOS) then
  begin
    // Any operating system: do not update string further so exclude all OSs
    Result := OSCodeToStr(cAnyOS);
    ExcOSs := cAnyOS;
  end
  else
  begin
    if OSs and cAnyWin9xPlatform = cAnyWin9xPlatform then
    begin
      // all of Win9x platform included: so we don't specify any individual
      // Win9x OSs
      AppendToResult(OSCodeToStr(cAnyWin9xPlatform));
      ExcOSs := ExcOSs or cAnyWin9xPlatform;
    end;
    if OSs and cAnyWinNTPlatform = cAnyWinNTPlatform then
    begin
      // all of NT platform included: so we don't specify any individual NT OSs
      AppendToResult(OSCodeToStr(cAnyWinNTPlatform));
      ExcOSs := ExcOSs or cAnyWinNTPlatform;
    end;
  end;
  // We have now noted if we are using all OSs or if any whole platforms are
  // included: exclusion bitmap will prevent any individual OSs from being
  // listed. Now go thru individual OSs listing any not excluded
  for Idx := Low(cOSDescs) to High(cOSDescs) do
  begin
    if (cOSDescs[Idx].Code and ExcOSs = 0)
      and (cOSDescs[Idx].Code and OSs = cOSDescs[Idx].Code) then
      if Result = '' then
        Result := cOSDescs[Idx].Desc
      else
        Result := Result + ', ' + cOSDescs[Idx].Desc;
  end;
  // Add full stop to end of list
  if Result <> '' then
    Result := Result + '.';
end;

end.


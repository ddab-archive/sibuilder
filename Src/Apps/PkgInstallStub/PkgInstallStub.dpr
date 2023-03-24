{
 * PkgInstallStub.dpr
 *
 * Project file for the SITools "package" based installer. This console
 * application stub understands how to extract data attached to it to perform
 * program installation. Required data is attached to this stub by SIBuilder.
 *
 * v1.0 of 29 Dec 2002  - Original version.
 * v1.1 of 16 Jan 2006  - Added UFileProcsLite and UInstallerFiles units.
 *                      - Freed Extractor class after use.
 * v1.2 of 19 Feb 2008  - Added UFileNames unit.
 *                      - Moved UInstallBody, UExtractor and UInstExcept units
 *                        from shared files to project directory.
 * v1.3 of 11 Apr 2008  - Added ISResource.res resource file containing Vista
 *                        manifest
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
 * The Original Code is PkgInstallStub.dpr.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


program PkgInstallStub;


{$APPTYPE CONSOLE}                // makes this a console application
{$RESOURCE VPkgInstallStub.res}   // version information
{$RESOURCE ISResources.res}       // other resources

uses
  Windows,
  UExtractor in 'UExtractor.pas',
  UInstallBody in 'UInstallBody.pas',
  UInstExcept in 'UInstExcept.pas',
  UPackageExtractor in 'UPackageExtractor.pas',
  UEmbeddedData in '..\..\Shared\Install\UEmbeddedData.pas',
  UInflater in '..\..\Shared\Install\UInflater.pas',
  UInstallerFiles in '..\..\Shared\Install\UInstallerFiles.pas',
  UInstProcs in '..\..\Shared\Install\UInstProcs.pas',
  UFileNames in '..\..\Shared\Params\UFileNames.pas',
  UPayload in '..\..\Shared\Install\UPayload.pas',
  UFileProcsLite in '..\..\Shared\Utils\UFileProcsLite.pas';

var
  Extractor: TExtractor;          // object used to extract program data

begin
  // Create extractor object capable of extracting program data from attached
  // payload data
  Extractor := TPackageExtractor.Create(ParamStr(0));
  try
    // Perform the installation using the extractor object
    UInstallBody.Install(Extractor);
  finally
    Extractor.Free;
  end;
end.


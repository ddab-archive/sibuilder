{ ##
  @FILE                     SIBuilder.dpr
  @COMMENTS                 A windows application that allows user to create and
                            edit installation programs.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 04/09/2000
      @COMMENTS             Significant change since many new source files have
                            been added, some removed and some moved. New files
                            added were:
                            + FmFileEdit.pas
                            + FmProgEdit.pas
                            + FmConvertFiles.pas
                            + ..\..\Shared\XML\UXMLTag.pas
                            + ..\..\Shared\XML\UXMLFile.pas
                            + ..\..\Shared\XML\UTokeniser.pas
                            + ..\..\Shared\XML\UCharStreamReader.pas
                            + ..\..\Shared\XML\UCharStreamWriter.pas
                            + UTabSheetHandler
                            + UDetailsSheetHandler.pas
                            + UGroupsTabHandler.pas
                            + URunTabHandler.pas
                            + URegistryTabHandler.pas
                            + UBuildTabHandler.pas
                            + UProjFileManager.pas
                            + UProjFileVerManager.pas
                            + USIBConverter.pas
                            + UActiveRTFWrapper.pas
                            + UCtrlState.pas
                            + URunProgs.pas
                            + ..\..\Shared\Params\UCommonTypes.pas
                            + RTF.res \
                            In addition the following files were removed:
                            + UProjectBuilder.pas
                            + Welcome.res\
                            and these files were moved to the SIBuilder source
                            folder from ..\..\Shared\SIPrj\:
                            + UProject.pas (this was renamed from
                              UBaseProject.pas)
                            + UFiles.pas
                            + UGroups.pas
                            + URegData.pas
                            + URegKeys.pas
                            + URegRootKeys.pas
                            + URegObjects.pas\
                            Finally, ..\Shared\List\StreamableObjects.pas was
                            renamed as ..\..\Shared\List\UXMLObjects.pas
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 20/12/2000
      @COMMENTS             + Added code to detect whether any 32 bit version of
                              Delphi is present and terminates program with
                              error message if so
                            + Added new UDCC32Info unit.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 28/12/2000
      @COMMENTS             Added new units re support for COM server
                            registration:
                            + FmCOMServers
                            + FmInstallFileName
                            + UCOMServers
    )
    @REVISION(
      @VERSION              2.3
      @DATE                 06/01/2001
      @COMMENTS             Added new unit: FmCOMServer.pas
    )
    @REVISION(
      @VERSION              2.4
      @DATE                 21/01/2001
      @COMMENTS             Added new units relating to support for license
                            dialog boxes:
                            + FmConfigLicense
                            + ULicenseInfo
                            + IntfSITLicenseDlg
                            + UInstallReport
                            + ULicenseDlgHelp
    )
    @REVISION(
      @VERSION              2.5
      @DATE                 25/06/2001
      @COMMENTS             Removed unit: FmChooseFolderDlg.pas
    )
    @REVISION(
      @VERSION              3.0
      @DATE                 29/12/2002
      @COMMENTS             Significant changes with addition of new units and
                            changes to project source as follows:
                            + Added new units: UOS, FmOSOptions,
                              UCompressionMgr, IntfCompressor, UBuilders,
                              UPayloadStream, UPayload, UEmbeddedData, FmAbout,
                              UOSTabHandler, UOSTreeViewMgr, UCmdDispatcher,
                              UInfoWdw, UBinStrings, URegUtils, URegEditFile,
                              UUnicodeConvStringList, FmRegTemplates,
                              FmRegMacroEdit, FmSimpleEdit, URegTemplates,
                              UDataWriter, UMessageBox, URunProgValidator,
                              UWarner, UFileValidator, USIP2Converter,
                              FmSIP2ConvMsg.
                            + Added new UImageList unit that replaces previous
                              ImageList.inc include file.
                            + Renamed some existing units: UWelcomeTabHandler
                              (replacing FmWelcomeDlg), ULicenseTabHandler
                              (replacing FmConfigLicense).
                            + Removed units: FmCOMServers, FmCOMServer,
                              UCOMServers.
                            + Renamed UCtrlState to UGUIHelper.
                            + All resources (except version info) are now in
                              Resources.res: so references to Images.res and
                              RTF.res were removed.
                            + Removed code that tests whether Delphi is present:
                              we have native packaging compiler now, so
                              SIBuilder can always run regardless of
                              availability of Delphi.
                            + Changed title from "SIBuilder for Delphi" to
                              "SIBuilder".
    )
    @REVISION(
      @VERSION              3.1
      @DATE                 23/02/2003
      @COMMENTS             Added units UUninstPrevProj and FmUninstProjects
    )
    @REVISION(
      @VERSION              3.2
      @DATE                 19/11/2003
      @COMMENTS             Removed UActiveRTFWrapper unit.
    )
    @REVISION(
      @VERSION              3.3
      @DATE                 25/11/2005
      @COMMENTS             + Added UHelpManager unit.
                            + Set Application.HelpFile to '' and
                              Application.OnHelp event to call an event handler
                              from THelpManager. Together these changes inhibit
                              Delphi's default help processing leaving
                              THelpManager to handle it.
    )
    @REVISION(
      @VERSION              3.4
      @DATE                 25/03/2007
      @COMMENTS             Removed UDCC32, UDCC32Info and UResWriterStreams
                            units.
    )
    @REVISION(
      @VERSION              3.5
      @DATE                 20/02/2008
      @COMMENTS             + Added UFileNames, UHelpContexts, URegistry and
                              UResources units. These all replace include files.
                            + Moved UBinaryStream, UDataStream, UObjList,
                              UReportingObjects, UStreamWrapper and UXMLObjects
                              units from shared files to project directory.
    )
    @REVISION(
      @VERSION              3.6
      @DATE                 27/02/2008
      @COMMENTS             Added new UHTMLHelp unit.
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
 * The Original Code is SIBuilder.dpr.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


program SIBuilder;

{%ToDo 'SIBuilder.todo'}

uses
  Forms,
  FmAbout in 'FmAbout.pas' {AboutDlg},
  FmFileEdit in 'FmFileEdit.pas' {FileEditDlg},
  FmFileProperties in 'FmFileProperties.pas' {FilePropertiesDlg},
  FmGenericDlg in 'FmGenericDlg.pas' {GenericDlg},
  FmGenericOKDlg in 'FmGenericOKDlg.pas' {GenericOKDlg},
  FmGenericViewDlg in 'FmGenericViewDlg.pas' {GenericViewDlg},
  FmGroupEdit in 'FmGroupEdit.pas' {GroupEditDlg},
  FmInstallFileName in 'FmInstallFileName.pas' {InstallFileNameDlg},
  FmMain in 'FmMain.pas' {MainForm},
  FmOptions in 'FmOptions.pas' {OptionsDlg},
  FmOSOptions in 'FmOSOptions.pas' {OSOptionsDlg},
  FmProgEdit in 'FmProgEdit.pas' {EditProgDlg},
  FmRegDataItemEdit in 'FmRegDataItemEdit.pas' {RegDataItemEditDlg},
  FmRegKeyEdit in 'FmRegKeyEdit.pas' {RegKeyEditDlg},
  FmRegMacroEdit in 'FmRegMacroEdit.pas' {RegMacroEditDlg},
  FmRegTemplates in 'FmRegTemplates.pas' {RegTemplatesDlg},
  FmSimpleEdit in 'FmSimpleEdit.pas' {SimpleEditDlg},
  FmSIP2ConvMsg in 'FmSIP2ConvMsg.pas' {SIP2ConvMsgDlg},
  FmUninstProjects in 'FmUninstProjects.pas' {UninstProjectsDlg},
  UBuilders in 'UBuilders.pas',
  UBuildTabHandler in 'UBuildTabHandler.pas',
  UCmdDispatcher in 'UCmdDispatcher.pas',
  UCompressionMgr in 'UCompressionMgr.pas',
  UDataWriter in 'UDataWriter.pas',
  UDetailsTabHandler in 'UDetailsTabHandler.pas',
  UFiles in 'UFiles.pas',
  UFileValidator in 'UFileValidator.pas',
  UGroups in 'UGroups.pas',
  UGroupsTabHandler in 'UGroupsTabHandler.pas',
  UGUIHelper in 'UGUIHelper.pas',
  UHelpManager in 'UHelpManager.pas',
  UImageList in 'UImageList.pas',
  UInfoWdw in 'UInfoWdw.pas',
  UInstallReport in 'UInstallReport.pas',
  ULicenseDlgHelp in 'ULicenseDlgHelp.pas',
  ULicenseInfo in 'ULicenseInfo.pas',
  ULicenseTabHandler in 'ULicenseTabHandler.pas',
  UMessageBox in 'UMessageBox.pas',
  UOSTabHandler in 'UOSTabHandler.pas',
  UOSTreeViewMgr in 'UOSTreeViewMgr.pas',
  UPayloadStream in 'UPayloadStream.pas',
  UProject in 'UProject.pas',
  UProjFileManager in 'UProjFileManager.pas',
  UProjFileVerManager in 'UProjFileVerManager.pas',
  URegData in 'URegData.pas',
  URegEditFile in 'URegEditFile.pas',
  URegistryTabHandler in 'URegistryTabHandler.pas',
  URegKeys in 'URegKeys.pas',
  URegObjects in 'URegObjects.pas',
  URegRootKeys in 'URegRootKeys.pas',
  URegTemplates in 'URegTemplates.pas',
  URunProgs in 'URunProgs.pas',
  URunProgValidator in 'URunProgValidator.pas',
  URunTabHandler in 'URunTabHandler.pas',
  USettings in 'USettings.pas',
  USIP2Converter in 'USIP2Converter.pas',
  USIBConverter in 'USIBConverter.pas',
  UTabSheetHandler in 'UTabSheetHandler.pas',
  UUnicodeConvStringList in 'UUnicodeConvStringList.pas',
  UUninstPrevProj in 'UUninstPrevProj.pas',
  UWarner in 'UWarner.pas',
  UWelcomeTabHandler in 'UWelcomeTabHandler.pas',
  IntfSITLicenseDlg in '..\..\Intf\IntfSITLicenseDlg.pas',
  IntfCompressor in '..\..\Intf\IntfCompressor.pas',
  UEmbeddedData in '..\..\Shared\Install\UEmbeddedData.pas',
  UPayload in '..\..\Shared\Install\UPayload.pas',
  UCommonTypes in '..\..\Shared\Params\UCommonTypes.pas',
  UPathMacros in '..\..\Shared\Support\UPathMacros.pas',
  UOS in '..\..\Shared\Support\UOS.pas',
  UBinStrings in '..\..\Shared\Utils\UBinStrings.pas',
  UFileProcs in '..\..\Shared\Utils\UFileProcs.pas',
  URegUtils in '..\..\Shared\Utils\URegUtils.pas',
  UStringProcs in '..\..\Shared\Utils\UStringProcs.pas',
  UCharStreamReader in '..\..\Shared\XML\UCharStreamReader.pas',
  UCharStreamWriter in '..\..\Shared\XML\UCharStreamWriter.pas',
  UTokeniser in '..\..\Shared\XML\UTokeniser.pas',
  UXMLFile in '..\..\Shared\XML\UXMLFile.pas',
  UXMLTag in '..\..\Shared\XML\UXMLTag.pas',
  URegistry in '..\..\Shared\Params\URegistry.pas',
  UFileNames in '..\..\Shared\Params\UFileNames.pas',
  UResources in 'UResources.pas',
  UObjList in 'UObjList.pas',
  UReportingObjects in 'UReportingObjects.pas',
  UXMLObjects in 'UXMLObjects.pas',
  UBinaryStream in 'UBinaryStream.pas',
  UDataStream in 'UDataStream.pas',
  UStreamWrapper in 'UStreamWrapper.pas',
  UHelpContexts in 'UHelpContexts.pas',
  UHTMLHelp in 'UHTMLHelp.pas';

{$R *.RES}
{$Resource VSIBuilder.res} // version information
{$Resource Resources.res}  // various SIBuilder resources

begin
  Application.Initialize;
  Application.HelpFile := '';
  Application.OnHelp := THelpManager.PreventAppHelp;
  Application.Title := 'SIBuilder';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


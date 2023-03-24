{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmGenericOKDlg.pas
  @COMMENTS                 This is a form unit unique to the SIBuilder.exe
                            sub-project. It is a generic OK dialogue box,
                            descended from TGenericDlg, that is used as a base
                            class for dialogue boxes that permit editing - adds
                            "OK" and "Cancel" buttons to the form that close the
                            dialogue box with the appropriate modal result. All
                            descendant dlg boxes should place their additional
                            components within the body panel and size it
                            accordingly. The dialog and the basic components
                            will then be sized and positioned using the body
                            panel's size.
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
 * The Original Code is FmGenericOKDlg.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit FmGenericOKDlg;

interface

uses
  // VCL
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  // Project
  FmGenericDlg;

type
  {Generic OK dialogue box used as a base class for dialogue boxes that permit
  editing - adds OK and cancel buttons to the form that close the dialogue box
  with appropriate modal result}
  TGenericOKDlg = class(TGenericDlg)
    CancelBtn: TButton;
    OKBtn: TButton;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.DFM}

procedure TGenericOKDlg.FormCreate(Sender: TObject);
  {Align OK and cancel buttons to form}
begin
  inherited;
  OKBtn.Top := BottomBevel.Top + 8;
  CancelBtn.Top := OKBtn.Top;
  CancelBtn.Left := HelpBtn.Left - CancelBtn.Width - 4;
  OKBtn.Left := CancelBtn.Left - OKBtn.Width - 4;
end;

end.

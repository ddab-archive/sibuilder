{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     FmGenericViewDlg.pas
  @COMMENTS                 This is a form unit unique to the SIBuilder.exe
                            sub-project. It is a generic base class for dialog
                            boxes that simply display information and have a
                            "done" button rather than "OK" and "Cancel". It
                            descends from TGenericDlg and simple adds a "Done"
                            button that closes the dialogue box. All descendant
                            dlg boxes should place their additional components
                            within the body panel and size it accordingly. The
                            dialog and the basic components will then be sized
                            and positioned using the body panel's size.
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
 * The Original Code is FmGenericViewDlg.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit FmGenericViewDlg;

interface

uses
  // VCL
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  // Project
  FmGenericDlg;

type
  {Generic dialogue box used to view (rather then edit) information - provides
  a Done button that closes dialogue box in addition to Help button inherited
  from TGenericDlg}
  TGenericViewDlg = class(TGenericDlg)
    DoneBtn: TButton;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.DFM}

procedure TGenericViewDlg.FormCreate(Sender: TObject);
  {Form creation event handler - aligns done button to rest of form}
begin
  inherited;
  DoneBtn.Top := BottomBevel.Top + 8;
  DoneBtn.Left := HelpBtn.Left - DoneBtn.Width - 4;
end;

end.

{ ##
  @PROJECT_NAME             SI Tools
  @PROJECT_DESC             Simple program installation tools - project manager
                            and creator with installation and un-installation
                            programs.
  @FILE                     UReportingObjects.pas
  @COMMENTS                 This unit contains defintions of two related clases
                            - TReportingObjList and TReportingListItem that
                            extend the TXMLObjectList and TXMLObjectItem classes
                            by adding the ability to report progress to their
                            owning classes or lists (if these are also descended
                            from this unit's classes) or by triggering events
                            containing the progress report. These classes are
                            designed to propagate reports up through nested
                            lists without the need for event handlers at each
                            level.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 09/03/2000
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 10/08/2000
      @COMMENTS             New version of this object now works with XML
                            objects instead of binary streams to achieve
                            persistency. There were quite minor code changes to
                            achieve this major change in functionality:
                            + the reference to the UStreambleObjList unit was
                              changed to UXMLObjects
                            + the parent classes TStreamableListItem and
                              TStreamableObjList were replaced by TXMLObjectItem
                              and TXMLObjectList respectively.
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
 * The Original Code is UReportingObjects.pas.
 * 
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 * 
 * Portions created by the Initial Developer are Copyright (C) 2000 Peter
 * Johnson. All Rights Reserved.
 * 
 * ***** END LICENSE BLOCK *****
}


unit UReportingObjects;

interface

uses
  // Project
  UXMLObjects;

type
  TReportEvent = procedure(const Msg: string) of object;
    {The type of event handler required by the OnReport event}

  TReportingListItem = class(TXMLObjectItem)
    {A streamble object that can report progress either by calling a method in
    any TReportingObjList it belongs to or by triggering an event. The class is
    designed to be used to propagate changes up through nested lists. Descendant
    classes call the ReportMessage method when any progress is to be notified}
  private // properties
    FOnReport: TReportEvent;
  protected
    procedure ReportMessage(const Msg: string); virtual;
      {If list that contains this item is a TReportingObjList then this method
      calls the ReportMessage the list with the message. If the object does not
      belong to a list or if the list is not a TReportingObjList then the
      OnReport event is triggered with the message}
  public
    property OnReport: TReportEvent read FOnReport write FOnReport;
      {Event triggered when ReportMessage is called and object is not in a
      TReportingObjList}
  end;

  TReportingObjList = class(TXMLObjectList)
    {A streamble list that also reports progress to its owning object - either
    by calling a method in that object if it's a TReportingListItem or by
    triggering an event. The class is designed to be used to propagate changes
    up through nested lists}
  private // properties
    FOnReport: TReportEvent;
  protected
    procedure ReportMessage(const Msg: string); virtual;
      {If list is owned by a TReportingListItem object then this method calls
      the ReportMessage of the owning object with the message. If this is not
      the case then the OnReport event is triggered with the message}
  public
    property OnReport: TReportEvent read FOnReport write FOnReport;
      {Event triggered when ReportMessage is called and list is not owned by a
      TReportingListItem}
  end;

implementation

{ TReportingListItem }

procedure TReportingListItem.ReportMessage(const Msg: string);
  {If list that contains this item is a TReportingObjList then this method calls
  the ReportMessage the list with the message. If the object does not belong to
  a list or if the list is not a TReportingObjList then the OnReport event is
  triggered with the message}
begin
  if List is TReportingObjList then
    (List as TReportingObjList).ReportMessage(Msg)
  else if Assigned(FOnReport) then
    FOnReport(Msg);
end;

{ TReportingObjList }

procedure TReportingObjList.ReportMessage(const Msg: string);
  {If list is owned by a TReportingListItem object then this method calls the
  ReportMessage of the owning object with the message. If this is not the case
  then the OnReport event is triggered with the message}
begin
  if Owner is TReportingListItem then
    (Owner as TReportingListItem).ReportMessage(Msg)
  else if Assigned(FOnReport) then
    FOnReport(Msg);
end;

end.

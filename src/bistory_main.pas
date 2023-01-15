{********************************************************}
{                                                        }
{                     Bistory                            }
{                                                        }
{         Copyright (c) 2022    Helmut Elsner            }
{                                                        }
{       Compiler: FPC 3.0.4   /    Lazarus 2.0.8         }
{                                                        }
{ Pascal programmers tend to plan ahead, they think      }
{ before they type. We type a lot because of Pascal      }
{ verboseness, but usually our code is right from the    }
{ start. We end up typing less because we fix less bugs. }
{           [Jorge Aldo G. de F. Junior]                 }
{********************************************************}

(*
This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.

================================================================================
Brief description:

Collect and categorize terminal command that one often forget.

================================================================================

Format .Bistory.dat

Data separator: #
category # command # descrition
string     string    string

2022-01-15   Idea and functionality

==============================================================================*)
unit bistory_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ComCtrls, ActnList, Menus, Grids, lclintf, Process;

type
  TBistory = record
    cat: string;           // Category
    cmd: string;           // Command string
    hnt: string;           // Description / Hint
   end;

  { TForm1 }

  TForm1 = class(TForm)
    actClose: TAction;
    actAdd: TAction;
    actExecute: TAction;
    actAbout: TAction;
    actCopy: TAction;
    actGit: TAction;
    actEdit: TAction;
    actDelete: TAction;
    actManual: TAction;
    actNew: TAction;
    actLoadHist: TAction;
    ActionList: TActionList;
    btnAdd: TBitBtn;
    btnCopy: TBitBtn;
    btnNew: TBitBtn;
    btnExecute: TBitBtn;
    btnClose: TBitBtn;
    btnEdit: TBitBtn;
    btnDelete: TBitBtn;
    btnReadHist: TBitBtn;
    cbCategory: TComboBox;
    edCommand: TEdit;
    gbCommand: TGroupBox;
    ImageList1: TImageList;
    lblInfo: TLabel;
    lblCommand: TLabel;
    lblCategory: TLabel;
    edInfo: TMemo;
    MainMenu1: TMainMenu;
    Separator2: TMenuItem;
    mnDelete: TMenuItem;
    mnEdit: TMenuItem;
    mnGit: TMenuItem;
    mnAbout: TMenuItem;
    mnManual: TMenuItem;
    mnHilfe: TMenuItem;
    mnExecute: TMenuItem;
    mnCopy: TMenuItem;
    mnAdd: TMenuItem;
    mnNew: TMenuItem;
    mnTools: TMenuItem;
    mnClose: TMenuItem;
    Separator1: TMenuItem;
    mnLoadHist: TMenuItem;
    mnDatei: TMenuItem;
    pcHist: TPageControl;
    StatusBar: TStatusBar;
    gridHist: TStringGrid;
    tsTree: TTabSheet;
    tsHist: TTabSheet;
    tvCommand: TTreeView;
    procedure actAboutExecute(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actEditExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure actGitExecute(Sender: TObject);
    procedure actLoadHistExecute(Sender: TObject);
    procedure actManualExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure gridHistBeforeSelection(Sender: TObject; aCol, aRow: Integer);

  private
    procedure FillTree;            // Read data and fill treeview

  public

  end;

{$I bistory_de.inc}
{.$I bistory_en.inc}

var
  Form1: TForm1;

const
  tab1=' ';
  sep='#';               // data separator
  myhistfile='.bistory.dat';
  manualfile='Bistory_Manual.pdf';
  gitlink='https://github.com/h-elsner/Bistory';
  homepage='http://h-elsner.mooo.com';
  meinname='h-elsner';
  bashhistfile='.bash_history';

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);          // Initialization
begin
  Caption:=progname+tab1+version;
  gbCommand.Caption:=capCommands;
  lblCategory.Caption:=capCategory;
  lblCategory.Hint:=hntCategory;
  cbCategory.Text:='';
  cbCategory.TextHint:=hntCategory;
  cbCategory.Hint:=hntCategory;
  lblCommand.Caption:=capCommand;
  lblCommand.Hint:=hntCommand;
  edCommand.Text:='';
  edCommand.TextHint:=hntCommand;
  edCommand.Hint:=hntCommand;
  lblInfo.Caption:=capInfo;
  lblInfo.Hint:=hntInfo;
  edInfo.Clear;
  edInfo.Hint:=hntInfo;
  Statusbar.Panels[0].Width:=pcHist.Left;
  tsTree.Caption:=capCommands;
  tsHist.Caption:=capLoadHist;
  gridHist.Cells[0, 0]:=capCommands;

  actClose.Caption:=capClose;
  actClose.Hint:=hntClose;
  actLoadHist.Caption:=capLoadHist;
  actLoadHist.Hint:=hntLoadHist;
  actNew.Caption:=capNew;
  actNew.Hint:=hntNew;
  actAdd.Caption:=capAdd;
  actAdd.Hint:=hntAdd;
  actExecute.Caption:=capExecute;
  actExecute.Hint:=hntExecute;
  actAbout.Caption:=capAbout;
  actManual.Caption:=capManual;
  actManual.Hint:=hntManual;
  actCopy.Caption:=capCopy;
  actCopy.Hint:=hntCopy;
  actGit.Caption:=capGit;
  actGit.Hint:=hntGit;
  actEdit.Caption:=capEdit;
  actEdit.Hint:=hntEdit;
  actDelete.Caption:=capDelete;
  actDelete.Hint:=hntDelete;

  mnHilfe.Caption:=capHilfe;
  mnTools.Caption:=capTools;
  mnDatei.Caption:=capDatei;
end;

procedure TForm1.FormDblClick(Sender: TObject);             // Copy to clipboard by double click
begin
  edCommand.CopyToClipboard;
end;

procedure TForm1.FormResize(Sender: TObject);               // Arrange controls on app window
begin
  pcHist.Width:=(Form1.Width-pcHist.Left*3) div 2;
  gbCommand.Left:=pcHist.Left+pcHist.Width+pcHist.Left;
  gbCommand.Width:=pcHist.Width;

  btnReadHist.Left:=gbCommand.Left+btnAdd.Left;

  btnAdd.Width:=(gbCommand.Width-btnAdd.Left*4) div 3;
  btnNew.Width:=btnAdd.Width;
  btnEdit.Width:=btnAdd.Width;
  btnDelete.Width:=btnAdd.Width;
  btnCopy.Width:=btnAdd.Width;
  btnExecute.Width:=btnAdd.Width;

  btnDelete.Left:=btnAdd.Width+btnAdd.Left*2;
  btnEdit.Left:=btnDelete.Left;
  btnExecute.Left:=btnAdd.Width*2+btnAdd.Left*3;
  btnCopy.Left:=btnExecute.Left;
  gridHist.ColWidths[0]:=gridHist.Width;
end;

procedure TForm1.gridHistBeforeSelection(Sender: TObject; aCol, aRow: Integer);   // Take over a command from bash history
begin
  edCommand.Text:=gridHist.Cells[aCol, aRow].Split([sep])[0];
  edInfo.Text:=trim(gridHist.Cells[aCol, aRow].Split([sep])[1]);
end;

procedure RemoveDuplicates(var list: TStringList);   // Remove duplicates from string list
var
  sortlist: TStringList;
  i: integer;

begin
  sortlist:=TStringList.Create;
  sortlist.Sorted:=true;
  sortlist.Duplicates:=dupIgnore;
  try
    for i:=0 to list.Count-1 do
      sortlist.Add(list[i]);
    list.Assign(sortlist);
  finally
    sortlist.Free;
  end;
end;

function SplitData(const s: string): TBistory;   // Read one line and split
begin
  result.cat:=trim(s.Split([sep])[0]);
  if result.cat='' then
    result.cat:=defaultcat;
  result.cmd:=s.Split([sep])[1];
  result.hnt:=s.Split([sep])[2];
end;

procedure TForm1.FillTree;            // Read data and fill treeview
var
  fn, oldcat: string;
  hlist, catlist: TStringList;
  i: integer;
  zl: TBistory;
  nodecat, nodecmd: TTreeNode;

begin
  StatusBar.Panels[1].Text:='';
  hlist:=TStringList.Create;
  catlist:=TStringList.Create;
  catlist.Sorted:=true;
  catlist.Duplicates:=dupIgnore;
  oldcat:='';
  try
    fn:=GetUserDir+myhistfile;
    if not FileExists(fn) then begin      // no data yet - create default
      hlist.Add(defaultcommand);
      hlist.SaveToFile(fn);
      catlist.Assign(cbCategory.Items);   // Keep default categories
    end;
    hlist.LoadFromFile(fn);
    if hlist.Count>0 then begin
      hlist.Sort;
      StatusBar.Panels[1].Text:=IntToStr(hlist.Count)+tab1+capCommands;
      tvCommand.Items.Clear;
      for i:=0 to hlist.Count-1 do begin
        zl:=SplitData(hlist[i]);
        catlist.Add(zl.cat);
        if zl.cat<>oldcat then begin
          nodecat:=tvCommand.Items.Add(nil, zl.cat);
          oldcat:=zl.cat;
        end;
        nodecmd:=tvCommand.Items.AddChild(nodecat, zl.cmd);
        if zl.hnt<>'' then
          tvCommand.Items.AddChild(nodecmd, zl.hnt);
      end;
      cbCategory.Items.Assign(catlist);
      tvCommand.FullExpand;
    end else
      StatusBar.Panels[1].Text:=errNoData+myhistfile;
  finally
    HList.Free;
    catlist.Free;
  end;
end;

procedure TForm1.actLoadHistExecute(Sender: TObject);   // Load bash history
var
  inlist: TStringList;
  i: integer;

begin
  StatusBar.Panels[1].Text:='';
  inlist:=TStringList.Create;
  try
    inlist.LoadFromFile(GetUserDir+bashhistfile);
    if inlist.Count>0 then begin
      RemoveDuplicates(inlist);
      StatusBar.Panels[1].Text:=IntToStr(inlist.Count)+tab1+capCommands;
      gridHist.RowCount:=inlist.Count+1;
      for i:=0 to inlist.Count-1 do begin
        gridHist.Cells[0, i+1]:=inlist[i];
      end;
      pcHist.ActivePage:=tsHist;
    end
      else StatusBar.Panels[1].Text:=errNoData+bashhistfile;
  finally
    inlist.Free;
  end;
end;

procedure TForm1.actManualExecute(Sender: TObject);   // Open Manual
var
  fn: string;

begin
  fn:=Application.Location+manualfile;
  if FileExists(fn) then begin
    OpenDocument(fn);
  end else begin
    OpenURL(gitlink);
  end;
end;

procedure TForm1.actNewExecute(Sender: TObject);     // New command
begin
  cbCategory.Text:='';
  edCommand.Text:='';
  edInfo.Clear;
  cbCategory.SetFocus;
end;

procedure TForm1.FormActivate(Sender: TObject);       // All to do at start
begin
  FillTree;
end;

procedure TForm1.actCloseExecute(Sender: TObject);    // Quit
begin
  Close;
end;

procedure TForm1.actCopyExecute(Sender: TObject);     // Copy to Clipboard
begin
  edCommand.CopyToClipboard;
end;

procedure TForm1.actDeleteExecute(Sender: TObject);
var
  fn: string;
  hlist: TStringList;
  i: integer;
  zl: TBistory;
  del: boolean;

begin
  del:=false;
  StatusBar.Panels[1].Text:='';
  if trim(edCommand.Text)<>'' then begin
    fn:=GetUserDir+myhistfile;
    hlist:=TStringList.Create;
    try
      if FileExists(fn) then
        hlist.LoadFromFile(fn);
      for i:=hlist.Count-1 downto 0 do begin
        zl:=SplitData(hlist[i]);
        if zl.cmd=edCommand.Text then begin
          hlist.Delete(i);
          del:=true;
        end;
      end;
      if del then begin
        hlist.SaveToFile(fn);
        FillTree;                                   // Reload TreeView from file
        StatusBar.Panels[1].Text:=edCommand.Text+sDeleted;
      end;
    finally
      hlist.Free;
    end;
  end else
    StatusBar.Panels[1].Text:=errNoCmd;
end;

procedure TForm1.actEditExecute(Sender: TObject);
var
  fn: string;
  hlist: TStringList;
  i: integer;
  zl: TBistory;
  ed: Boolean;

begin
  ed:=false;
  StatusBar.Panels[1].Text:='';
  if trim(edCommand.Text)<>'' then begin
    fn:=GetUserDir+myhistfile;
    hlist:=TStringList.Create;
    try
      if FileExists(fn) then
        hlist.LoadFromFile(fn);
      for i:=0 to hlist.Count-1 do begin
        zl:=SplitData(hlist[i]);
        if zl.cmd=edCommand.Text then begin
          hlist[i]:=cbCategory.Text+sep+edCommand.Text+sep+edInfo.Text;
          ed:=true;
          break;
        end;
      end;
      if not ed then begin  // Command could not be found, was edited
        hlist.Add(cbCategory.Text+sep+edCommand.Text+sep+edInfo.Text);
        StatusBar.Panels[1].Text:=edCommand.Text+sAdded;
      end else
        StatusBar.Panels[1].Text:=edCommand.Text+sEdited;
      hlist.SaveToFile(fn);
      FillTree;                                   // Reload TreeView from file
    finally
      hlist.Free;
    end;
  end else
    StatusBar.Panels[1].Text:=errNoCmd;
end;

procedure TForm1.actExecuteExecute(Sender: TObject);  // Excecute the selected command
var
  cmd: TProcess;

begin
  StatusBar.Panels[1].Text:='';
  if trim(edCommand.Text)<>'' then begin
    StatusBar.Panels[1].Text:=edCommand.Text+' ...';
    cmd:=TProcess.Create(nil);
    try
      cmd.Options:=cmd.Options+[poNewConsole, poWaitOnExit];
      cmd.Executable:=edCommand.Text;
      cmd.Execute;
    finally
      cmd.Free;
    end;
  end else
    StatusBar.Panels[1].Text:=errNoCmd;
end;

procedure TForm1.actGitExecute(Sender: TObject);     // Homepage in GitHub
begin
  OpenURL(gitlink);
end;

procedure TForm1.actAddExecute(Sender: TObject);
var
  fn: string;
  hlist: TStringList;

begin
  StatusBar.Panels[1].Text:='';
  if trim(edCommand.Text)<>'' then begin
    fn:=GetUserDir+myhistfile;
    hlist:=TStringList.Create;
    try
      if FileExists(fn) then
        hlist.LoadFromFile(fn);
      hlist.Add(cbCategory.Text+sep+edCommand.Text+sep+edInfo.Text);
      hlist.SaveToFile(fn);
      FillTree;                                      // Reload TreeView from file
      StatusBar.Panels[1].Text:=edCommand.Text+sAdded;
    finally
      hlist.Free;
    end;
  end else
    StatusBar.Panels[1].Text:=errNoCmd;
end;

procedure TForm1.actAboutExecute(Sender: TObject);    // About box
begin
  if MessageDlg(ProgName+tab1+tab1+Version+sLineBreak+
                sLineBreak+meinname+sLineBreak+sLineBreak+homepage,
                mtInformation,[mbHelp, mbOK],0)=mrNone then
    actManualExecute(self);
end;

end.


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
  ComCtrls, ActnList, Menus, Grids, lclintf, XMLPropStorage, Process, Types,
  FileUtil, Clipbrd;

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
    actResult: TAction;
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
    edRes: TMemo;
    mnSaveAll: TMenuItem;
    mnCopyAll: TMenuItem;
    mnSave: TMenuItem;
    pmResult: TPopupMenu;
    SaveDialog: TSaveDialog;
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
    tsResult: TTabSheet;
    tsTree: TTabSheet;
    tsHist: TTabSheet;
    tvCommand: TTreeView;
    XMLPropStorage1: TXMLPropStorage;
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
    procedure actResultExecute(Sender: TObject);
    procedure edCommandKeyPress(Sender: TObject; var Key: char);
    procedure edResMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure edResMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure gridHistBeforeSelection(Sender: TObject; aCol, aRow: Integer);
    procedure mnCopyAllClick(Sender: TObject);
    procedure tvCommandClick(Sender: TObject);
    procedure tvCommandDblClick(Sender: TObject);
    procedure tvCommandMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure tvCommandMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);

  private
    procedure FillTree;                                                          // Read data and fill treeview

  public

  end;

{$I bistory_de.inc}
{.$I bistory_en.inc}

var
  Form1: TForm1;

const
  tab1=' ';
  sep='#';                                                                       // data separator
  myhistfile='.bistory.dat';
  tmpfile='.bi_temp.txt';
  manualfile='Bistory_Manual.pdf';
  gitlink='https://github.com/h-elsner/Bistory';
  homepage='http://h-elsner.mooo.com';
  meinname='h-elsner';
  bashhistfile='.bash_history';
  version=' V0.2';


implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);                                    // Initialization
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
  edRes.Clear;
  edInfo.Hint:=hntInfo;
  Statusbar.Panels[0].Width:=pcHist.Left;
  tsTree.Caption:=capCommands;
  tsHist.Caption:=capLoadHist;
  tsResult.Caption:=capResult;
  gridHist.Cells[0, 0]:=capCommands;
  SaveDialog.Title:=capSaveTitle;

  actClose.Caption:=capClose;                                                     // GUI settings
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
  actResult.Caption:=capSaveTitle;

  mnHilfe.Caption:=capHilfe;
  mnTools.Caption:=capTools;
  mnDatei.Caption:=capDatei;
  mnCopyAll.Caption:=capCopyAll;
  pcHist.ActivePage:=tsTree;
end;

procedure TForm1.FormDblClick(Sender: TObject);                                  // Copy to clipboard by double click
begin
  edCommand.SelectAll;
  edCommand.CopyToClipboard;
end;

procedure TForm1.FormResize(Sender: TObject);                                    // Arrange controls on app window
begin
  if (gbCommand.Width<500) or (pcHist.Width<500) then begin
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
  end;
  gridHist.ColWidths[0]:=gridHist.Width;
end;

procedure TForm1.gridHistBeforeSelection(Sender: TObject; aCol, aRow: Integer);  // Take over a command from bash history
begin
  edCommand.Text:=gridHist.Cells[aCol, aRow].Split([sep])[0];
  edInfo.Text:=trim(gridHist.Cells[aCol, aRow].Split([sep])[1]);
end;

procedure TForm1.mnCopyAllClick(Sender: TObject);                                // Copy Result to clipboard
begin
  ClipBoard.AsText:=edRes.Text;
end;

procedure TForm1.tvCommandClick(Sender: TObject);                                // Select one command
var
  cnode: TTreeNode;

begin
  if (tvCommand.Selected<>nil) then begin
    if (tvCommand.Selected.Level>0) then begin
      cnode:=tvCommand.Selected;
      if cnode.Level=1 then begin
        if cnode.GetFirstChild<>nil then
          edInfo.Text:=cnode.GetFirstChild.Text
        else
          edInfo.Text:='';
      end;
      if cnode.Level=2 then begin                                                // Command one level up
        edInfo.Text:=cnode.Text;
        cnode:=cnode.Parent;
      end;
      cbCategory.Text:=cnode.Parent.Text;
      edCommand.Text:=cnode.Text;
      edCommand.SetFocus;
    end else
      cbCategory.Text:=tvCommand.Selected.Text;                                  // Take over only category
  end;
end;

procedure TForm1.tvCommandDblClick(Sender: TObject);
begin
  tvCommand.FullExpand;
end;

procedure TForm1.tvCommandMouseWheelDown(Sender: TObject; Shift: TShiftState;    // Size font down
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    tvCommand.Font.Size:=tvCommand.Font.Size-1;
end;

procedure TForm1.tvCommandMouseWheelUp(Sender: TObject; Shift: TShiftState;      // Size font up
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    tvCommand.Font.Size:=tvCommand.Font.Size+1;
end;

procedure RemoveDuplicates(var list: TStringList);                               // Remove duplicates from string list
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

function SplitData(const s: string): TBistory;                                   // Read one line and split
begin
  result.cat:=trim(s.Split([sep])[0]);
  if result.cat='' then
    result.cat:=defaultcat;
  result.cmd:=s.Split([sep])[1];
  result.hnt:=s.Split([sep])[2];
end;

procedure TForm1.FillTree;                                                       // Read data and fill treeview
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
    if not FileExists(fn) then begin                                             // no data yet - create default
      hlist.Add(defaultcommand);
      hlist.SaveToFile(fn);
      catlist.Assign(cbCategory.Items);                                          // Keep default categories
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
        nodecmd.ImageIndex:=13;
        if zl.hnt<>'' then
          tvCommand.Items.AddChild(nodecmd, zl.hnt);
      end;
      catlist.Add('Info');                                                       // Add default categories
      catlist.Add('System');
      catlist.Add('Tools');
      cbCategory.Items.Assign(catlist);

      for i:=0 to tvCommand.Items.Count-1 do begin                               // Expand current used category tree
        nodecat:=tvCommand.Items[i];
        if (nodecat.Level=0) and (nodecat.Text=cbCategory.Text) then
          nodecat.Expand(true);
      end;
    end else
      StatusBar.Panels[1].Text:=errNoData+myhistfile;
  finally
    HList.Free;
    catlist.Free;
  end;
end;

procedure TForm1.actLoadHistExecute(Sender: TObject);                            // Load bash history
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
    end else
      StatusBar.Panels[1].Text:=errNoData+bashhistfile;
  finally
    inlist.Free;
  end;
end;

procedure TForm1.actManualExecute(Sender: TObject);                              // Open Manual
var
  fn: string;

begin
  fn:=Application.Location+manualfile;
  if FileExists(fn) then begin
    OpenDocument(fn);
  end else begin
    OpenURL(gitlink);                                                            // GitHub link
  end;
end;

procedure TForm1.actNewExecute(Sender: TObject);                                 // New command
begin
  cbCategory.Text:='';
  edCommand.Text:='';
  edInfo.Clear;
  edCommand.SetFocus;
end;

procedure TForm1.actResultExecute(Sender: TObject);                              // Save Result of command execution
begin
  if SaveDialog.Execute then
    edRes.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TForm1.edCommandKeyPress(Sender: TObject; var Key: char);              // Command execute when ENTER was pressed
begin
  if key=#13 then begin                                                          // Enter key
    Key:=#0;
    actExecuteExecute(self);
  end;
end;

procedure TForm1.edResMouseWheelDown(Sender: TObject; Shift: TShiftState;        // Size Font down
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    edRes.Font.Size:=edRes.Font.Size-1;
end;

procedure TForm1.edResMouseWheelUp(Sender: TObject; Shift: TShiftState;          // Size Font up
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    edRes.Font.Size:=edRes.Font.Size+1;
end;

procedure TForm1.FormActivate(Sender: TObject);                                  // All to do at start
begin
  FillTree;
end;

procedure TForm1.actCloseExecute(Sender: TObject);                               // Quit
begin
  Close;
end;

procedure TForm1.actCopyExecute(Sender: TObject);                                // Copy to Clipboard
begin
  ClipBoard.AsText:=edCommand.Text;
end;

procedure TForm1.actDeleteExecute(Sender: TObject);                              // Delete selected command
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
        FillTree;                                                                // Reload TreeView from file
        StatusBar.Panels[1].Text:=edCommand.Text+sDeleted;
      end;
    finally
      hlist.Free;
    end;
  end else
    StatusBar.Panels[1].Text:=errNoCmd;
end;

procedure TForm1.actEditExecute(Sender: TObject);                                // Save an edited command
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
      if not ed then begin                                                       // Command could not be found, was edited
        hlist.Add(cbCategory.Text+sep+edCommand.Text+sep+edInfo.Text);
        StatusBar.Panels[1].Text:=edCommand.Text+sAdded;
      end else
        StatusBar.Panels[1].Text:=edCommand.Text+sEdited;
      hlist.SaveToFile(fn);
      FillTree;                                                                  // Reload TreeView from file
    finally
      hlist.Free;
    end;
  end else
    StatusBar.Panels[1].Text:=errNoCmd;
end;

procedure TForm1.actExecuteExecute(Sender: TObject);                             // Excecute the selected command
var
  cmd: TProcess;
  fn, s: string;
  outlist: TStringList;
  i, k, nc: integer;
  commands: TStringArray;

begin
  StatusBar.Panels[1].Text:='';
  nc:=0;
  if trim(edCommand.Text)<>'' then begin
    outlist:=TStringList.Create;
    cmd:=TProcess.Create(nil);
    Screen.Cursor:=crHourGlass;                                                  // Let the user know that something is going on
    try
      fn:=GetUserDir+tmpfile;
      StatusBar.Panels[1].Text:=edCommand.Text+' ...';
      edRes.Clear;
      s:=edCommand.Text;
      commands:=s.Split(['&&']);
      nc:=high(commands);
      edRes.Lines.Add(edCommand.Text);
      edRes.Lines.Add('');
      cmd.Options:=cmd.Options+[poWaitOnExit, poNewConsole];                     // poNewConsole vs. poUsePipes ???
      cmd.Executable:=FindDefaultExecutablePath('bash');                           // Find shell
      for k:=0 to nc do begin
        cmd.Parameters.Clear;
        cmd.Parameters.Add('-c');                                                // Read commands from the command_string
        s:=trim(commands[k]);
        cmd.Parameters.Add(s+' 2>&1|tee '+fn);                                   // Duplicate stdout and stderr
        cmd.Execute;
   //     outlist.LoadFromStream(cmd.Output);
        outlist.LoadFromFile(fn);                                                // Workaround by temp file
        if nc>0 then
          edRes.Lines.Add('  [ '+s+' ]');
        for i:=0 to outlist.Count-1 do
          edRes.Lines.Add(outlist[i]);                                           // Append
      end;
      if outlist.Count>0 then
        pcHist.ActivePage:=tsResult;
    finally
      cmd.FreeOnRelease;
      outlist.Free;
      Screen.Cursor:=crDefault;
    end;
  end else
    StatusBar.Panels[1].Text:=errNoCmd;
end;

(*
procedure TForm1.actExecuteExecute(Sender: TObject);                             // Excecute the selected command
var
  r: integer;
begin
  r:=fpsystem(edCommand.Text);
  StatusBar.Panels[1].Text:='Exit code: '+IntToStr(r);
end;   *)

procedure TForm1.actGitExecute(Sender: TObject);                                 // Homepage in GitHub
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
      FillTree;                                                                  // Reload TreeView from file
      StatusBar.Panels[1].Text:=edCommand.Text+sAdded;
    finally
      hlist.Free;
    end;
  end else
    StatusBar.Panels[1].Text:=errNoCmd;
end;

procedure TForm1.actAboutExecute(Sender: TObject);                               // About box
begin
  if MessageDlg(ProgName+tab1+tab1+Version+sLineBreak+
                sLineBreak+meinname+sLineBreak+sLineBreak+homepage,
                mtInformation,[mbHelp, mbOK],0)=mrNone then
    actManualExecute(self);
end;

end.


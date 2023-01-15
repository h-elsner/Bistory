unit bistory_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ComCtrls, ActnList;

type

  { TForm1 }

  TForm1 = class(TForm)
    actClose: TAction;
    actAdd: TAction;
    actExecute: TAction;
    actCategory: TAction;
    actNew: TAction;
    actSaveBistory: TAction;
    actLoadHist: TAction;
    ActionList: TActionList;
    btnAdd: TBitBtn;
    btnNew: TBitBtn;
    btnExecute: TBitBtn;
    btnClose: TBitBtn;
    btnReadHist: TBitBtn;
    cbxTerminal: TCheckBox;
    cbCategory: TComboBox;
    edCommand: TEdit;
    gbCommand: TGroupBox;
    ImageList1: TImageList;
    lblInfo: TLabel;
    lblCommand: TLabel;
    lblCategory: TLabel;
    edInfo: TMemo;
    StatusBar: TStatusBar;
    tvCommand: TTreeView;
    procedure btnCloseClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnReadHistClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);


  private

  public

  end;

{$I bistory_de.inc}
{.$I bistory_en.inc}

var
  Form1: TForm1;

const
  tab1=' ';
  sep='#';
  GUIspace=24;
  myhistfile='.bistory.dat';
  bashhistfile='.bash_history';

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
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
  cbxTerminal.Caption:=capTerminal;
  cbxTerminal.Hint:=hntTerminal;
  Statusbar.Panels[0].Width:=tvCommand.Left;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  tvCommand.Width:=(Form1.Width-GUIspace*3) div 2;
  gbCommand.Left:=tvCommand.Left+tvCommand.Width+GUIspace;
  gbCommand.Width:=tvCommand.Width;
  btnReadHist.Left:=gbCommand.Left+cbxTerminal.Left;
end;

procedure RemoveDuplicates(var list: TStringList);
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

procedure TForm1.btnReadHistClick(Sender: TObject);   // Read bash history
var
  inlist: TStringList;

begin
  inlist:=TStringList.Create;
  try
    inlist.LoadFromFile(GetUserDir+bashhistfile);
    RemoveDuplicates(inlist);
    StatusBar.Panels[1].Text:=IntToStr(inlist.Count)+tab1+capCommands;
  finally
    inlist.Free;
  end;
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btnNewClick(Sender: TObject);
begin
  cbCategory.Text:='';
  edCommand.Text:='';
  edInfo.Clear;
  cbCategory.SetFocus;
end;

end.


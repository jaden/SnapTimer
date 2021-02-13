unit Options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, LResources, Forms, Graphics, Dialogs, StdCtrls, ComCtrls, Spin;

type

  { TOptionsForm }

  TOptionsForm = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    BtnDefaults: TButton;
    CheckAutoRestart: TCheckBox;
    CheckHideSeconds: TCheckBox;
    CheckSecondsMode: TCheckBox;
    CheckAutoSave: TCheckBox;
    CheckAlwaysOnTop: TCheckBox;
    CheckTicking: TCheckBox;
    CheckLoopAudio: TCheckBox;
    CheckClickTime: TCheckBox;
    CheckDblClickTime: TCheckBox;
    FontColor: TColorButton;
    FontNameBtn: TButton;
    CheckAutostart: TCheckBox;
    CheckMinToTray: TCheckBox;
    BgColor: TColorButton;
    FontSize: TSpinEdit;
    Label1: TLabel;
    FontName: TStaticText;
    LabelTextColor: TLabel;
    LabelTextColor1: TLabel;
    TimerFontBox: TGroupBox;
    PositionCombo: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    NotifyAudio: TEdit;
    NotifyAudioBtn: TButton;
    NotifyAudioOn: TCheckBox;
    NotifyAudioTest: TButton;
    NotifyMsg: TEdit;
    NotifyMsgOn: TCheckBox;
    NotifyMsgTest: TButton;
    NotifyRunApp: TEdit;
    NotifyRunAppOn: TCheckBox;
    NotifyRunBtn: TButton;
    NotifyRunTest: TButton;
    NotifyTrayMsg: TEdit;
    NotifyTrayMsgOn: TCheckBox;
    NotifyTrayMsgTest: TButton;
    PageControl1: TPageControl;
    GeneralTab: TTabSheet;
    AlarmsTab: TTabSheet;
    FontsTab: TTabSheet;
    procedure ChooseFont(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure GetAudioFile(Sender: TObject);
    procedure GetAppFile(Sender: TObject);
    function GetFile(Orig: String; FilterStr: String; Dir : String): String;
    procedure EnableFields(Sender: TObject);
    procedure SetDefaults(Sender: TObject);
    procedure TestAudio(Sender: TObject);
    procedure TestMessage(Sender: TObject);
    procedure TestRunApp(Sender: TObject);
    procedure TestTrayMsg(Sender: TObject);
    procedure UpdateFontColor(Sender: TObject);
    procedure UpdateFonts(Sender: TObject);
    procedure UpdateFontSize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  OptionsForm: TOptionsForm;
  f: TFont;

implementation
uses MainForm1;
{ TOptionsForm }

// TODO Move all text strings to the top

procedure TOptionsForm.GetAudioFile(Sender: TObject);
begin
  NotifyAudio.Text := GetFile(NotifyAudio.Text, 'Wave files (*.wav)|*.wav', GetCurrentDir + '\sounds');
end;

procedure TOptionsForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if key = #27 then Close;
end;

procedure TOptionsForm.ChooseFont(Sender: TObject);
var
	dlg : TFontDialog;
begin
	dlg := TFontDialog.Create(nil);
  dlg.Font := f;

  if dlg.Execute then
  begin
    f.Name := dlg.Font.Name;
    f.Size := dlg.Font.Size;
    // TODO Only set color if user changed it (XP dialog doesn't support custom colors)
    // Works fine on Win7
    //if not (f.Color = dlg.Font.Color) then f.Color:= dlg.Font.Color;
    //f.Color:= dlg.Font.Color;
    f.CharSet:= dlg.Font.CharSet;
    f.Style:= dlg.Font.Style;
		UpdateFonts(Sender);
  end;
  dlg.Free;
end;

procedure TOptionsForm.UpdateFonts(Sender: TObject);
begin
  FontName.Caption := f.Name + ' : 1234567890';
  FontName.Font.Name := f.Name;
  FontName.Font.Color := f.Color;
  FontName.Font.CharSet := f.CharSet;
  FontName.Font.Style := f.Style;
  FontName.Font.Quality:= fqAntialiased;
  FontName.Font.Size := 18;

  FontName.Color := BgColor.ButtonColor;
  FontSize.Value := f.Size;
  FontColor.ButtonColor := f.Color;
end;

procedure TOptionsForm.UpdateFontSize(Sender: TObject);
begin
  f.Size := FontSize.Value;
  UpdateFonts(Sender);
end;

procedure TOptionsForm.UpdateFontColor(Sender: TObject);
begin
  f.Color := FontColor.ButtonColor;
  UpdateFonts(Sender);
end;

procedure TOptionsForm.GetAppFile(Sender: TObject);
begin
  NotifyRunApp.Text := GetFile(NotifyRunApp.Text, 'Executables (*.exe)|*.exe|All files (*.*)|*.*', GetCurrentDir);
end;

function TOptionsForm.GetFile(Orig: String; FilterStr: String; Dir : String): String;
var
  dlg : TOpenDialog;
  fname : String;
  cwd : String;
  relPath : String;
begin
  dlg := TOpenDialog.Create(nil);
  dlg.FileName := '';
  if DirectoryExists(Dir)
  then dlg.InitialDir := Dir
  else dlg.InitialDir := GetCurrentDir;
  dlg.Options := [ofFileMustExist];
  dlg.Filter := FilterStr;
  dlg.FilterIndex := 1;

  if dlg.Execute then
  begin
    fname := dlg.FileName;
    cwd := GetCurrentDir + '\';
  	relPath := ExtractRelativePath(cwd, ExtractFilePath(fname));
    Result := '.\' + relPath + ExtractFileName(fname);
  end else begin
  	Result := Orig;
  end;
  dlg.Free;
end;

procedure TOptionsForm.EnableFields(Sender: TObject);
begin
	NotifyMsg.Enabled := NotifyMsgOn.Checked;
  NotifyMsgTest.Enabled := NotifyMsgOn.Checked;
  NotifyTrayMsg.Enabled := NotifyTrayMsgOn.Checked;
  NotifyTrayMsgTest.Enabled := NotifyTrayMsgOn.Checked;

  NotifyAudio.Enabled := NotifyAudioOn.Checked;
  NotifyAudioBtn.Enabled := NotifyAudioOn.Checked;
  NotifyAudioTest.Enabled := NotifyAudioOn.Checked;

  NotifyRunApp.Enabled := NotifyRunAppOn.Checked;
  NotifyRunBtn.Enabled := NotifyRunAppOn.Checked;
  NotifyRunTest.Enabled := NotifyRunAppOn.Checked;
end;

procedure TOptionsForm.SetDefaults(Sender: TObject);
begin
  (Self.Owner as TMainForm).SetDefaults(Self);
  UpdateFonts(Sender);
end;

procedure TOptionsForm.TestAudio(Sender: TObject);
begin
with Self.Owner as TMainForm do
  PlayAudio(NotifyAudio.Text, False);
end;

procedure TOptionsForm.TestMessage(Sender: TObject);
begin
with Self.Owner as TMainForm do
  ShowDoneMessage(NotifyMsg.Text);
end;

procedure TOptionsForm.TestRunApp(Sender: TObject);
begin
with Self.Owner as TMainForm do
  RunApp(NotifyRunApp.Text);
end;

procedure TOptionsForm.TestTrayMsg(Sender: TObject);
begin
with Self.Owner as TMainForm do
  ShowTrayMessage(NotifyTrayMsg.Text);
end;

initialization
  {$I options.lrs}
end.


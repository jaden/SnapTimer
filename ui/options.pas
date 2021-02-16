unit Options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, LResources, Forms, Graphics, Dialogs, StdCtrls, ComCtrls, Spin, Classes;

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
    FontPreview: TStaticText;
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
    procedure FormCreate(Sender: TObject);
    procedure EnableFields(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure GetAudioFile(Sender: TObject);
    procedure GetAppFile(Sender: TObject);
    function GetFile(Orig: String; FilterStr: String; Dir : String): String;
    procedure SetFontDefaults(Sender: TObject);
    procedure TestAudio(Sender: TObject);
    procedure TestMessage(Sender: TObject);
    procedure TestRunApp(Sender: TObject);
    procedure TestTrayMsg(Sender: TObject);
    procedure UpdateFontColor(Sender: TObject);
    procedure UpdateFontBgColor(Sender: TObject);
    procedure UpdateFontSize(Sender: TObject);
  private
  public
    { public declarations }
  end; 

var
  OptionsForm: TOptionsForm;
//  f: TFont; // TODO make private

implementation

uses Controls, MainForm1, Config;
{ TOptionsForm }

const
  FONT_PREVIEW_FONT_SIZE = 18;

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
  dlg.Font:= FontPreview.Font;
  dlg.Font.Size:= FontSize.Value;
  if dlg.Execute then
  begin
    FontSize.Value:= Dlg.Font.Size;
    FontColor.ButtonColor:= Dlg.Font.Color;
    FontPreview.Caption:= Dlg.Font.Name + ' : 1234567890';
    FontPreview.Font:= Dlg.Font;
    FontPreview.Font.Size:= FONT_PREVIEW_FONT_SIZE;
  end;
  dlg.Free;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
var Config: TConfig;
    WndPositions: TStrings;
begin
  Config:= GetConfig;

  PageControl1.TabIndex := 0;

  // Main
  WndPositions := TStringList.Create;
  WndPositions.Add('Centered');
  WndPositions.Add('Remember position');
  WndPositions.Add('Top left');
  WndPositions.Add('Top right');
  WndPositions.Add('Bottom left');
  WndPositions.Add('Bottom right');
  PositionCombo.Items     := WndPositions;
  PositionCombo.ItemIndex := Ord(Config.WndPosition);

  CheckAlwaysOnTop.Checked:= Config.AlwaysOnTop;
  CheckMinToTray.Checked:= Config.MinToTray;
  CheckAutoSave.Checked:= Config.AutoSave;

  CheckHideSeconds.Checked:= Config.HideSeconds;
  CheckAutostart.Checked:= Config.AutoStart;
  CheckClicktime.Checked:= Config.ClickTime;
  CheckDblClickTime.Checked:= Config.DblClickTime;

  CheckSecondsMode.Checked:= Config.SecondsMode;
  CheckAutoRestart.Checked:= Config.AutoRestart;
  CheckLoopAudio.Checked:= Config.LoopAudio;
  CheckTicking.Checked:= Config.TickingOn;

  // Alarms
  NotifyMsg.Text:= Config.DoneMessage;
  NotifyMsgOn.Checked:= Config.DoneMessageEnabled;

  NotifyTrayMsg.Text:= Config.DoneTrayMsg;
  NotifyTrayMsgOn.Checked:= Config.DoneTrayMsgEnabled;

  NotifyAudio.Text:= Config.DoneAudio;
  NotifyAudioOn.Checked:= Config.DoneAudioEnabled;

  NotifyRunApp.Text:= Config.DoneApp;
  NotifyRunAppOn.Checked:= Config.DoneAppEnabled;

  // Fonts
  FontSize.Value:= Config.Font.Size;
  FontColor.ButtonColor:= Config.Font.Color;
  BgColor.ButtonColor:= Config.Font.BgColor;
  FontPreview.Caption:= Config.Font.Name + ' : 1234567890';
  FontPreview.Font.Name:= Config.Font.Name;
  //FontPreview.Font.Size:= Config.Font.Size; // size is fixed
  FontPreview.Font.Size:= FONT_PREVIEW_FONT_SIZE;
  FontPreview.Font.CharSet:= Config.Font.Charset;
  FontPreview.Font.Color:= Config.Font.Color;
  FontPreview.Font.Style:= Config.Font.Style;
  FontPreview.Color:= Config.Font.BgColor;

  EnableFields(Sender);
end;

procedure TOptionsForm.FormDestroy(Sender: TObject);
var Config : TConfig;
begin
  if ModalResult = mrOk then
  begin
    // Update Config
    Config:= GetConfig;
    // Main
    Config.AlwaysOnTop:= CheckAlwaysOnTop.Checked;
    Config.MinToTray:= CheckMinToTray.Checked;
    Config.AutoStart:= CheckAutostart.Checked;
    Config.HideSeconds:= CheckHideSeconds.Checked;
    Config.ClickTime:= CheckClicktime.Checked;
    Config.DblClickTime:= CheckDblClickTime.Checked;
    Config.AutoRestart:= CheckAutoRestart.Checked;
    Config.LoopAudio:= CheckLoopAudio.Checked;
    Config.TickingOn:= CheckTicking.Checked;
    Config.AutoSave:= CheckAutoSave.Checked;
    Config.SecondsMode:= CheckSecondsMode.Checked;

    // Placement
    Config.WndPosition:= TPosition(PositionCombo.ItemIndex);

    // Alarms
    Config.DoneMessage:= NotifyMsg.Text;
    Config.DoneMessageEnabled:= NotifyMsgOn.Checked;
    Config.DoneTrayMsg:= NotifyTrayMsg.Text;
    Config.DoneTrayMsgEnabled:= NotifyTrayMsgOn.Checked;
    Config.DoneAudio:= NotifyAudio.Text;
    Config.DoneAudioEnabled:= NotifyAudioOn.Checked;
    Config.DoneApp:= NotifyRunApp.Text;
    Config.DoneAppEnabled:= NotifyRunAppOn.Checked;

    // Fonts
    Config.Font.Name:= FontPreview.Font.Name;
    Config.Font.Charset:= FontPreview.Font.CharSet;
    Config.Font.Color:= FontPreview.Font.Color;
    Config.Font.BgColor:= FontPreview.Color;
    Config.Font.Size:= FontSize.Value; // FontPreview has fixed size
    Config.Font.Style:= FontPreview.Font.Style;
  end;
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


procedure TOptionsForm.UpdateFontSize(Sender: TObject);
begin
  //FontPreview.Font.Size:= FontSize.Value;
end;

procedure TOptionsForm.UpdateFontColor(Sender: TObject);
begin
  FontPreview.Font.Color:= FontColor.ButtonColor;
end;

procedure TOptionsForm.UpdateFontBgColor(Sender: TObject);
begin
  FontPreview.Color:=  BgColor.ButtonColor;

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
  if DirectoryExists(Dir) then
     dlg.InitialDir := Dir
  else
    dlg.InitialDir := GetCurrentDir;
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



procedure TOptionsForm.SetFontDefaults(Sender: TObject);
var
  DefFont : TFontConfig;
begin
  DefFont:= GetConfig.GetDefaultFont;

  FontSize.Value:= DefFont.Size;
  FontColor.ButtonColor:= DefFont.Color;
  BgColor.ButtonColor:= DefFont.BgColor;
  FontPreview.Caption:= DefFont.Name + ' : 1234567890';
  FontPreview.Font.Name:= DefFont.Name;
  //FontPreview.Font.Size:= DefFont.Size;
  FontPreview.Font.CharSet:= DefFont.Charset;
  FontPreview.Font.Color:= DefFont.Color;
  FontPreview.Font.Style:= DefFont.Style;
  FontPreview.Color:= DefFont.BgColor;
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


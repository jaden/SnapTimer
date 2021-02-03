unit MainForm1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, StdCtrls, Spin, ExtCtrls, About, LCLType, IniFiles, Settings, MMSystem,
  Windows, StrUtils, DateUtils;

type
  TMode = (Timer, Stopwatch);
  TPosition = (Center, Remember, TopLeft, TopRight, BottomLeft, BottomRight);

  { TMainForm }

  TMainForm = class(TForm)
    ImgIconMain: TImage;
    ImgIconRunning: TImage;
    ImgIconPaused: TImage;
    ImgIconDone: TImage;
    ImgStart: TImage;
    ImgPause: TImage;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuCompact: TMenuItem;
    MenuReset: TMenuItem;
    MenuToggle: TMenuItem;
    MenuSave: TMenuItem;
    PopupMenuCompact: TPopupMenu;
    ImgReset: TImage;
    TrayMenuOptions: TMenuItem;
    TrayMenuShow: TMenuItem;
    TrayMenuToggle: TMenuItem;
    TrayMenuReset: TMenuItem;
    TrayMenuExit: TMenuItem;
    MenuCount: TMenuItem;
    TrayMenuAbout: TMenuItem;
    MenuOptions: TMenuItem;
    MenuHelp: TMenuItem;
    MenuAbout: TMenuItem;
    MenuFile: TMenuItem;
    MenuExit: TMenuItem;
    MenuEdit: TMenuItem;
    TrayMenu: TPopupMenu;
    TimeLabel: TLabel;
    Minutes: TSpinEdit;
    Count:   TStaticText;
    Timer1:  TTimer;
    TrayIconMain: TTrayIcon;
    procedure FormActivate(Sender: TObject);
    procedure ToggleCompact(Sender: TObject);
    procedure MinutesChanged(Sender: TObject);
    procedure OnDestroyForm(Sender: TObject);
    procedure OnShowForm(Sender: TObject);
    procedure ShowAbout(Sender: TObject);
    procedure CloseApp(Sender: TObject);
    procedure OnCreateForm(Sender: TObject);
    procedure Countdown(Sender: TObject);
    procedure ResetCountdown(Sender: TObject);
    procedure DisableTimer();
    procedure EnableTimer();
    procedure SetTimer();
    procedure ShowOptions(Sender: TObject);
    procedure ToggleCountdown(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    function SecondsToTime(Seconds: integer): string;
    procedure FormWindowStateChange(Sender: TObject);
    procedure UpdateTime();
    procedure SaveSettings(Sender: TObject);
    class function GetFilePath(Path: string): string;
    procedure PlayAudio(Path: string; Loop: boolean);
    procedure StopAudio();
    procedure RunApp(Path: string);
    procedure ShowDoneMessage(Msg: string);
    procedure ShowTrayMessage(Msg: string);
    function GetHandle(): HWND;
    procedure SetDefaults(Sender: TObject);
    function FontStylesToInt(Fnt: TFont): integer;
    function IntToFontStyles(Mask: integer): TFontStyles;
    procedure SetFieldsVisible(showFields: boolean);
    procedure UpdateAlwaysOnTop(onTop: boolean);
    procedure PlayTicking();
    function IsInteger(S: String): boolean;
    procedure UpdateTimeCaption(newSecondsMode: boolean);
  private
    { private declarations }
    IniFile: TIniFile;
    AlwaysOnTop: boolean;
    MinToTray: boolean;
    AutoStart: boolean;
    HideSeconds: boolean;
    ClickTime: boolean;
    DblClickTime: boolean;
    AutoRestart: boolean;
    LoopAudio: boolean;
    AudioPlaying: boolean;
    TickingOn: boolean;
    AutoSave: boolean;
    DoneMessage: string;
    DoneMessageEnabled: boolean;
    DoneTrayMsg: string;
    DoneTrayMsgEnabled: boolean;
    DoneAudio: string;
    DoneAudioEnabled: boolean;
    DoneApp: string;
    DoneAppEnabled: boolean;
    WndPosition: TPosition;
    WndPositions: TStrings;
    WndHeight: integer;
    WndWidth: integer;
    WndTop: integer;
    WndLeft: integer;
    WndHandle: HWND;
    SecondsMode: boolean; // Undocumented mode to treat minutes as seconds
    Mode: TMode;
    CompactMode: boolean;
    FontSizeChanged : boolean;
    //FormerWidth : integer;
    //FormerHeight : integer;
    EndTime: TDateTime;
    StartTime: TDateTime;
    CountdownDone: boolean;
  public
    { public declarations }
  end;

var
  MainForm:    TMainForm;
  AppName:     string;
  IniFilename: string;

implementation

uses msgbox;

{ TMainForm }

const
  APP_NAME      = 'SnapTimer';
  INI_SEC_MAIN  = 'Main';
  INI_SEC_ALARMS = 'Alarms';
  INI_SEC_PLACEMENT = 'Placement';
  INI_SEC_FONTS = 'Fonts';
  INI_MINUTES  = 'Minutes';
  INI_ALWAYS_ON_TOP = 'AlwaysOnTop';
  INI_MIN_TO_TRAY = 'MinToTray';
  INI_AUTOSTART = 'AutoStart';
  INI_HIDESECONDS = 'HideSeconds';
  INI_CLICKTIME = 'ClickTime';
  INI_DBLCLICKTIME = 'DoubleClickTime';
  INI_AUTORESTART = 'AutoRestart';
  INI_LOOP_AUDIO = 'LoopAudio';
  INI_TICKING_ON = 'TickingEnabled';
  INI_AUTOSAVE = 'AutoSave';
  INI_HEIGHT   = 'WinHeight';
  INI_WIDTH    = 'WinWidth';
  INI_LEFT     = 'WinLeft';
  INI_TOP      = 'WinTop';
  INI_POSITION = 'WinPosition';
  INI_DONE_MESSAGE = 'Message';
  INI_DONE_MESSAGE_ON = 'MessageEnabled';
  INI_DONE_TRAY_MESSAGE = 'TrayMessage';
  INI_DONE_TRAY_MESSAGE_ON = 'TrayMessageEnabled';
  INI_DONE_AUDIO = 'AudioFile';
  INI_DONE_AUDIO_ON = 'AudioFileEnabled';
  INI_DONE_APP = 'RunApp';
  INI_DONE_APP_ON = 'RunAppEnabled';
  INI_SECONDS_MODE = 'SecondsMode';
  INI_FONT_NAME = 'Name';
  INI_FONT_CHARSET = 'Charset';
  INI_FONT_COLOR = 'Color';
  INI_FONT_SIZE = 'Size';
  INI_FONT_STYLE = 'Style';
  INI_BG_COLOR = 'BgColor';

  // Menu items
  MENU_FILE      = '&File';
  MENU_EXIT      = 'E&xit';
  MENU_EDIT      = '&Edit';
  MENU_OPTIONS   = 'Op&tions';
  MENU_HELP      = '&Help';
  MENU_ABOUT     = '&About';
  TRAY_MENU_SHOW = 'Show application';

  // Buttons
  BTN_START = '&Start';
  BTN_PAUSE = '&Pause';
  BTN_RESET = '&Reset';

  // Labels
  LBL_MINUTES = '&Minutes:';
  LBL_SECONDS = '&Seconds:';

  // Default values
  DEF_TIME      = 15;
  DEF_DONE_MSG  = 'Time''s up';
  DEF_DONE_MSG_ON = True;
  DEF_DONE_TRAY_MSG = 'Countdown completed';
  DEF_DONE_TRAY_MSG_ON = False;
  DEF_DONE_AUDIO = '.\sounds\alarm_clock_bell.wav';
  DEF_DONE_AUDIO_ON = False;
  DEF_DONE_APP  = '';
  DEF_DONE_APP_ON = False;
  DEF_ALWAYS_ON_TOP = False;
  DEF_MIN_TO_TRAY = False;
  DEF_AUTOSTART = False;
  DEF_HIDESECONDS = False;
  DEF_CLICKTIME = True;
  DEF_DBLCLICKTIME = True;
  DEF_AUTORESTART = False;
  DEF_LOOP_AUDIO = False;
  DEF_TICKING_ON = False;
  DEF_TICKING_PATH = '.\sounds\ticking\ticking.wav';
  DEF_AUTOSAVE  = True;
  DEF_SECONDS_MODE = False;
  DEF_POSITION  = Ord(Center);
  DEF_HEIGHT    = 149;
  DEF_WIDTH     = 214;
  DEF_FONT_NAME = 'Arial';
  DEF_FONT_CHARSET = 0;
  DEF_FONT_COLOR = clNavy;
  DEF_FONT_SIZE = 38;
  DEF_FONT_STYLE = 0;
  DEF_BG_COLOR  = clNone;

  // Messages
  MSG_OPEN_INI  = 'An error occurred opening the .ini file, settings won''t be saved';
  MSG_WRITE_INI =
    'An error occurred trying to write to the .ini file, settings won''t be saved.';

procedure TMainForm.OnCreateForm(Sender: TObject);
begin
  MenuFile.Caption    := MENU_FILE;
  MenuToggle.Caption  := BTN_START;
  MenuReset.Caption   := BTN_RESET;
  MenuExit.Caption    := MENU_EXIT;
  MenuEdit.Caption    := MENU_EDIT;
  MenuOptions.Caption := MENU_OPTIONS;
  MenuHelp.Caption    := MENU_HELP;
  MenuAbout.Caption   := MENU_ABOUT;

  TrayMenuShow.Caption   := TRAY_MENU_SHOW;
  TrayMenuToggle.Caption := BTN_START;
  TrayMenuReset.Caption  := BTN_RESET;
  TrayMenuAbout.Caption  := MENU_ABOUT;
  TrayMenuExit.Caption   := MENU_EXIT;

  // Set all defaults, then load from .ini
  Minutes.Value := DEF_TIME;
  AlwaysOnTop := DEF_ALWAYS_ON_TOP;
  MinToTray   := DEF_MIN_TO_TRAY;
  AutoStart   := DEF_AUTOSTART;
  HideSeconds := DEF_HIDESECONDS;
  ClickTime   := DEF_CLICKTIME;
  DblClickTime := DEF_DBLCLICKTIME;
  AutoRestart := DEF_AUTORESTART;
  LoopAudio   := DEF_LOOP_AUDIO;
  TickingOn   := DEF_TICKING_ON;
  AutoSave    := DEF_AUTOSAVE;
  DoneMessage := DEF_DONE_MSG;
  DoneMessageEnabled := DEF_DONE_MSG_ON;
  DoneTrayMsg := DEF_DONE_TRAY_MSG;
  DoneTrayMsgEnabled := DEF_DONE_TRAY_MSG_ON;
  DoneAudio   := DEF_DONE_AUDIO;
  DoneAudioEnabled := DEF_DONE_AUDIO_ON;
  DoneApp     := DEF_DONE_APP;
  DoneAppEnabled := DEF_DONE_APP_ON;
  SecondsMode := DEF_SECONDS_MODE;
  WndPosition := TPosition(DEF_POSITION);

  WndPositions := TStringList.Create;
  WndPositions.Add('Centered');
  WndPositions.Add('Remember position');
  WndPositions.Add('Top left');
  WndPositions.Add('Top right');
  WndPositions.Add('Bottom left');
  WndPositions.Add('Bottom right');

  Mode := Timer;
  CompactMode := False;
  AudioPlaying := False;
  Self.DoubleBuffered := True;
  Count.DoubleBuffered := True;
  Timer1.Interval := 150;
  CountdownDone := False;

  AppName := ExtractFileName(Application.ExeName);
  IniFilename := ExtractFilePath(Application.ExeName) + ChangeFileExt(AppName, '.ini');

  // TODO For some reason this isn't working at all
  //WriteLn('checking for options: ' + IntToStr(ParamCount));
  //If ParamCount > 1 then begin
  //	WriteLn('has options');
  //if Application.HasOption('h', 'help') then begin
  //  WriteLn('SnapTimer usage');
  //  WriteLn('[number of minutes] (must be the only parameter)');
  //  WriteLn('-h|--help\tShow this usage');
  //  WriteLn('-i|--ini=<filename.ini>\tPath to .ini file to use');
  //end else if Application.HasOption('i', 'ini') then begin
  //  //IniFilename := Application.GetOptionValue('i', 'ini');
  //  WriteLn('ini option found');
  //end;
  //end;
  try
    IniFile := TIniFile.Create(IniFilename);
    Minutes.Value := IniFile.ReadInteger(INI_SEC_MAIN, INI_MINUTES, DEF_TIME);
    AlwaysOnTop := IniFile.ReadBool(INI_SEC_MAIN, INI_ALWAYS_ON_TOP, DEF_ALWAYS_ON_TOP);
    MinToTray := IniFile.ReadBool(INI_SEC_MAIN, INI_MIN_TO_TRAY, DEF_MIN_TO_TRAY);
    AutoStart := IniFile.ReadBool(INI_SEC_MAIN, INI_AUTOSTART, DEF_AUTOSTART);
    HideSeconds := IniFile.ReadBool(INI_SEC_MAIN, INI_HIDESECONDS, DEF_HIDESECONDS);
    ClickTime := IniFile.ReadBool(INI_SEC_MAIN, INI_CLICKTIME, DEF_CLICKTIME);
    DblClickTime := IniFile.ReadBool(INI_SEC_MAIN, INI_DBLCLICKTIME, DEF_DBLCLICKTIME);
    AutoRestart := IniFile.ReadBool(INI_SEC_MAIN, INI_AUTORESTART, DEF_AUTORESTART);
    LoopAudio := IniFile.ReadBool(INI_SEC_MAIN, INI_LOOP_AUDIO, DEF_LOOP_AUDIO);
    TickingOn := IniFile.ReadBool(INI_SEC_MAIN, INI_TICKING_ON, DEF_TICKING_ON);
    AutoSave := IniFile.ReadBool(INI_SEC_MAIN, INI_AUTOSAVE, DEF_AUTOSAVE);
    SecondsMode := IniFile.ReadBool(INI_SEC_MAIN, INI_SECONDS_MODE, DEF_SECONDS_MODE);
    DoneMessage := IniFile.ReadString(INI_SEC_ALARMS, INI_DONE_MESSAGE, DEF_DONE_MSG);
    DoneMessageEnabled := IniFile.ReadBool(INI_SEC_ALARMS, INI_DONE_MESSAGE_ON,
      DEF_DONE_MSG_ON);
    DoneTrayMsg := IniFile.ReadString(INI_SEC_ALARMS, INI_DONE_TRAY_MESSAGE,
      DEF_DONE_TRAY_MSG);
    DoneTrayMsgEnabled := IniFile.ReadBool(INI_SEC_ALARMS,
      INI_DONE_TRAY_MESSAGE_ON, DEF_DONE_TRAY_MSG_ON);
    DoneAudio := IniFile.ReadString(INI_SEC_ALARMS, INI_DONE_AUDIO, DEF_DONE_AUDIO);
    DoneAudioEnabled := IniFile.ReadBool(INI_SEC_ALARMS, INI_DONE_AUDIO_ON,
      DEF_DONE_AUDIO_ON);
    DoneApp := IniFile.ReadString(INI_SEC_ALARMS, INI_DONE_APP, DEF_DONE_APP);
    DoneAppEnabled := IniFile.ReadBool(INI_SEC_ALARMS, INI_DONE_APP_ON, DEF_DONE_APP_ON);
    WndHeight := IniFile.ReadInteger(INI_SEC_PLACEMENT, INI_HEIGHT, DEF_HEIGHT);
    WndWidth := IniFile.ReadInteger(INI_SEC_PLACEMENT, INI_WIDTH, DEF_WIDTH);
    WndPosition := TPosition(IniFile.ReadInteger(INI_SEC_PLACEMENT, INI_POSITION, DEF_POSITION));
    WndLeft := IniFile.ReadInteger(INI_SEC_PLACEMENT, INI_LEFT, 0);
    WndTop := IniFile.ReadInteger(INI_SEC_PLACEMENT, INI_TOP, 0);

    Count.Font.Quality := fqAntialiased;
    Count.Font.Name := IniFile.ReadString(INI_SEC_FONTS, INI_FONT_NAME,
      DEF_FONT_NAME);
    Count.Font.CharSet := IniFile.ReadInteger(INI_SEC_FONTS, INI_FONT_CHARSET,
      DEF_FONT_CHARSET);
    Count.Font.Color := IniFile.ReadInteger(INI_SEC_FONTS, INI_FONT_COLOR,
      DEF_FONT_COLOR);
    Count.Font.Size := IniFile.ReadInteger(INI_SEC_FONTS, INI_FONT_SIZE,
      DEF_FONT_SIZE);
    Count.Font.Style := IntToFontStyles(IniFile.ReadInteger(INI_SEC_FONTS,
      INI_FONT_STYLE, DEF_FONT_STYLE));
    Count.Color := IniFile.ReadInteger(INI_SEC_FONTS, INI_BG_COLOR, DEF_BG_COLOR);

  except
    MessageDlg(MSG_OPEN_INI, mtError, [mbOK], 0);
  end;
  if Assigned(IniFile) then
    IniFile.Free;

  // Use minutes argument if it's the only parameter and it's numeric
  if (ParamCount = 1) and (IsInteger(ParamStr(1))) then
    Minutes.Value := StrToInt(ParamStr(1));

  UpdateTimeCaption(SecondsMode);
  ResetCountdown(Sender);
  if AutoStart then
    ToggleCountdown(Sender);
end;

procedure TMainForm.ResetCountdown(Sender: TObject);
begin
  if (Sender.ClassName = Count.ClassName) and (not DblClickTime) then
    Exit;
  // Turn off both looping audio and ticking
  StopAudio();
  DisableTimer();
  SetTimer();
  TrayIconMain.Icon := ImgIconMain.Picture.Icon;
end;

procedure TMainForm.DisableTimer();
begin
  Timer1.Enabled     := False;
  ImgPause.Visible   := False;
  ImgStart.Visible   := True;
  MenuToggle.Caption := BTN_START;
  TrayMenuToggle.Caption := BTN_START;
  StopAudio();
end;

procedure TMainForm.EnableTimer();
begin
  Timer1.Enabled     := True;
  ImgPause.Visible   := True;
  ImgStart.Visible   := False;
  MenuToggle.Caption := BTN_PAUSE;
  TrayMenuToggle.Caption := BTN_PAUSE;
  TrayIconMain.Icon  := ImgIconRunning.Picture.Icon;

  if TickingOn then
    PlayTicking();
end;

procedure TMainForm.SetTimer();
begin
  if SecondsMode then
    Timer1.Tag := Minutes.Value
  else
    Timer1.Tag := Minutes.Value * 60;
  UpdateTime();
end;

procedure TMainForm.ShowOptions(Sender: TObject);
begin
  OptionsForm := TOptionsForm.Create(Self);
  with OptionsForm do
  begin
    NotifyMsg.Text      := DoneMessage;
    NotifyMsgOn.Checked := DoneMessageEnabled;

    NotifyTrayMsg.Text      := DoneTrayMsg;
    NotifyTrayMsgOn.Checked := DoneTrayMsgEnabled;

    NotifyAudio.Text      := DoneAudio;
    NotifyAudioOn.Checked := DoneAudioEnabled;

    NotifyRunApp.Text      := DoneApp;
    NotifyRunAppOn.Checked := DoneAppEnabled;

    CheckAlwaysOnTop.Checked  := AlwaysOnTop;
    CheckMinToTray.Checked    := MinToTray;
    CheckAutostart.Checked    := AutoStart;
    CheckHideSeconds.Checked  := HideSeconds;
    CheckClicktime.Checked    := ClickTime;
    CheckDblClickTime.Checked := DblClickTime;
    CheckAutoRestart.Checked  := AutoRestart;
    CheckLoopAudio.Checked    := LoopAudio;
    CheckTicking.Checked      := TickingOn;
    CheckAutoSave.Checked     := AutoSave;
    CheckSecondsMode.Checked  := SecondsMode;

    PositionCombo.Items     := WndPositions;
    PositionCombo.ItemIndex := Ord(WndPosition);

    PageControl1.TabIndex := 0;

    f      := TFont.Create;
    f.Name := Count.Font.Name;
    f.Size := Count.Font.Size;
    f.CharSet := Count.Font.CharSet;
    f.Color := Count.Font.Color;
    f.Style := Count.Font.Style;
    BgColor.ButtonColor := Count.Color;
    FontSizeChanged := False;

    if ShowModal = mrOk then
    begin
      UpdateTimeCaption(CheckSecondsMode.Checked);

      DoneMessage  := NotifyMsg.Text;
      DoneMessageEnabled := NotifyMsgOn.Checked;
      DoneTrayMsg  := NotifyTrayMsg.Text;
      DoneTrayMsgEnabled := NotifyTrayMsgOn.Checked;
      DoneAudio    := NotifyAudio.Text;
      DoneAudioEnabled := NotifyAudioOn.Checked;
      DoneApp      := NotifyRunApp.Text;
      DoneAppEnabled := NotifyRunAppOn.Checked;
      AlwaysOnTop  := CheckAlwaysOnTop.Checked;
      MinToTray    := CheckMinToTray.Checked;
      AutoStart    := CheckAutostart.Checked;
      HideSeconds  := CheckHideSeconds.Checked;
      ClickTime    := CheckClicktime.Checked;
      DblClickTime := CheckDblClickTime.Checked;
      AutoRestart  := CheckAutoRestart.Checked;
      LoopAudio    := CheckLoopAudio.Checked;
      TickingOn    := CheckTicking.Checked;
      AutoSave     := CheckAutoSave.Checked;
      SecondsMode  := CheckSecondsMode.Checked;
      WndPosition  := TPosition(PositionCombo.ItemIndex);
      //if not (Count.Font.Size = f.Size) then
      //begin
      	//FontSizeChanged := True;
        //GetPreferredSize(FormerWidth, FormerHeight, True);
        //ShowMessageFmt('width: %d, height: %d', [FormerWidth, FormerHeight]);
      //end;
      //Count.Font  := f;
      // TODO Get font colors and background color to be shown - what's up?
      Count.Font.Name := f.Name;
      Count.Font.Size := f.Size;
      Count.Font.CharSet := f.CharSet;
      Count.Font.Color := f.Color;
      Count.Font.Style := f.Style;
      Count.Font.Quality := fqAntialiased;
      Count.Color := BgColor.ButtonColor;

      //if FontSizeChanged then OnShowForm(Sender);

      UpdateAlwaysOnTop(AlwaysOnTop);

      if Timer1.Enabled then
      begin
        if TickingOn then
          PlayTicking
        else
          StopAudio();
      end;

      if AutoSave then
        SaveSettings(Sender);
      // TODO Resize the window if necessary to accomodate larger text
      // Get the text size from the font for '00:00:00'?

    end;
  end;
  OptionsForm.Free;
end;

procedure TMainForm.ToggleCountdown(Sender: TObject);
begin
  if (Sender.ClassName = Count.ClassName) and (not ClickTime) then
    Exit;

  if Minutes.Value = 0 then
  begin
    Mode := Stopwatch;
    StartTime := Now - (Timer1.Tag * OneSecond);
  end
  else
  begin
    Mode := Timer;
    EndTime := Now + (Timer1.Tag * OneSecond);
  end;

  if Timer1.Enabled then
  begin
    TrayIconMain.Icon := ImgIconPaused.Picture.Icon;
    DisableTimer();
  end
  else
  begin
    if CountdownDone then
    begin
        // TODO When restarting after countdown has elapsed, quickly jumps from
        // starting number to next one - fix?
        CountdownDone := False;
        ResetCountdown(Sender);
        EndTime := Now + (Timer1.Tag * OneSecond);
    end;
    EnableTimer();
  end
end;

procedure TMainForm.Countdown(Sender: TObject);
begin
  if MODE = Stopwatch then
  begin
    Timer1.Tag := SecondsBetween(StartTime, Now);
    UpdateTime();
  end
  else
  begin
    if EndTime <= Now then
    begin
      Timer1.Tag := 0;
      UpdateTime();
      DisableTimer();
      CountdownDone := True;
      Application.Title := APP_NAME;
      if DoneAudioEnabled then
        PlayAudio(DoneAudio, LoopAudio);
      if DoneAppEnabled then
        RunApp(DoneApp);
      if DoneTrayMsgEnabled then
        ShowTrayMessage(DoneTrayMsg);
      if DoneMessageEnabled then
        ShowDoneMessage(DoneMessage);

      if AutoRestart then
        ToggleCountdown(Sender)
      else
        TrayIconMain.Icon := ImgIconDone.Picture.Icon;
    end
    else
      UpdateTime();
    Timer1.Tag := SecondsBetween(EndTime, Now);
  end;
end;

procedure TMainForm.ShowAbout(Sender: TObject);
var
  AboutForm: TAboutForm;
begin
  AboutForm := TAboutForm.Create(Self);
  AboutForm.ShowModal;
  AboutForm.Release;
end;

procedure TMainForm.OnDestroyForm(Sender: TObject);
begin
  if AutoSave then
    SaveSettings(Sender);
end;

procedure TMainForm.OnShowForm(Sender: TObject);
var
  //w: integer;
  //h: integer;
  wRect: TRect;
  //cRect: TRect;
  flags: integer;

begin
  Self.Position := poDesigned;
  WndHandle := GetHandle();
  GetWindowRect(WndHandle, wRect);
  flags := SWP_SHOWWINDOW;

  case WndPosition of
       Remember: SetWindowPos(WndHandle, HWND_TOP, WndLeft, WndTop, WndWidth, WndHeight, flags);
       TopLeft: SetWindowPos(WndHandle, HWND_TOP, 0, 0, WndWidth, WndHeight, flags);
       TopRight: SetWindowPos(WndHandle, HWND_TOP, Monitor.WorkareaRect.Right - WndWidth, 0, WndWidth, WndHeight, flags);
       BottomLeft: SetWindowPos(WndHandle, HWND_TOP, 0, Monitor.WorkareaRect.Bottom - WndHeight, WndWidth, WndHeight, flags);
       BottomRight: SetWindowPos(WndHandle, HWND_TOP, Monitor.WorkareaRect.Right - WndWidth,
                    Monitor.WorkareaRect.Bottom - WndHeight, WndWidth, WndHeight, flags);
       Center:
         begin
              SetWindowPos(WndHandle, HWND_TOP, 0, 0, WndWidth, WndHeight, SWP_SHOWWINDOW or SWP_NOMOVE);
              Self.Position := poScreenCenter;
         end;
  end;

  // TODO Get this working for window size (getpreferred size gets form, not window)
  // Get preferred size before font change, then after and add or subtract that delta from the
  // window size?
  // AutoSize is working on startup, just not when font size is changed.
  // Need to find the method that will set the window to the preferred size, because the preferred size is correct
  // setBounds does it, but it also requires
  if FontSizeChanged then
  begin
  	//AutoSize:= True;
    //MainForm.FontChanged(Sender);
 	  //GetPreferredSize(w, h, True);
    //cRect := GetClientRect;
    //cRect.Right := w;
    //cRect.Bottom := h;
    //AdjustClientRect(cRect);

    // These two lines cause an exception - why I don't know
    //Count.Width:= w;
    //Count.Height:= h;

    //InvalidatePreferredSize;
    //Repaint;
    //Resize;
    //AdjustSize;

	  //GetPreferredSize(w, h, True);
   	//ShowMessageFmt('width: %d, height: %d', [w, h]);
    //ShowMessageFmt('width: %d, height: %d', [FormerWidth, FormerHeight]);
    //WndWidth:= WndWidth + (w - FormerWidth);
    //WndHeight:= WndHeight + (h - FormerHeight);
    //SetBounds(Monitor.WorkareaRect.Right - w + rightAdjust, 0, w, h);
    //WndWidth:= w;
    //WndHeight:= h;
  	//ShowMessageFmt('width: %d, height: %d', [w, h]);
    //exit;
  end;

  UpdateAlwaysOnTop(AlwaysOnTop);
end;

procedure TMainForm.SaveSettings(Sender: TObject);
var
  wRect: TRect;
begin
  try
    IniFile := TIniFile.Create(IniFilename);
    IniFile.CacheUpdates := True;
    IniFile.WriteInteger(INI_SEC_MAIN, INI_MINUTES, Minutes.Value);
    IniFile.WriteBool(INI_SEC_MAIN, INI_ALWAYS_ON_TOP, AlwaysOnTop);
    IniFile.WriteBool(INI_SEC_MAIN, INI_MIN_TO_TRAY, MinToTray);
    IniFile.WriteBool(INI_SEC_MAIN, INI_AUTOSTART, AutoStart);
    IniFile.WriteBool(INI_SEC_MAIN, INI_HIDESECONDS, HideSeconds);
    IniFile.WriteBool(INI_SEC_MAIN, INI_CLICKTIME, ClickTime);
    IniFile.WriteBool(INI_SEC_MAIN, INI_DBLCLICKTIME, DblClickTime);
    IniFile.WriteBool(INI_SEC_MAIN, INI_AUTORESTART, AutoRestart);
    IniFile.WriteBool(INI_SEC_MAIN, INI_LOOP_AUDIO, LoopAudio);
    IniFile.WriteBool(INI_SEC_MAIN, INI_TICKING_ON, TickingOn);
    IniFile.WriteBool(INI_SEC_MAIN, INI_AUTOSAVE, AutoSave);
    IniFile.WriteBool(INI_SEC_MAIN, INI_SECONDS_MODE, SecondsMode);

    GetWindowRect(GetHandle(), wRect);

    IniFile.WriteInteger(INI_SEC_PLACEMENT, INI_HEIGHT, wRect.Bottom - wRect.Top);
    IniFile.WriteInteger(INI_SEC_PLACEMENT, INI_WIDTH, wRect.Right - wRect.Left);
    IniFile.WriteInteger(INI_SEC_PLACEMENT, INI_LEFT, wRect.Left);
    IniFile.WriteInteger(INI_SEC_PLACEMENT, INI_TOP, wRect.Top);
    IniFile.WriteInteger(INI_SEC_PLACEMENT, INI_POSITION, Ord(WndPosition));

    IniFile.WriteString(INI_SEC_ALARMS, INI_DONE_MESSAGE, DoneMessage);
    IniFile.WriteBool(INI_SEC_ALARMS, INI_DONE_MESSAGE_ON, DoneMessageEnabled);
    IniFile.WriteString(INI_SEC_ALARMS, INI_DONE_TRAY_MESSAGE, DoneTrayMsg);
    IniFile.WriteBool(INI_SEC_ALARMS, INI_DONE_TRAY_MESSAGE_ON, DoneTrayMsgEnabled);
    IniFile.WriteString(INI_SEC_ALARMS, INI_DONE_AUDIO, DoneAudio);
    IniFile.WriteBool(INI_SEC_ALARMS, INI_DONE_AUDIO_ON, DoneAudioEnabled);
    IniFile.WriteString(INI_SEC_ALARMS, INI_DONE_APP, DoneApp);
    IniFile.WriteBool(INI_SEC_ALARMS, INI_DONE_APP_ON, DoneAppEnabled);

    IniFile.WriteString(INI_SEC_FONTS, INI_FONT_NAME, Count.Font.Name);
    IniFile.WriteInteger(INI_SEC_FONTS, INI_FONT_CHARSET, Count.Font.CharSet);
    IniFile.WriteInteger(INI_SEC_FONTS, INI_FONT_COLOR, Count.Font.Color);
    IniFile.WriteInteger(INI_SEC_FONTS, INI_FONT_SIZE, Count.Font.Size);
    IniFile.WriteInteger(INI_SEC_FONTS, INI_FONT_STYLE, FontStylesToInt(Count.Font));
    IniFile.WriteInteger(INI_SEC_FONTS, INI_BG_COLOR, Count.Color);
    IniFile.UpdateFile;
  except
    MessageDlg(MSG_WRITE_INI, mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.MinutesChanged(Sender: TObject);
begin
  if not Timer1.Enabled then
    ResetCountdown(Sender);
end;

procedure TMainForm.ToggleCompact(Sender: TObject);
begin
  SetFieldsVisible(CompactMode);
  CompactMode := not CompactMode;
end;

 // TODO Figure out how to remove the fields from the form so they don't
 // take up space anymore (and count can expand to top)
 // Easier to create a new form, hide this one and show the other one
 // Set border style to none too.
procedure TMainForm.SetFieldsVisible(showFields: boolean);
begin
  if showFields then
  begin
    MainForm.Menu := MainMenu1;
  end
  else
  begin
    MainForm.Menu    := nil;
    TimeLabel.Visible := showFields;
    Minutes.Visible  := showFields;
    ImgStart.Visible := showFields;
    ImgReset.Visible := showFields;
    ImgPause.Visible := showFields;
  end;
end;

procedure TMainForm.CloseApp(Sender: TObject);
begin
  OnDestroyForm(Sender);
  Close;
end;

function TMainForm.SecondsToTime(Seconds: integer): string;
var
  h, m, s: integer;
begin
  h      := Seconds div 3600;
  m      := Seconds div 60 - h * 60;
  s      := Seconds - (h * 3600 + m * 60);
  if HideSeconds then
  begin
    Result := SysUtils.Format('%.2d:%.2d', [h, m]);
  end
  else
  begin
    Result := SysUtils.Format('%.2d:%.2d:%.2d', [h, m, s]);
  end;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  // TMainForm's KeyPreview property must be set to True for this to work.
  if key = #27 then
    Close; // 27 = Escape
  if (Minutes.Focused) and (key = #13) then
    ToggleCountdown(Sender);
end;

 // TODO Animate icon between alarm icon and main so it blinks every second
 // Instead of playing icon, animate the clock hands so they go around?
 // http://delphi.about.com/od/kbwinshell/l/aa122501a.htm
procedure TMainForm.UpdateTime();
begin
  // Changing caption while it's not visible keeps time from flashing
  Count.Visible := False;
  Count.Caption := SecondsToTime(Timer1.Tag);
  Count.Visible := True;
  MenuCount.Caption := Count.Caption;
  if Timer1.Enabled then
  begin
    Application.Title := Count.Caption + ' - ' + APP_NAME;
    TrayIconMain.Hint := Count.Caption;
  end
  else
  begin
    Application.Title := APP_NAME;
    TrayIconMain.Hint := APP_NAME;
  end;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if not MinToTray then
    Exit;

  if WindowState = wsMinimized then
  begin
    WindowState   := wsNormal;
    ShowInTaskBar := stNever;
    Hide;
  end;
end;

// TODO Interpret env vars in the path
class function TMainForm.GetFilePath(Path: string): string;
begin
  if AnsiStartsStr('.', Path) then
  begin
    Result := ExtractFilePath(Application.ExeName) + Path;
  end else
    Result := Path;
end;

procedure TMainForm.PlayAudio(Path: string; Loop: boolean);
begin
  if AudioPlaying then
    Exit;
  if Loop then
  begin
    AudioPlaying := True;
    sndPlaySound(PChar(GetFilePath(Path)), SND_NODEFAULT or SND_ASYNC or SND_LOOP);
  end
  else
    sndPlaySound(PChar(GetFilePath(Path)), SND_NODEFAULT or SND_ASYNC);
end;

procedure TMainForm.StopAudio();
begin
  sndPlaySound(nil, 0);
  AudioPlaying := False;
end;

procedure TMainForm.PlayTicking();
begin
  sndPlaySound(PChar(GetFilePath(DEF_TICKING_PATH)), SND_NODEFAULT or
    SND_ASYNC or SND_LOOP);
end;

procedure TMainForm.RunApp(Path: string);
begin
  ShellExecute(0, 'open', PChar(GetFilePath(Path)), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.ShowTrayMessage(Msg: string);
begin
  with TrayIconMain do
  begin
    BalloonHint := Msg;
    ShowBalloonHint;
    BalloonTimeout := 4000;
  end;
end;

procedure TMainForm.ShowDoneMessage(Msg: string);
var MsgboxForm : TMsgBoxForm;
begin
  // http://msdn.microsoft.com/en-us/library/ms645505(VS.85).aspx
  //Windows.MessageBox(GetHandle(), pChar(msg), 'Done',
  //   MB_SYSTEMMODAL or MB_SETFOREGROUND or MB_TOPMOST or MB_ICONINFORMATION);
  // Custom MessageBox
  MsgboxForm := TMsgBoxForm.Create(Self);
  MsgBoxForm.SetText(Msg);
  MsgboxForm.ShowModal;
  MsgboxForm.Release;
  StopAudio();
end;

function TMainForm.GetHandle(): HWND;
begin
  Result := Application.MainForm.Handle;
end;

procedure TMainForm.SetDefaults(Sender: TObject);
begin
  with Sender as TOptionsForm do
    BgColor.ButtonColor := DEF_BG_COLOR;
  f.Name    := DEF_FONT_NAME;
  f.Color   := DEF_FONT_COLOR;
  f.CharSet := DEF_FONT_CHARSET;
  f.Size    := DEF_FONT_SIZE;
  f.Style   := IntToFontStyles(DEF_FONT_STYLE);
end;

function TMainForm.FontStylesToInt(Fnt: TFont): integer;
var
  Mask:  integer;
  Style: TFontStyle;
begin
  // Translate the set into a bit mask
  Mask := 0;
  for Style := Low(TFontStyle) to High(TFontStyle) do
    if Style in Fnt.Style then
      Mask := Mask or (1 shl Ord(Style));
  Result   := Mask;
end;

function TMainForm.IntToFontStyles(Mask: integer): TFontStyles;
var
  i: integer;
  StyleSet: TFontStyles;
begin
  // Translate the bit mask into a set
  StyleSet := [];
  for i := 0 to Ord(High(TFontStyle)) do
    if Mask and (1 shl i) <> 0 then
      StyleSet := StyleSet + [TFontStyle(i)];
  Result := StyleSet;
end;

procedure TMainForm.UpdateAlwaysOnTop(onTop: boolean);
begin
  if onTop then
     FormStyle:= fsSystemStayOnTop
  else
     FormStyle:= fsNormal
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  WindowState   := wsNormal;
  ShowInTaskBar := stDefault;
  Show;
end;

function TMainForm.IsInteger(S: String): boolean;
begin
  try
    Result := True;
    StrToInt(S);
  except on E: EConvertError do
    Result := False;
  end;
end;

// TODO Only update the time if this value changed (otherwise it resets
// the time every time you modify settings)
// It's not working - test it thoroughly and fix it.
procedure TMainForm.UpdateTimeCaption(newSecondsMode: boolean);
begin
  if newSecondsMode <> SecondsMode then
  begin
    if newSecondsMode then
      TimeLabel.Caption := LBL_SECONDS
    else
      TimeLabel.Caption := LBL_MINUTES;
    SetTimer();
  end;

end;

initialization
  {$I mainform1.lrs}
end.


unit MainForm1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, StdCtrls, Spin, ExtCtrls, About, LCLType, Options, MMSystem,
  Windows, StrUtils, DateUtils;

type
    TMode = (Timer, Stopwatch);

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
    procedure SetFieldsVisible(showFields: boolean);
    procedure PlayTicking();
    function IsInteger(S: String): boolean;
  private
    { private declarations }
    AudioPlaying: boolean;
    Mode: TMode;
    FontSizeChanged : boolean;
    //FormerWidth : integer;
    //FormerHeight : integer;
    EndTime: TDateTime;
    StartTime: TDateTime;
    CountdownDone: boolean;
    OnShowFormFirstTime: Boolean;
    procedure ApplyConfig;
  public
    { public declarations }
  end;

var
  MainForm:    TMainForm;
  //AppName:     string;


implementation

uses
  Config;

{ TMainForm }

const
  APP_NAME      = 'SnapTimer';

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

  // Messages
  MSG_OPEN_INI  = 'An error occurred opening the .ini file, settings won''t be saved';
  MSG_WRITE_INI =
    'An error occurred trying to write to the .ini file, settings won''t be saved.';

procedure TMainForm.OnCreateForm(Sender: TObject);
var
  Config: TConfig;
begin
  Config:= GetConfig;

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

  Mode := Timer;
  AudioPlaying := False;
  Self.DoubleBuffered := True;
  Count.DoubleBuffered := True;
  Timer1.Interval := 150;
  CountdownDone := False;
  OnShowFormFirstTime:= True;

  //AppName := ExtractFileName(Application.ExeName);

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

  if Config.Load = False then
  begin
    // When will this code execute?
    MessageDlg(MSG_OPEN_INI, mtError, [mbOK], 0);
    Exit;
  end;
  Minutes.Value:= Config.Minutes;

  // Use minutes argument if it's the only parameter and it's numeric
  if (ParamCount = 1) and (IsInteger(ParamStr(1))) then
    Minutes.Value := StrToInt(ParamStr(1));

  //UpdateTimeCaption(SecondsMode); ApplyConfig will do this
  ResetCountdown(Sender);
  if Config.AutoStart then
    ToggleCountdown(Sender);
end;

procedure TMainForm.ResetCountdown(Sender: TObject);
begin
  if (Sender.ClassName = Count.ClassName) and (not GetConfig.DblClickTime) then
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

  if GetConfig.TickingOn then
    PlayTicking();
end;

procedure TMainForm.SetTimer();
begin
  if GetConfig.SecondsMode then
    Timer1.Tag := Minutes.Value
  else
    Timer1.Tag := Minutes.Value * 60;
  UpdateTime();
end;

procedure TMainForm.ShowOptions(Sender: TObject);
var
  Config: TConfig;
  Ok: Boolean;
  r: TRect;
begin
  OptionsForm := TOptionsForm.Create(Self);
  Ok:= OptionsForm.ShowModal = mrOk;
  OptionsForm.Free;

  if Ok then
  begin
    Config:= GetConfig;
    GetWindowRect(MainForm.Handle, r);
    Config.WndLeft:= r.Left;
    Config.WndTop:= r.Top;
    Config.WndWidth:= r.Right - r. Left;
    Config.WndHeight:= r.Bottom - r.Top;
    ApplyConfig;

    //if not (Count.Font.Size = f.Size) then
    //begin
      //FontSizeChanged := True;
      //GetPreferredSize(FormerWidth, FormerHeight, True);
      //ShowMessageFmt('width: %d, height: %d', [FormerWidth, FormerHeight]);
    //end;
    //Count.Font  := f;
    // TODO Get font colors and background color to be shown - what's up?

    //if FontSizeChanged then OnShowForm(Sender);
    if Timer1.Enabled then
    begin
      if Config.TickingOn then
        PlayTicking
      else
        StopAudio();
    end;

    if Config.AutoSave then
      SaveSettings(Sender);
    // TODO Resize the window if necessary to accomodate larger text
    // Get the text size from the font for '00:00:00'?

  end;
end;

procedure TMainForm.ToggleCountdown(Sender: TObject);
begin
  if (Sender.ClassName = Count.ClassName) and (not GetConfig.ClickTime) then
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
var Config: TConfig;
begin
  Config:= GetConfig;
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
      if Config.DoneAudioEnabled then
        PlayAudio(Config.DoneAudio, Config.LoopAudio);
      if Config.DoneAppEnabled then
        RunApp(Config.DoneApp);
      if Config.DoneTrayMsgEnabled then
        ShowTrayMessage(Config.DoneTrayMsg);
      if Config.DoneMessageEnabled then
        ShowDoneMessage(Config.DoneMessage);

      if Config.AutoRestart then
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
  if GetConfig.AutoSave then
    SaveSettings(Sender);
end;

procedure TMainForm.OnShowForm(Sender: TObject);
begin
  if OnShowFormFirstTime then
  begin
    OnShowFormFirstTime:= False;
    ApplyConfig;
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

  //UpdateAlwaysOnTop(AlwaysOnTop);
end;

procedure TMainForm.SaveSettings(Sender: TObject);
var
  Config: TConfig;
  wRect: TRect;
begin
  Config:= GetConfig;

  // These config values are not set in OptionsForm.
  Config.Minutes:= Minutes.Value;
  GetWindowRect(self.Handle, wRect);
  Config.WndWidth:= wRect.Right - wRect.Left;
  Config.WndHeight:= wRect.Bottom - wRect.Top;
  Config.WndLeft:= wRect.Left;
  Config.WndTop:= wRect.Top;

  if Config.Save = False then
    MessageDlg(MSG_WRITE_INI, mtError, [mbOK], 0);
end;

procedure TMainForm.MinutesChanged(Sender: TObject);
begin
  if not Timer1.Enabled then
    ResetCountdown(Sender);
end;

procedure TMainForm.ToggleCompact(Sender: TObject);
begin
  // todo
//  SetFieldsVisible(GetConfig.CompactMode);
//  CompactMode := not CompactMode;
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
  if GetConfig.HideSeconds then
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
  if not GetConfig.MinToTray then
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
  sndPlaySound(PChar(GetFilePath('.\sounds\ticking\ticking.wav')), SND_NODEFAULT or
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
begin
  // http://msdn.microsoft.com/en-us/library/ms645505(VS.85).aspx
  Windows.MessageBox(self.Handle, pChar(msg), 'Done',
     MB_SYSTEMMODAL or MB_SETFOREGROUND or MB_TOPMOST or MB_ICONINFORMATION);
  StopAudio();
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

procedure TMainForm.ApplyConfig;
var
  Config: TConfig;
  WndHandle: HWND;
  flags: Integer;
begin
  Config:= GetConfig;
  if Config.AlwaysOnTop then
     FormStyle:= fsSystemStayOnTop
  else
      FormStyle:= fsNormal;

  if Config.SecondsMode then
     TimeLabel.Caption := LBL_SECONDS
  else
      TimeLabel.Caption := LBL_MINUTES;

  Count.Font.Quality := fqAntialiased;
  Count.Font.Name := Config.Font.Name;
  Count.Font.CharSet := Config.Font.Charset;
  Count.Font.Color := Config.Font.Color;
  Count.Font.Size := Config.Font.Size;
  Count.Font.Style := Config.Font.Style;
  Count.Color := Config.Font.BgColor;

  WndHandle:= self.Handle;
  Self.Position := poDesigned;
  flags:= SWP_SHOWWINDOW;
  case Config.WndPosition of
       Remember: SetWindowPos(WndHandle, HWND_TOP, Config.WndLeft, Config.WndTop, Config.WndWidth, Config.WndHeight, flags);
       TopLeft: SetWindowPos(WndHandle, HWND_TOP, 0, 0, Config.WndWidth, Config.WndHeight, flags);
       TopRight: SetWindowPos(WndHandle, HWND_TOP, Monitor.WorkareaRect.Right - Config.WndWidth, 0, Config.WndWidth, Config.WndHeight, flags);
       BottomLeft: SetWindowPos(WndHandle, HWND_TOP, 0, Monitor.WorkareaRect.Bottom - Config.WndHeight, Config.WndWidth, Config.WndHeight, flags);
       BottomRight: SetWindowPos(WndHandle, HWND_TOP, Monitor.WorkareaRect.Right - Config.WndWidth,
                    Monitor.WorkareaRect.Bottom - Config.WndHeight, Config.WndWidth, Config.WndHeight, flags);
       Center:
         begin
              SetWindowPos(WndHandle, HWND_TOP, 0, 0, Config.WndWidth, Config.WndHeight, SWP_SHOWWINDOW or SWP_NOMOVE);
              Self.Position := poScreenCenter;
         end;
  end;
end;

initialization
  {$I mainform1.lrs}
end.


unit MainForm1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, StdCtrls, Spin, ExtCtrls, About, LCLType, Options, DateUtils,
  MyTimer;

type
  { TMainForm }
  TMainForm = class(TForm)
    Count: TPanel;
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
    TimeEdit: TSpinEdit;
    TrayIconMain: TTrayIcon;
    procedure OnCreateForm(Sender: TObject);
    procedure OnDestroyForm(Sender: TObject);
    procedure OnShowForm(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure TrayIconMainClick(Sender: TObject);
    procedure CloseApp(Sender: TObject);

    procedure ToggleCountdown(Sender: TObject);
    procedure ResetCountdown(Sender: TObject);

    procedure OnTick(Sender: TObject);
    procedure OnTimerStateChanged(Sender: TObject);
    procedure UpdateButtonsAndMenus;
    procedure UpdateTime();

    procedure ToggleCompact(Sender: TObject);
    procedure TimeEditChanged(Sender: TObject);
    procedure ShowAbout(Sender: TObject);
    procedure ShowOptions(Sender: TObject);
    procedure SaveSettings(Sender: TObject);
    procedure ShowDoneMessage(Msg: string);
    procedure ShowTrayMessage(Msg: string);
    procedure SetFieldsVisible(showFields: boolean);
    procedure PlayTicking();
  private
    MyTimer : TMyTimer;
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
  Config, Utils, MsgBox;

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
  TRAY_MENU_SHOW = 'Show Application';
  TRAY_MENU_HIDE = 'Hide Application';

  // Buttons
  BTN_START = '&Start';
  BTN_PAUSE = '&Pause';
  BTN_RESET = '&Reset';

  // Labels
  LBL_MINUTES = '&Minutes:';
  LBL_SECONDS = '&Seconds:';

  // Messages
  MSG_OPEN_INI  = 'An error occurred opening the .ini file, settings won''t be saved';
  MSG_WRITE_INI = 'An error occurred trying to write to the .ini file, settings won''t be saved.';


procedure TMainForm.OnCreateForm(Sender: TObject);
var
  Config: TConfig;
begin
  MyTimer:= TMyTimer.Create;
  MyTimer.OnSecondElapsed:= @OnTick;
  MyTimer.OnStateChanged:= @OnTimerStateChanged;
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

  Self.DoubleBuffered := True;
  Count.DoubleBuffered := True;
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

  // Use TimeEdit argument if it's the only parameter and it's numeric
  if (ParamCount = 1) and (TUtils.IsInteger(ParamStr(1))) then
    TimeEdit.Value := StrToInt(ParamStr(1));

  if Config.AutoStart then
    ToggleCountdown(Sender);
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
    // ApplyConfig does not set MainForm dimensions and TimeEdit(TSpinEdit)
    TimeEdit.Value:= GetConfig.Minutes;
    MyTimer.Reset; // This will trigger OnTimerStateChanged
  end;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
  begin
    TrayMenuShow.Caption:= TRAY_MENU_SHOW;
    if GetConfig.MinToTray then
    begin
      // SysTray hack
      // https://forum.lazarus.freepascal.org/index.php/topic,2194.msg9843.html#msg9843
      WindowState:= wsNormal;
      Hide;
      ShowInTaskBar := stNever;

      // Restore WindowState to what it was
      WindowState:= wsMinimized;
    end;
  end
  else if WindowState = wsNormal then
  begin
    TrayMenuShow.Caption:= TRAY_MENU_HIDE;
    if GetConfig.MinToTray then
    begin
      ShowInTaskBar := stDefault;
      Show;
    end;
  end;

end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  // TMainForm's KeyPreview property must be set to True for this to work.
  if key = #27 then
    Close; // 27 = Escape
  if (TimeEdit.Focused) and (key = #13) then
    ToggleCountdown(Sender);
end;

procedure TMainForm.TrayIconMainClick(Sender: TObject);
begin
  if WindowState = wsNormal then
  begin
    // On Windows setting wsMinimized does not minimize to taskbar.
    //WindowState:= wsMinimized;
    Application.Minimize;
  end
  else if WindowState = wsMinimized then
  begin
    // Setting WindowState to wsNormal does not call FormOnWindowStateChange
    WindowState:= wsNormal;
    FormWindowStateChange(Sender);
  end;
end;


procedure TMainForm.CloseApp(Sender: TObject);
begin
  OnDestroyForm(Sender);
  Close;
end;


procedure TMainForm.ToggleCountdown(Sender: TObject);
begin
  if (Sender.ClassName = Count.ClassName) and (not GetConfig.ClickTime) then
    Exit;

  // We want to go from `Finished` state directly to `Running` state. (or not?)
  if MyTimer.State = TState.Finished then
    MyTimer.Reset;

  MyTimer.Toggle;
end;

procedure TMainForm.ResetCountdown(Sender: TObject);
begin
  if (Sender.ClassName = Count.ClassName) and (not GetConfig.DblClickTime) then
    Exit;

  MyTimer.Reset;
end;

procedure TMainForm.OnTick(Sender: TObject);
begin
  UpdateTime();
end;

procedure TMainForm.OnTimerStateChanged(Sender: TObject);
var Config: TConfig;
begin
  Config:= GetConfig;

  case MyTimer.State of
    Ready:
    begin
      if Config.SecondsMode then
        MyTimer.Seconds := TimeEdit.Value
      else
        MyTimer.Minutes:= TimeEdit.Value;
      UpdateTime();
      UpdateButtonsAndMenus;
      TrayIconMain.Icon:= ImgIconMain.Picture.Icon;
    end;

    Running:
    begin
      UpdateButtonsAndMenus;
      TrayIconMain.Icon:= ImgIconRunning.Picture.Icon;
      if Config.TickingOn then
        PlayTicking();
    end;

    Paused:
    begin
      UpdateButtonsAndMenus;
      TrayIconMain.Icon:= ImgIconPaused.Picture.Icon;
      TUtils.StopAudio;
    end;

    Finished:
    begin
      UpdateTime();
      UpdateButtonsAndMenus;
      TUtils.StopAudio;

      Application.Title := APP_NAME;
      if Config.DoneAudioEnabled then
        TUtils.PlayAudio(Config.DoneAudio, Config.LoopAudio);
      if Config.DoneAppEnabled then
        TUtils.RunApp(Config.DoneApp);
      if Config.DoneTrayMsgEnabled then
        ShowTrayMessage(Config.DoneTrayMsg);
      if Config.DoneMessageEnabled then
        ShowDoneMessage(Config.DoneMessage);
      if Config.AutoRestart then
        ToggleCountdown(Sender)
      else
        TrayIconMain.Icon := ImgIconDone.Picture.Icon;

      Show;
    end;
  end;
end;


// When it comes to buttons and menus, there are only two states.
procedure TMainForm.UpdateButtonsAndMenus;
begin
  if MyTimer.State = TState.Running then
  begin
    ImgPause.Visible   := True;
    ImgStart.Visible   := False;
    MenuToggle.Caption := BTN_PAUSE;
    TrayMenuToggle.Caption := BTN_PAUSE;
  end
  else
  begin
    ImgPause.Visible   := False;
    ImgStart.Visible   := True;
    MenuToggle.Caption := BTN_START;
    TrayMenuToggle.Caption := BTN_START;
  end;
end;

// TODO Animate icon between alarm icon and main so it blinks every second
// Instead of playing icon, animate the clock hands so they go around?
// http://delphi.about.com/od/kbwinshell/l/aa122501a.htm
procedure TMainForm.UpdateTime();
begin
  // Changing caption while it's not visible keeps time from flashing
  Count.Visible := False;
  Count.Caption := TUtils.SecondsToTime(MyTimer.Seconds, GetConfig.HideSeconds);
  Count.Visible := True;
  MenuCount.Caption := Count.Caption;
  if MyTimer.State = TState.Running then
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


procedure TMainForm.ShowOptions(Sender: TObject);
var
  Config: TConfig;
  Ok: Boolean;
  r : TRect;
begin
  OptionsForm := TOptionsForm.Create(Self);
  Ok:= OptionsForm.ShowModal = mrOk;
  OptionsForm.Free;

  if Ok then
  begin
    Config:= GetConfig;
    r:= TUtils.GetFormRect(self);
    Config.WndLeft:= r.Left;
    Config.WndTop:= r.Top;
    Config.WndWidth:= r.Width;
    Config.WndHeight:= r.Height;
    ApplyConfig;

    //if not (Count.Font.Size = f.Size) then
    //begin
      //FontSizeChanged := True;
      //GetPreferredSize(FormerWidth, FormerHeight, True);
      //ShowMessageFmt('width: %d, height: %d', [FormerWidth, FormerHeight]);
    //end;
    //Count.Font  := f;
    // TODO Get font colors and background color to be shown - what's up?

    if MyTimer.State = TState.Running then
    begin
      if Config.TickingOn then
        PlayTicking
      else
        TUtils.StopAudio;
    end;

    if Config.AutoSave then
      SaveSettings(Sender);
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


procedure TMainForm.SaveSettings(Sender: TObject);
var
  Config: TConfig;
  wRect: TRect;
begin
  Config:= GetConfig;

  // These config values are not set in OptionsForm.
  Config.Minutes:= TimeEdit.Value;
  wRect:= TUtils.GetFormRect(self);
  Config.WndWidth:= wRect.Width;
  Config.WndHeight:= wRect.Height;
  Config.WndLeft:= wRect.Left;
  Config.WndTop:= wRect.Top;

  if Config.Save = False then
    MessageDlg(MSG_WRITE_INI, mtError, [mbOK], 0);
end;

procedure TMainForm.TimeEditChanged(Sender: TObject);
begin
  if (MyTimer.State <> TState.Running) and (MyTimer.State <> TState.Paused) then
    ResetCountdown(Sender);
end;

procedure TMainForm.ToggleCompact(Sender: TObject);
begin
  // todo
//  SetFieldsVisible(GetConfig.CompactMode);
//  CompactMode := not CompactMode;
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
  //Windows.MessageBox(self.Handle, pChar(msg), 'Done',
  //   MB_SYSTEMMODAL or MB_SETFOREGROUND or MB_TOPMOST or MB_ICONINFORMATION);
  MsgboxForm := TMsgBoxForm.Create(Self);
  MsgBoxForm.SetText(Msg);
  MsgboxForm.ShowModal;
  MsgboxForm.Release;
  TUtils.StopAudio;
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
   TimeEdit.Visible  := showFields;
   ImgStart.Visible := showFields;
   ImgReset.Visible := showFields;
   ImgPause.Visible := showFields;
 end;
end;



procedure TMainForm.PlayTicking();
begin
  TUtils.PlayAudio('.\sounds\ticking\ticking.wav', True);
end;


procedure TMainForm.ApplyConfig;
var
  Config: TConfig;
  tw, th: Integer;
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


  Count.Canvas.GetTextSize('00:00:00', tw, th);
  if tw < 168 then
    tw:= 168;
  self.ClientWidth:= tw + 8;
  self.ClientHeight:= th + 32;

  self.Position:= poDesigned;
  case Config.WndPosition of
    Remember: TUtils.SetFormPos(self, Config.WndLeft, Config.WndTop);
    TopLeft: TUtils.SetFormPos(self, 0, 0);
    TopRight: TUtils.SetFormPos(self, Monitor.WorkareaRect.Right - Width, 0);
    BottomLeft: TUtils.SetFormPos(self, 0, Monitor.WorkareaRect.Bottom - Height);
    BottomRight: TUtils.SetFormPos(self, Monitor.WorkareaRect.Right - Width, Monitor.WorkareaRect.Bottom - Height);
    Center:
    begin
      Self.Position := poScreenCenter;
      // After the main form was centered, we want to be able to position it and we
      // don't want minimize/restore action to center it again.
      Self.Position := poDesigned;
    end;
  end;

end;

initialization
  {$I mainform1.lrs}
end.


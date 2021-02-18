unit MainForm1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, StdCtrls, Spin, ExtCtrls, About, LCLType, Options, Windows, DateUtils,
  MyTimer;

type
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
    TimeEdit: TSpinEdit;
    Count:   TStaticText;
    TrayIconMain: TTrayIcon;
    procedure OnCreateForm(Sender: TObject);
    procedure OnDestroyForm(Sender: TObject);
    procedure OnShowForm(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
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
    FontSizeChanged : boolean;
    //FormerWidth : integer;
    //FormerHeight : integer;
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
  Config, Utils;

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
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  WindowState   := wsNormal;
  ShowInTaskBar := stDefault;
  Show;
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

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  // TMainForm's KeyPreview property must be set to True for this to work.
  if key = #27 then
    Close; // 27 = Escape
  if (TimeEdit.Focused) and (key = #13) then
    ToggleCountdown(Sender);
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
        TUtils.PlayAudio('.\sounds\ticking\ticking.wav', True);
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
    if MyTimer.State = TState.Running then
    begin
      if Config.TickingOn then
        PlayTicking
      else
        TUtils.StopAudio;
    end;

    if Config.AutoSave then
      SaveSettings(Sender);
    // TODO Resize the window if necessary to accomodate larger text
    // Get the text size from the font for '00:00:00'?

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
  GetWindowRect(self.Handle, wRect);
  Config.WndWidth:= wRect.Right - wRect.Left;
  Config.WndHeight:= wRect.Bottom - wRect.Top;
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
  Windows.MessageBox(self.Handle, pChar(msg), 'Done',
     MB_SYSTEMMODAL or MB_SETFOREGROUND or MB_TOPMOST or MB_ICONINFORMATION);
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


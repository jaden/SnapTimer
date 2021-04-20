unit MainController;

{$mode objfpc}

interface

uses
  Classes, SysUtils, MyTimer;

type
  TMainController = class
    private
      FCompactMode : Boolean;
      FTimer : TMyTimer;
      procedure SetCompactMode(Value: Boolean);

      procedure OnTick(Sender: TObject);
      procedure OnTimerStateChanged(Sender: TObject);
      procedure UpdateTime;
      procedure UpdateButtonsAndMenus;
    public
      constructor Create;

      // Called from MainForm and CompactModeForm
      procedure ToggleCountdown(Sender: TObject);
      procedure ResetCountdown(Sender: TObject);

      procedure PlayTicking;
      procedure ShowTrayMessage(Msg: string);
      procedure ShowDoneMessage(Msg: string);

      //property CompactMode : Boolean read FCompactMode write SetCompactMode; // Do we need this???
      property CompactMode : Boolean read FCompactMode;
      property Timer : TMyTimer read FTimer;
  end;


function GetMainController : TMainController;

implementation

uses Forms, MainForm1, CompactMode, Utils, Config, MsgBox, Consts;

var
  MainControllerInstance : TMainController;

procedure TMainController.SetCompactMode(Value: Boolean);
var Pos : TPoint;
begin
  if (Value = FCompactMode) or (GetConfig.CompactMode = False) then
    Exit;

  if Value = True then
  begin
    // Note: Width and ClientWidth are the same
    CompactModeForm.Width:= MainForm.Count.Width;
    CompactModeForm.Height:= MainForm.Count.Height;
    Pos:= TUtils.GetControlPos(MainForm.Count);
    CompactModeForm.Top:= Pos.Y;
    CompactModeForm.Left:= Pos.X;
    CompactModeForm.Count.Caption:= MainForm.Count.Caption;
    CompactModeForm.Count.Font:= MainForm.Count.Font;
    CompactModeForm.Count.Color:= MainForm.Count.Color;
    CompactModeForm.Visible:= True;
    MainForm.Visible:= False;
  end
  else
  begin
    MainForm.Count.Caption:= CompactModeForm.Count.Caption;
    CompactModeForm.Visible:= False;
    MainForm.Visible:= True;
  end;

  FCompactMode:= Value;
end;

procedure TMainController.OnTick(Sender: TObject);
begin
  UpdateTime;
end;

procedure TMainController.OnTimerStateChanged(Sender: TObject);
var Config: TConfig;
begin
  Config:= GetConfig;

  case Timer.State of
    Ready:
    begin
      SetCompactMode(False);
      if Config.SecondsMode then
        Timer.Seconds := MainForm.TimeEdit.Value
      else
        Timer.Minutes:= MainForm.TimeEdit.Value;
      UpdateTime();
      UpdateButtonsAndMenus;
      MainForm.TrayIconMain.Icon:= MainForm.ImgIconMain.Picture.Icon;
    end;

    Running:
    begin
      UpdateButtonsAndMenus;
      MainForm.TrayIconMain.Icon:= MainForm.ImgIconRunning.Picture.Icon;
      if Config.TickingOn then
        PlayTicking();

      SetCompactMode(True);
    end;

    Paused:
    begin
      SetCompactMode(False);
      UpdateButtonsAndMenus;
      MainForm.TrayIconMain.Icon:= MainForm.ImgIconPaused.Picture.Icon;
      TUtils.StopAudio;
    end;

    Finished:
    begin
      SetCompactMode(False);
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
        MainForm.TrayIconMain.Icon := MainForm.ImgIconDone.Picture.Icon;

      MainForm.ShowForm;
    end;
  end;
end;

// TODO Animate icon between alarm icon and main so it blinks every second
// Instead of playing icon, animate the clock hands so they go around?
// http://delphi.about.com/od/kbwinshell/l/aa122501a.htm
procedure TMainController.UpdateTime();
var
  Time : String;
begin
  Time:= TUtils.SecondsToTime(Timer.Seconds, GetConfig.HideSeconds);

  if CompactMode then
  begin
    CompactModeForm.Count.Visible := False;
    CompactModeForm.Count.Caption := Time;
    CompactModeForm.Count.Visible := True;
  end
  else
  begin
    // Changing caption while it's not visible keeps time from flashing
    MainForm.Count.Visible := False;
    MainForm.Count.Caption := Time;
    MainForm.Count.Visible := True;
  end;

  MainForm.MenuCount.Caption := Time;
  if Timer.State = TState.Running then
  begin
    Application.Title := Time + ' - ' + APP_NAME;
    MainForm.TrayIconMain.Hint := Time;
  end
  else
  begin
    Application.Title := APP_NAME;
    MainForm.TrayIconMain.Hint := APP_NAME;
  end;
end;

// When it comes to buttons and menus, there are only two states.
procedure TMainController.UpdateButtonsAndMenus;
begin
  with MainForm do
  begin
    if Timer.State = TState.Running then
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
end;





constructor TMainController.Create;
begin
  FCompactMode:= False;
  FTimer:= TMyTimer.Create;
  FTimer.OnSecondElapsed:= @OnTick;
  FTimer.OnStateChanged:= @OnTimerStateChanged;
end;

procedure TMainController.ToggleCountdown(Sender: TObject);
begin
  if ((Sender = MainForm.Count) or (Sender = CompactModeForm.Count)) and (GetConfig.ClickTime = False) then
    Exit;

  // We want to go from `Finished` state directly to `Running` state. (or not?)
  if Timer.State = TState.Finished then
    Timer.Reset;

  Timer.Toggle;
end;

procedure TMainController.ResetCountdown(Sender: TObject);
begin
  if ((Sender = MainForm.Count) or (Sender = CompactModeForm.Count)) and (GetConfig.DblClickTime = False) then
    Exit;

  // TODO compact mode

  Timer.Reset;
end;

procedure TMainController.PlayTicking();
begin
  TUtils.PlayAudio('.\sounds\ticking\ticking.wav', True);
end;

procedure TMainController.ShowTrayMessage(Msg: string);
begin
  with MainForm.TrayIconMain do
  begin
    BalloonHint := Msg;
    ShowBalloonHint;
    BalloonTimeout := 4000;
  end;
end;

procedure TMainController.ShowDoneMessage(Msg: string);
begin
  // http://msdn.microsoft.com/en-us/library/ms645505(VS.85).aspx
  //Windows.MessageBox(self.Handle, pChar(msg), 'Done',
  //   MB_SYSTEMMODAL or MB_SETFOREGROUND or MB_TOPMOST or MB_ICONINFORMATION);
  MsgboxForm := TMsgBoxForm.Create(MainForm);
  MsgBoxForm.SetText(Msg);
  MsgboxForm.ShowModal;
  MsgboxForm.Release;
  TUtils.StopAudio;
end;


function GetMainController : TMainController;
begin
  Result:= MainControllerInstance;
end;

initialization
  MainControllerInstance:= TMainController.Create;

end.


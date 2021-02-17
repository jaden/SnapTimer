unit mytimer;

{$mode objfpc}

interface

uses
  Classes, SysUtils, ExtCtrls;

type
  TMode = (Timer, Stopwatch);
  TState = (Running, Stopped, Paused);

  // TODO find better name
  TMyTimer = class
  private
    FTimer: TTimer;
    // Start or End time, depending on the Mode.
    FTime: TDateTime;
    FSeconds: Integer;

    FState: TState;
    FMode: TMode;
    FOnSecondElapsed: TNotifyEvent;
    FOnFinished: TNotifyEvent;

    function GetSeconds : Integer;
    procedure SetSeconds(AValue: Integer);
    function GetMinutes : Integer;
    procedure SetMinutes(AValue: Integer);

    procedure OnTimer(Sender: TObject);
    procedure UpdateSeconds;
    procedure UpdateTime;
  public
    constructor Create;
    destructor Free;
    procedure Start;
    procedure Pause;
    procedure Resume;
    procedure StartPauseResume;
    procedure Stop;

    property Seconds: Integer read GetSeconds write SetSeconds;
    property Minutes: Integer read GetMinutes write SetMinutes;
    property State: TState read FState;
    property Mode: TMode read FMode write FMode;
    property OnSecondElapsed: TNotifyEvent read FOnSecondElapsed write FOnSecondElapsed;
    property OnFinished: TNotifyEvent read FOnFinished write FOnFinished;
end;

implementation

uses DateUtils;

function TMyTimer.GetSeconds : Integer;
begin
  Result:= FSeconds;
end;

procedure TMyTimer.SetSeconds(AValue: Integer);
begin
  if State <> Stopped then
     Raise Exception.Create('Timer is not in `Stopped` state');
  FSeconds:= AValue;
end;

function TMyTimer.GetMinutes : Integer;
begin
  Result:= FSeconds div 60;
end;

procedure TMyTimer.SetMinutes(AValue: Integer);
begin
  if State <> Stopped then
     Raise Exception.Create('Timer is not in `Stopped` state');
  FSeconds:= AValue * 60;
end;

procedure TMyTimer.OnTimer(Sender: TObject);
begin
  if Mode = Timer then
  begin
    if Now >= FTime then
    begin
      FTimer.Enabled:= False;
      FSeconds:= 0;
      FState:= Stopped;
      OnFinished(self);
    end
    else
      UpdateSeconds;
  end
  else
  begin
    // Stopwatch
    UpdateSeconds;
  end
end;

procedure TMyTimer.UpdateSeconds;
var s : Integer;
begin
  s:= SecondsBetween(FTime, Now);
  if s <> FSeconds then
  begin
    FSeconds:= s;
    OnSecondElapsed(self);
  end;
end;

procedure TMyTimer.UpdateTime;
begin
  if Mode = Timer then
    FTime:= Now + (FSeconds * OneSecond)     // FTime is the end time
  else
    FTime:= Now - (FSeconds * OneSecond)     // FTime is the start time
end;

constructor TMyTimer.Create;
begin
  FTimer:= TTimer.Create(nil);
  FTimer.Interval:= 160;
  FTimer.OnTimer:= @OnTimer;
  FMode:= Timer;
end;

destructor TMyTimer.Free;
begin
  FTimer.Free;
end;


procedure TMyTimer.Start;
begin
  if State <> Stopped then
     Raise Exception.Create('Timer is not in `Stopped` state');

  FState:= Running;
  UpdateTime;
  FTimer.Enabled:= True;
end;

procedure TMyTimer.Pause;
begin
  if State <> Running then
     Raise Exception.Create('Timer is not in `Running` state');

  FState:= Paused;
  FTimer.Enabled:= False;
end;

procedure TMyTimer.Resume;
begin
  if State <> Paused then
     Raise Exception.Create('Timer is not in `Paused` state');

  FState:= Running;
  UpdateTime;
  FTimer.Enabled:= True;
end;

procedure TMyTimer.StartPauseResume;
begin
  case State of
      Running : Pause;
      Stopped : Start;
      Paused : Resume;
  end;
end;

procedure TMyTimer.Stop;
begin
  FTimer.Enabled:= False;
  FState:= Stopped;
  // TODO reset FTime, FSeconds??
  // Call OnFinished?
end;

end.


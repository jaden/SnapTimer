unit MyTimer;

{$mode objfpc}

interface

uses
  Classes, SysUtils, ExtCtrls;

type
  TMode = (Timer, Stopwatch);
  TState = (Ready, Running, Finished, Paused);

  // TODO find better name
  TMyTimer = class
  private
    FTimer: TTimer;
    // Start or End time, depending on the Mode.
    FTime: TDateTime;
    FSeconds: Integer;
    FSecondsInitial: Integer;
    FState: TState;
    FMode: TMode;
    FOnSecondElapsed: TNotifyEvent;
    FOnStateChanged: TNotifyEvent;

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
    procedure Toggle;
    procedure Reset;

    property Seconds: Integer read GetSeconds write SetSeconds;
    property Minutes: Integer read GetMinutes write SetMinutes;
    property State: TState read FState;
    property Mode: TMode read FMode write FMode;
    property OnSecondElapsed: TNotifyEvent read FOnSecondElapsed write FOnSecondElapsed;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  private

end;

implementation

uses DateUtils;

function TMyTimer.GetSeconds : Integer;
begin
  Result:= FSeconds;
end;

procedure TMyTimer.SetSeconds(AValue: Integer);
begin
  if State <> Ready then
    Raise Exception.Create('Timer is not in `Ready` state');
  FSeconds:= AValue;
  FSecondsInitial:= AValue;
end;

function TMyTimer.GetMinutes : Integer;
begin
  Result:= FSeconds div 60;
end;

procedure TMyTimer.SetMinutes(AValue: Integer);
begin
  if State <> Ready then
    Raise Exception.Create('Timer is not in `Ready` or `Finished` state');
  FSeconds:= AValue * 60;
  FSecondsInitial:= FSeconds;
end;

procedure TMyTimer.OnTimer(Sender: TObject);
begin
  if Mode = Timer then
  begin
    if Now >= FTime then
    begin
      FTimer.Enabled:= False;
      FSeconds:= 0;
      FState:= Finished;
      if Assigned(FOnStateChanged) then
        FOnStateChanged(self);
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
    if Assigned(FOnSecondElapsed) then
      FOnSecondElapsed(self);
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
  FTimer.Enabled:= False;
  FTimer.Interval:= 160;
  FTimer.OnTimer:= @OnTimer;
  FSeconds:= 10 * 60;
  FSecondsInitial:= FSeconds;
  FState:= Ready;
  FMode:= Timer;
  FOnSecondElapsed:= nil;
  FOnStateChanged:= nil;
end;

destructor TMyTimer.Free;
begin
  FTimer.Free;
end;


procedure TMyTimer.Start;
begin
  if State <> Ready then
    Raise Exception.Create('Timer is not in `Ready` state');

  FState:= Running;
  UpdateTime;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(self);
  FTimer.Enabled:= True;
end;

procedure TMyTimer.Pause;
begin
  if State <> Running then
    Raise Exception.Create('Timer is not in `Running` state');

  FState:= Paused;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(self);
  FTimer.Enabled:= False;
end;

procedure TMyTimer.Resume;
begin
  if State <> Paused then
    Raise Exception.Create('Timer is not in `Paused` state');

  FState:= Running;
  UpdateTime;
  if Assigned(FOnStateChanged) then
    FOnStateChanged(self);
  FTimer.Enabled:= True;
end;

procedure TMyTimer.Toggle;
begin
  case State of
      Ready : Start;
      Running : Pause;
      Finished : Reset;
      Paused : Resume;
  end;
end;

procedure TMyTimer.Reset;
var PrevState: TState;
begin
  PrevState:= FState;
  FTimer.Enabled:= False;
  FState:= Ready;
  FSeconds:= FSecondsInitial;
  // We want FOnStateChanged to be called even if we are in `Ready` state.
  if Assigned(FOnStateChanged) {and (PrevState <> Ready)} then
    FOnStateChanged(self);
end;


end.


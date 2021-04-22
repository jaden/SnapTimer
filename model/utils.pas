unit utils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

Uses Forms, Types, Controls;


// https://forum.lazarus.freepascal.org/index.php/topic,29450.msg186002.html?PHPSESSID=0l7k7l8bgpqdvcqcqetk17rt04#msg186002
type
  TRectHelper = record helper for TRect
  private
    function GetWidth: Integer;
    procedure SetWidth(const Value: Integer);
    function GetHeight: Integer;
    procedure SetHeight(const Value: Integer);
  public
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;

// TUtils class serves as a namespace.
type
  TUtils = class
    class function SecondsToTime(Seconds: integer; HideSeconds: Boolean): string;
    class function GetFilePath(Path: string): string;
    class procedure RunApp(Path: string);
    class function IsInteger(S: String): boolean;
    class function GetFormRect(Form: TForm) : TRect;
    class procedure SetControlPos(Control: TWinControl; X: Integer; Y: Integer);
    class function GetControlPos(Control: TWinControl) : TPoint;
    class procedure PlayAudio(Path: string; Loop: boolean);
    class procedure StopAudio;
  end;

implementation

uses
  Classes, SysUtils, StrUtils, FileUtil
{$IFDEF WINDOWS}
  ,Windows, MMSystem
{$ENDIF}
{$IFDEF LINUX}
  ,Process, asyncprocess
{$ENDIF}
;


function TRectHelper.GetHeight: Integer;
begin
  Result := Bottom - Top;
end;

procedure TRectHelper.SetHeight(const Value: Integer);
begin
  Bottom := Top + Value;
end;

function TRectHelper.GetWidth: Integer;
begin
  Result := Right - Left;
end;

procedure TRectHelper.SetWidth(const Value: Integer);
begin
  Right := Left + Value;
end;



class function TUtils.SecondsToTime(Seconds: integer; HideSeconds: Boolean): string;
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

// TODO Interpret env vars in the path
class function TUtils.GetFilePath(Path: string): string;
begin
  if AnsiStartsStr('.', Path) then
  begin
    Result := ExtractFilePath(Application.ExeName) + Path;
  end else
    Result := Path;
end;


{$IFDEF LINUX}
// https://stackoverflow.com/questions/48609416/shellexecute-equivalent-for-linux-as-target-platform
procedure ShlOpen( FileName: String ) ;
var prc: TProcess;
begin
 prc:= TProcess.Create ( nil ) ;
 prc.CommandLine:= 'xdg-open ' + FileName;
 prc.Execute;
 prc.free;
end ;
{$ENDIF}

class procedure TUtils.RunApp(Path: string);
begin
{$IFDEF WINDOWS}
  ShellExecute(0, 'open', PChar(GetFilePath(Path)), nil, nil, SW_SHOWNORMAL);
{$ENDIF}

{$IFDEF LINUX}
  try
     ShlOpen(Path);
  except
    On E: Exception do
        E.CreateFmt('Failed to run: %s', [Path]);
  end;
{$ENDIF}
end;


class function TUtils.IsInteger(S: String): boolean;
begin
  try
    Result := True;
    StrToInt(S);
  except on E: EConvertError do
    Result := False;
  end;
end;


class function TUtils.GetFormRect(Form: TForm) : TRect;
var r : TRect;
begin
{$IFDEF WINDOWS}
  GetWindowRect(Form.Handle, r);
  Result:= r;
{$ENDIF}

{$IFDEF LINUX}
  // TODO test this
  r.SetWidth(Form.Width);
  r.SetHeight(Form.height);
  Result:= r;
{$ENDIF}
end;

class procedure TUtils.SetControlPos(Control: TWinControl; X: Integer; Y: Integer);
begin
{$IFDEF WINDOWS}
  // It seems that there is no difference.
  //SetWindowPos(Control.Handle, HWND_TOP, X, Y, 0, 0, SWP_SHOWWINDOW or SWP_NOSIZE);
  Control.Left:= X;
  Control.Top:= Y;
{$ENDIF}

{$IFDEF LINUX}
  Control.Left:= X;
  Control.Top:= Y;
{$ENDIF}
end;

class function TUtils.GetControlPos(Control: TWinControl) : TPoint;
var r : TRect;
    p : TPoint;
begin
{$IFDEF WINDOWS}
  GetWindowRect(Control.Handle, r);
  Result.X:= r.Left;
  Result.Y:= r.Top;

  // Slightly different results
  //p.X:= Control.Left;
  //p.Y:= Control.Top;
  //Result:= Control.ClientToScreen(p);
{$ENDIF}

{$IFDEF LINUX}
  // TODO: This works on Lubuntu, bit of a hack
  p.X:= Control.Left - 2;
  p.Y:= Control.Top - 32;
  Result:= Control.ClientToScreen(p);
{$ENDIF}
end;

// https://wiki.freepascal.org/Play_Sound_Multiplatform
// Modified
{$IFDEF LINUX}
procedure PlaySound(const szSoundFilename: string);
var
  szNonWindowsPlayCommand: string;
  SoundPlayerAsyncProcess: Tasyncprocess;
begin
  szNonWindowsPlayCommand := '';
  // How to play in Linux? Use generic Linux commands
  // Use asyncprocess to play sound as SND_ASYNC
  // Try play
  if (FindDefaultExecutablePath('play') <> '') then
    szNonWindowsPlayCommand := 'play';
  // Try aplay
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('aplay') <> '') then
      szNonWindowsPlayCommand := 'aplay -q ';
  // Try paplay
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('paplay') <> '') then
      szNonWindowsPlayCommand := 'paplay';
  // Try mplayer
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('mplayer') <> '') then
      szNonWindowsPlayCommand := 'mplayer -really-quiet ';
  // Try CMus
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('CMus') <> '') then
      szNonWindowsPlayCommand := 'CMus ';
  // Try pacat
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('pacat') <> '') then
      szNonWindowsPlayCommand := 'pacat -p ';
  // Try ffplay
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('ffplay') <> '') then
      szNonWindowsPlayCommand := 'ffplay -autoexit -nodisp ';
  // Try cvlc
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('cvlc') <> '') then
      szNonWindowsPlayCommand := 'cvlc -q --play-and-exit ';
  // Try canberra-gtk-play
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('canberra-gtk-play') <> '') then
      szNonWindowsPlayCommand := 'canberra-gtk-play -c never -f ';
  // Try Macintosh command?
  if (szNonWindowsPlayCommand = '') then
    if (FindDefaultExecutablePath('afplay') <> '') then
      szNonWindowsPlayCommand := 'afplay';
  // proceed if we managed to find a valid command
  if (szNonWindowsPlayCommand <> '') then
  begin
    SoundPlayerAsyncProcess := Tasyncprocess.Create(nil);
    //SoundPlayerAsyncProcess.CurrentDirectory := ExtractFileDir(szSoundFilename);
    SoundPlayerAsyncProcess.Executable :=
      FindDefaultExecutablePath(Copy2Space(szNonWindowsPlayCommand));
    SoundPlayerAsyncProcess.Parameters.Clear;
    SoundPlayerAsyncProcess.Parameters.Add(szSoundFilename);
    try
      SoundPlayerAsyncProcess.Execute;
    except
      On E: Exception do
        E.CreateFmt('Playstyle=paASync: Unable to Play %s Message:%s', [szSoundFilename, E.Message]);
    end;
    //FreeAndNil(SoundPlayerAsyncProcess);
  end
  else
    raise Exception.CreateFmt('The play command %s does not work on your system',
      [szNonWindowsPlayCommand]);
end;
{$ENDIF}


class procedure TUtils.PlayAudio(Path: string; Loop: boolean);
begin
{$IFDEF WINDOWS}
  if Loop then
    sndPlaySound(PChar(GetFilePath(Path)), SND_NODEFAULT or SND_ASYNC or SND_LOOP)
  else
    sndPlaySound(PChar(GetFilePath(Path)), SND_NODEFAULT or SND_ASYNC);
{$ENDIF}

{$IFDEF LINUX}
  // TODO Loop
  PlaySound(Path);
{$ENDIF}
end;

class procedure TUtils.StopAudio;
begin
{$IFDEF WINDOWS}
  sndPlaySound(nil, 0);
{$ENDIF}

{$IFDEF LINUX}
  // TODO
{$ENDIF}
end;


end.


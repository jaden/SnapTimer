unit Config;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Graphics;

const
 COMPACT_MODE_ALPHA_MIN = 10;

type
  TPosition = (
    Center,
    Remember,
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
    // Aliases
    _TPositionFirst = Center,
    _TPositionLast = BottomRight
    );

  TCompactModeTransparency = (
    None,
    TransparentBackground,
    AlphaBlending,
    // Aliases
    _TCompactModeTransparencyFirst = None,
    _TCompactTransparencyLast = AlphaBlending
  );

   TFontConfig = class
    Name: String;
    Charset: Integer;
    Color: Integer;
    BgColor: Integer;
    Size: Integer;
    Style: TFontStyles;
  end;

  TCompactModeConfig = class
    Enabled:  Boolean;
    Transparency: TCompactModeTransparency;
    AlphaValue: Byte;
  end;

  TConfig = class
  private
    // Main
    FMinutes: Integer;
    FAlwaysOnTop: boolean;
    FMinToTray: boolean;
    FAutoStart: boolean;
    FHideSeconds: boolean;
    FClickTime: boolean;
    FDblClickTime: boolean;
    FAutoRestart: boolean;
    FLoopAudio: boolean;
    FTickingOn: boolean;
    FAutoSave: boolean;
    FSecondsMode: boolean;
    // Placement
    FWndHeight: integer;
    FWndWidth: integer;
    FWndLeft: integer;
    FWndTop: integer;
    FWndPosition: TPosition;
    // Alarms
    FDoneMessage: string;
    FDoneMessageEnabled: boolean;
    FDoneTrayMsg: string;
    FDoneTrayMsgEnabled: boolean;
    FDoneAudio: string;
    FDoneAudioEnabled: boolean;
    FDoneApp: string;
    FDoneAppEnabled: boolean;
    // Font
    FFont: TFontConfig;
    FDefaultFont: TFontConfig;
    // Compact Mode
    FCompactMode: TCompactModeConfig;

    procedure SetWndPosition(Value: TPosition);
    procedure SetDoneMessage(Msg: String);
    procedure SetDoneTrayMessage(Msg: String);
  public
    constructor Create;
    destructor Free;
    function Load : Boolean;
    function Save : Boolean;
    function GetDefaultFont : TFontConfig;

    // Main
    property Minutes: Integer read FMinutes write FMinutes;
    property AlwaysOnTop: boolean read FAlwaysOnTop write FAlwaysOnTop;
    property MinToTray: boolean read FMinToTray write FMinToTray;
    property AutoStart: boolean read FAutoStart write FAutoStart;
    property HideSeconds: boolean read FHideSeconds write FHideSeconds;
    property ClickTime: boolean read FClickTime write FClickTime;
    property DblClickTime: boolean read FDblClickTime write FDblClickTime;
    property AutoRestart: boolean read FAutoRestart write FAutoRestart;
    property LoopAudio: boolean read FLoopAudio write FLoopAudio;
    property TickingOn: boolean read FTickingOn write FTickingOn;
    property AutoSave: boolean read FAutoSave write FAutoSave;
    property SecondsMode: boolean read FSecondsMode write FSecondsMode; // Undocumented mode to treat minutes as seconds
    // Placement
    property WndHeight: integer read FWndHeight write FWndHeight;
    property WndWidth: integer read FWndWidth write FWndWidth;
    property WndLeft: integer read FWndLeft write FWndLeft;
    property WndTop: integer read FWndTop write FWndTop;
    property WndPosition: TPosition read FWndPosition write SetWndPosition;
    // Alarm
    property DoneMessage: string read FDoneMessage write SetDoneMessage;
    property DoneMessageEnabled: boolean read FDoneMessageEnabled write FDoneMessageEnabled;
    property DoneTrayMsg: string read FDoneTrayMsg write SetDoneTrayMessage;
    property DoneTrayMsgEnabled: boolean read FDoneTrayMsgEnabled write FDoneTrayMsgEnabled;
    property DoneAudio: string read FDoneAudio write FDoneAudio;
    property DoneAudioEnabled: boolean read FDoneAudioEnabled write FDoneAudioEnabled;
    property DoneApp: string read FDoneApp write FDoneApp;
    property DoneAppEnabled: boolean read FDoneAppEnabled write FDoneAppEnabled;
    // Fonts
    property Font: TFontConfig read FFont write FFont;
    // Compact Mode
    property CompactMode: TCompactModeConfig read FCompactMode write FCompactMode;
end;

function GetConfig : TConfig;

implementation

uses
  IniFiles;

// Default values
const
 INI_SEC_MAIN  = 'Main';
 INI_SEC_ALARMS = 'Alarms';
 INI_SEC_PLACEMENT = 'Placement';
 INI_SEC_FONTS = 'Fonts';
 INI_SEC_COMPACT_MODE = 'CompactMode';
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
 INI_COMPACT_MODE_ENABLED = 'Enabled';
 INI_COMPACT_MODE_TRANSPARENCY = 'Transparency';
 INI_COMPACT_MODE_ALPHA_VALUE = 'AlphaValue';

 INI_FILE_NAME = 'SnapTimer.ini';
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
 DEF_FONT_BG_COLOR = clNone;
 DEF_FONT_SIZE = 38;
 DEF_FONT_STYLE = 0;
 DEF_COMPACT_MODE = False;
 DEF_COMPACT_MODE_TRANSPARENCY = TCompactModeTransparency.None;
 DEF_COMPACT_MODE_ALPHA = 128;

 var
   ConfigInstance : TConfig;


 function FontStylesToInt(FontStyles: TFontStyles): integer;
 var
   Mask:  integer;
   Style: TFontStyle;
 begin
   // Translate the set into a bit mask
   Mask := 0;
   for Style := Low(TFontStyle) to High(TFontStyle) do
     if Style in FontStyles then
       Mask := Mask or (1 shl Ord(Style));
   Result   := Mask;
 end;

 function IntToFontStyles(Mask: integer): TFontStyles;
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

procedure TConfig.SetDoneMessage(Msg: String);
begin
  // TODO Set some reasonable length limit
  FDoneMessage:= Msg;
end;

procedure TConfig.SetDoneTrayMessage(Msg: String);
begin
  // TODO Set some reasonable length limit
  FDoneTrayMsg:= Msg;
end;


procedure TConfig.SetWndPosition(Value: TPosition);
begin
  if Value < _TPositionFirst then
    Value:= _TPositionFirst;

  if Value > _TPositionLast then
    Value:= _TPositionLast;

  FWndPosition:= Value;
end;

constructor TConfig.Create;
begin
  // Main
  Minutes:= DEF_TIME;
  AlwaysOnTop := DEF_ALWAYS_ON_TOP;
  MinToTray:= DEF_MIN_TO_TRAY;
  AutoStart:= DEF_AUTOSTART;
  HideSeconds:= DEF_HIDESECONDS;
  ClickTime:= DEF_CLICKTIME;
  DblClickTime:= DEF_DBLCLICKTIME;
  AutoRestart:= DEF_AUTORESTART;
  LoopAudio:= DEF_LOOP_AUDIO;
  TickingOn:= DEF_TICKING_ON;
  AutoSave:= DEF_AUTOSAVE;
  SecondsMode:= DEF_SECONDS_MODE;
  // Placement
  WndPosition:= TPosition(DEF_POSITION);
  WndWidth:= DEF_WIDTH;
  WndHeight:= DEF_HEIGHT;
  // Alarms
  DoneMessage:= DEF_DONE_MSG;
  DoneMessageEnabled:= DEF_DONE_MSG_ON;
  DoneTrayMsg:= DEF_DONE_TRAY_MSG;
  DoneTrayMsgEnabled:= DEF_DONE_TRAY_MSG_ON;
  DoneAudio:= DEF_DONE_AUDIO;
  DoneAudioEnabled:= DEF_DONE_AUDIO_ON;
  DoneApp:= DEF_DONE_APP;
  DoneAppEnabled:= DEF_DONE_APP_ON;
  // Fonts
  FFont:= TFontConfig.Create;
  Font.Name:= DEF_FONT_NAME;
  Font.Charset:= DEF_FONT_CHARSET;
  Font.Color:= DEF_FONT_COLOR;
  Font.BgColor:= DEF_FONT_BG_COLOR;
  Font.Size:= DEF_FONT_SIZE;
  Font.Style:= IntToFontStyles(DEF_FONT_STYLE);
  // CompactMode
  CompactMode := TCompactModeConfig.Create;
  CompactMode.Enabled:= DEF_COMPACT_MODE;
  CompactMode.Transparency:= DEF_COMPACT_MODE_TRANSPARENCY;
  CompactMode.AlphaValue:= DEF_COMPACT_MODE_ALPHA;

  FDefaultFont:= TFontConfig.Create;
  FDefaultFont.Name:= DEF_FONT_NAME;
  FDefaultFont.Charset:= DEF_FONT_CHARSET;
  FDefaultFont.Color:= DEF_FONT_COLOR;
  FDefaultFont.BgColor:= DEF_FONT_BG_COLOR;
  FDefaultFont.Size:= DEF_FONT_SIZE;
  FDefaultFont.Style:= IntToFontStyles(DEF_FONT_STYLE);
end;

destructor TConfig.Free;
begin
  FFont.Free;
  FDefaultFont.Free;
end;

function ReadInt(InIfile: TIniFile; const Section, Ident: String;  Min, Max, Default: Integer) : Integer;
begin
  Result:= IniFile.ReadInteger(Section, Ident, Default);
  if Result < Min then
    Result:= Min;
  if Result > Max then
    Result:= Max;
end;

function TConfig.Load : Boolean;
var IniFile: TIniFile;
begin
  try
    IniFile := TIniFile.Create(INI_FILE_NAME);
    // Main
    Minutes := IniFile.ReadInteger(INI_SEC_MAIN, INI_MINUTES, DEF_TIME);
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
    // Placement
    WndHeight := IniFile.ReadInteger(INI_SEC_PLACEMENT, INI_HEIGHT, DEF_HEIGHT);
    WndWidth := IniFile.ReadInteger(INI_SEC_PLACEMENT, INI_WIDTH, DEF_WIDTH);
    WndLeft := IniFile.ReadInteger(INI_SEC_PLACEMENT, INI_LEFT, 0);
    WndTop := IniFile.ReadInteger(INI_SEC_PLACEMENT, INI_TOP, 0);
    WndPosition := TPosition(ReadInt(IniFile, INI_SEC_PLACEMENT, INI_POSITION, Ord(_TPositionFirst), Ord(_TPositionLast), DEF_POSITION));
    // Alarm
    DoneMessage := IniFile.ReadString(INI_SEC_ALARMS, INI_DONE_MESSAGE, DEF_DONE_MSG);
    DoneMessageEnabled := IniFile.ReadBool(INI_SEC_ALARMS, INI_DONE_MESSAGE_ON, DEF_DONE_MSG_ON);
    DoneTrayMsg := IniFile.ReadString(INI_SEC_ALARMS, INI_DONE_TRAY_MESSAGE, DEF_DONE_TRAY_MSG);
    DoneTrayMsgEnabled := IniFile.ReadBool(INI_SEC_ALARMS, INI_DONE_TRAY_MESSAGE_ON, DEF_DONE_TRAY_MSG_ON);
    DoneAudio := IniFile.ReadString(INI_SEC_ALARMS, INI_DONE_AUDIO, DEF_DONE_AUDIO);
    DoneAudioEnabled := IniFile.ReadBool(INI_SEC_ALARMS, INI_DONE_AUDIO_ON, DEF_DONE_AUDIO_ON);
    DoneApp := IniFile.ReadString(INI_SEC_ALARMS, INI_DONE_APP, DEF_DONE_APP);
    DoneAppEnabled := IniFile.ReadBool(INI_SEC_ALARMS, INI_DONE_APP_ON, DEF_DONE_APP_ON);
    // Fonts
    Font.Name := IniFile.ReadString(INI_SEC_FONTS, INI_FONT_NAME, DEF_FONT_NAME);
    Font.Charset := IniFile.ReadInteger(INI_SEC_FONTS, INI_FONT_CHARSET, DEF_FONT_CHARSET);
    Font.Color := IniFile.ReadInteger(INI_SEC_FONTS, INI_FONT_COLOR, DEF_FONT_COLOR);
    Font.BgColor := IniFile.ReadInteger(INI_SEC_FONTS, INI_BG_COLOR, DEF_FONT_BG_COLOR);
    Font.Size := IniFile.ReadInteger(INI_SEC_FONTS, INI_FONT_SIZE, DEF_FONT_SIZE);
    Font.Style := IntToFontStyles(IniFile.ReadInteger(INI_SEC_FONTS, INI_FONT_STYLE, DEF_FONT_STYLE));
    // CompactMode
    CompactMode.Enabled := IniFile.ReadBool(INI_SEC_COMPACT_MODE, INI_COMPACT_MODE_ENABLED, DEF_COMPACT_MODE);
    CompactMode.Transparency:= TCompactModeTransparency(ReadInt(IniFile, INI_SEC_COMPACT_MODE, INI_COMPACT_MODE_TRANSPARENCY,
      Ord(_TCompactModeTransparencyFirst), Ord(_TCompactTransparencyLast), Ord(DEF_COMPACT_MODE_TRANSPARENCY)));
    CompactMode.AlphaValue:= ReadInt(Inifile, INI_SEC_COMPACT_MODE, INI_COMPACT_MODE_ALPHA_VALUE, COMPACT_MODE_ALPHA_MIN, 255, DEF_COMPACT_MODE_ALPHA);
  except
    Result:= False;
  end;
  Result:= True;

  if Assigned(IniFile) then
    IniFile.Free;

{$IFDEF LINUX}
  // Not supported on Linux
  LoopAudio:= False;
  TickingOn:= False;
  if CompactMode.Transparency = TransparentBackground then
    CompactMode.Transparency:= None;
{$ENDIF}
end;

function TConfig.Save : Boolean;
var IniFile: TIniFile;
begin
  try
    IniFile := TIniFile.Create(INI_FILE_NAME);
    IniFile.CacheUpdates := True;
    // Main
    IniFile.WriteInteger(INI_SEC_MAIN, INI_MINUTES, Minutes);
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
    // Placement
    IniFile.WriteInteger(INI_SEC_PLACEMENT, INI_HEIGHT, WndHeight);
    IniFile.WriteInteger(INI_SEC_PLACEMENT, INI_WIDTH, WndWidth);
    IniFile.WriteInteger(INI_SEC_PLACEMENT, INI_LEFT, WndLeft);
    IniFile.WriteInteger(INI_SEC_PLACEMENT, INI_TOP, WndTop);
    IniFile.WriteInteger(INI_SEC_PLACEMENT, INI_POSITION, Ord(WndPosition));
    // Alarms
    IniFile.WriteString(INI_SEC_ALARMS, INI_DONE_MESSAGE, DoneMessage);
    IniFile.WriteBool(INI_SEC_ALARMS, INI_DONE_MESSAGE_ON, DoneMessageEnabled);
    IniFile.WriteString(INI_SEC_ALARMS, INI_DONE_TRAY_MESSAGE, DoneTrayMsg);
    IniFile.WriteBool(INI_SEC_ALARMS, INI_DONE_TRAY_MESSAGE_ON, DoneTrayMsgEnabled);
    IniFile.WriteString(INI_SEC_ALARMS, INI_DONE_AUDIO, DoneAudio);
    IniFile.WriteBool(INI_SEC_ALARMS, INI_DONE_AUDIO_ON, DoneAudioEnabled);
    IniFile.WriteString(INI_SEC_ALARMS, INI_DONE_APP, DoneApp);
    IniFile.WriteBool(INI_SEC_ALARMS, INI_DONE_APP_ON, DoneAppEnabled);
    // Fonts
    IniFile.WriteString(INI_SEC_FONTS, INI_FONT_NAME, Font.Name);
    IniFile.WriteInteger(INI_SEC_FONTS, INI_FONT_CHARSET, Font.Charset);
    IniFile.WriteInteger(INI_SEC_FONTS, INI_FONT_COLOR, Font.Color);
    IniFile.WriteInteger(INI_SEC_FONTS, INI_BG_COLOR, Font.BgColor);
    IniFile.WriteInteger(INI_SEC_FONTS, INI_FONT_SIZE, Font.Size);
    IniFile.WriteInteger(INI_SEC_FONTS, INI_FONT_STYLE, FontStylesToInt(Font.Style));
    // CompactMode
    IniFile.WriteBool(INI_SEC_COMPACT_MODE, INI_COMPACT_MODE_ENABLED, CompactMode.Enabled);
    IniFile.WriteInteger(INI_SEC_COMPACT_MODE, INI_COMPACT_MODE_TRANSPARENCY, Ord(CompactMode.Transparency));
    IniFile.WriteInteger(INI_SEC_COMPACT_MODE, INI_COMPACT_MODE_ALPHA_VALUE, CompactMode.AlphaValue);

    IniFile.UpdateFile;
  except
    Result:= False;
  end;
  Result:= True;

  if Assigned(IniFile) then
    IniFile.Free;
end;

function TConfig.GetDefaultFont : TFontConfig;
begin
  Result:= FDefaultFont;
end;


function GetConfig : TConfig;
begin
  Result:= ConfigInstance;
end;

initialization
  ConfigInstance:= TConfig.Create;

end.


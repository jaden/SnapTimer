unit utils;

{$mode objfpc}{$H+}

interface

// TUtils class serves as a namespace.
type
  TUtils = class
    class function SecondsToTime(Seconds: integer; HideSeconds: Boolean): string;
    class function GetFilePath(Path: string): string;
    class procedure RunApp(Path: string);
    class function IsInteger(S: String): boolean;
  end;

implementation

uses
  Classes, SysUtils, StrUtils, Forms, Windows;

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

class procedure TUtils.RunApp(Path: string);
begin
  ShellExecute(0, 'open', PChar(GetFilePath(Path)), nil, nil, SW_SHOWNORMAL);
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

end.


unit CompactMode;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls, Config;

type

  { TCompactModeForm }

  TCompactModeForm = class(TForm)
    Count: TPanel;
    procedure CountClick(Sender: TObject);
    procedure CountDblClick(Sender: TObject);

    procedure SetTransparency(Value: TCompactModeConfig);
  protected
    procedure SetColor(Value: TColor); override;
  private
  end;

var
  CompactModeForm: TCompactModeForm;

implementation

uses MainController
{$IFDEF WINDOWS}
  ,Windows
{$ENDIF}
  ;

{ TCompactModeForm }

procedure TCompactModeForm.CountClick(Sender: TObject);
begin
  GetMainController.ToggleCountdown(Sender);
end;

procedure TCompactModeForm.CountDblClick(Sender: TObject);
begin
  GetMainController.ResetCountdown(Sender);
end;


procedure TCompactModeForm.SetTransparency(Value: TCompactModeConfig);
begin
  case Value.Transparency of
    None:
    begin
      AlphaBlend:= False;
    end;

    TransparentBackground:
    begin
{$IFDEF WINDOWS}
      // Antialiased fonts create weird visual artifact.
      Count.Font.Quality:= fqNonAntialiased;
      SetWindowLongPtr(Self.Handle, GWL_EXSTYLE,GetWindowLongPtr(Self.Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
      SetLayeredWindowAttributes(Self.Handle, Count.Color, 0, LWA_COLORKEY);
{$ENDIF}
    end;

    AlphaBlending:
    begin
      AlphaBlend:= True;
      AlphaBlendValue:= Value.AlphaValue;
    end;
  end;
end;

procedure TCompactModeForm.SetColor(Value: TColor);
begin
  inherited SetColor(Value);
{$IFDEF WINDOWS}
  if GetConfig.CompactMode.Transparency = TransparentBackground then
    SetLayeredWindowAttributes(Self.Handle, Value, 0, LWA_COLORKEY);
{$ENDIF}
end;


initialization
  {$I compactmode.lrs}

end.


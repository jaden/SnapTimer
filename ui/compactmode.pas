unit CompactMode;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TCompactModeForm }

  TCompactModeForm = class(TForm)
    Count: TPanel;
    procedure CountClick(Sender: TObject);
    procedure CountDblClick(Sender: TObject);
  private

  public

  end;

var
  CompactModeForm: TCompactModeForm;

implementation

uses MainController;

{ TCompactModeForm }

procedure TCompactModeForm.CountClick(Sender: TObject);
begin
  GetMainController.ToggleCountdown(Sender);
end;

procedure TCompactModeForm.CountDblClick(Sender: TObject);
begin
  GetMainController.ResetCountdown(Sender);
end;

initialization
  {$I compactmode.lrs}

end.


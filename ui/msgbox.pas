unit msgbox;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TMsgBoxForm }

  TMsgBoxForm = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private

  public
    procedure SetText(Msg: String);
  end;

var
  MsgBoxForm: TMsgBoxForm;

implementation

{ TMsgBoxForm }

procedure TMsgBoxForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TMsgBoxForm.SetText(Msg: String);
const MinFormSize = 162;
      TextThresholdWidth = 68;
      DefButtonLeft = 40;
var TextW,Delta : Integer;
begin
  Label1.Caption:= Msg;
  TextW:= Label1.Canvas.GetTextWidth(Msg);
  if TextW > TextThresholdWidth then
  begin
    Delta:= TextW - TextThresholdWidth;
    Width:= MinFormSize + Delta;
    Button1.Left:= DefButtonLeft + Delta div 2;
  end;
end;

initialization
  //{$I msgbox.lrs}
  {$R msgbox.lfm}

end.


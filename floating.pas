unit Floating;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, StdCtrls;

type

  { TFloatingForm }

  TFloatingForm = class(TForm)
    procedure DisableFloat(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FloatingForm: TFloatingForm;

implementation
uses MainForm1;
{ TFloatingForm }

// TODO Get this working
// Set location too

procedure TFloatingForm.DisableFloat(Sender: TObject);
begin
    Hide;
end;

initialization
  {$I floating.lrs}

end.


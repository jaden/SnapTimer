program snaptimer;

{$mode objfpc}{$H+}

uses
  Forms, Interfaces, MainForm1;

{$R *.res}

begin
  Application.Title:='SnapTimer';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


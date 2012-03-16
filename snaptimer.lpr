program snaptimer;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, MainForm1, About, settings;

{$R *.res}

begin
  Application.Title:='SnapTimer';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


program snaptimer;

{$mode objfpc}{$H+}

uses
  Forms, Interfaces, MainForm1, utils, CompactMode;

{$R *.res}

begin
  Application.Title:='SnapTimer';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TCompactModeForm, CompactModeForm);
  Application.Run;
end.


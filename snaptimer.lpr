program snaptimer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm1, LResources, About, settings;

{$IFDEF WINDOWS}{$R snaptimer.rc}{$ENDIF}

begin
  Application.Title:='SnapTimer';
  {$I snaptimer.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


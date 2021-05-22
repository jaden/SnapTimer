unit Consts;

{$mode objfpc}

interface

const
  APP_NAME = 'SnapTimer';

  // Menu items
  MENU_FILE      = '&File';
  MENU_EXIT      = 'E&xit';
  MENU_EDIT      = '&Edit';
  MENU_OPTIONS   = 'Op&tions';
  MENU_HELP      = '&Help';
  MENU_ABOUT     = '&About';
  TRAY_MENU_SHOW = 'Show Application';
  TRAY_MENU_HIDE = 'Hide Application';

  // Buttons
  BTN_START = '&Start';
  BTN_PAUSE = '&Pause';
  BTN_RESET = '&Reset';

  // Labels
  LBL_MINUTES = '&Minutes:';
  LBL_SECONDS = '&Seconds:';

  // Messages
  MSG_OPEN_INI  = 'An error occurred opening the .ini file, settings won''t be saved';
  MSG_WRITE_INI = 'An error occurred trying to write to the .ini file, settings won''t be saved.';

{$IFDEF WINDOWS}
    SEPARATOR = '\';
{$ENDIF}
{$IFDEF LINUX}
    SEPARATOR = '/';
{$ENDIF}


implementation

end.


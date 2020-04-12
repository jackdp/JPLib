unit JPL.Win.Dialogs;

interface

{$IFDEF MSWINDOWS}

{$I .\..\jp.inc}
{$IFDEF FPC}{$MODE OBJFPC}{$H+}{$ENDIF}

uses
  Windows;

{
  Information = Asterisks
  Warning = Exclamation
  Error = Hand = Stop
}

procedure WinMsg(const Text, Caption: string; Handle: HWND = 0; MBType: DWORD = MB_OK or MB_ICONINFORMATION);
procedure MB(const Text: string; Caption: string = 'Information'; Handle: HWND = 0);

procedure WinMsgInfo(Text: string; Caption: string = 'Information'; Handle: HWND = 0);
procedure WinMsgWarning(Text: string; Caption: string = 'Warning'; Handle: HWND = 0);
procedure WinMsgError(Text: string; Caption: string = 'Error'; Handle: HWND = 0);


{$ENDIF} // MSWINDOWS


implementation


{$IFDEF MSWINDOWS}

procedure WinMsg(const Text, Caption: string; Handle: HWND = 0; MBType: DWORD = MB_OK or MB_ICONINFORMATION);
begin
  MessageBox(Handle, PChar(Text), PChar(Caption), MBType);
end;

procedure MB(const Text: string; Caption: string = 'Information'; Handle: HWND = 0);
begin
  WinMsgInfo(Text, Caption, Handle);
end;

procedure WinMsgInfo(Text: string; Caption: string = 'Information'; Handle: HWND = 0);
begin
  WinMsg(Text, Caption, Handle, MB_OK or MB_ICONINFORMATION);
end;

procedure WinMsgWarning(Text: string; Caption: string = 'Warning'; Handle: HWND = 0);
begin
  WinMsg(Text, Caption, Handle, MB_OK or MB_ICONWARNING);
end;

procedure WinMsgError(Text: string; Caption: string = 'Error'; Handle: HWND = 0);
begin
  WinMsg(Text, Caption, Handle, MB_OK or MB_ICONERROR);
end;

{$ENDIF} // MSWINDOWS

end.


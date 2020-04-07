unit JPL.Dialogs;

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Graphics, {$IFDEF DCC}System.UITypes, {$ENDIF}
  Forms, Dialogs, Controls, StdCtrls;



type
  TWinStrParams = record
    FormCaption: string;
    FormWidth: integer;
    FormHeight: integer;
    FormColor: TColor;
    MemoText: string;
    MemoBgColor: TColor;
    MemoFontName: string;
    MemoFontSize: integer;
    MemoFontColor: TColor;
    MemoMarginLeft: integer;
    MemoMarginRight: integer;
    MemoMarginTop: integer;
    MemoMarginBottom: integer;
    MemoHideScrollbars: Boolean;
    //EscExit: Boolean;
  end;

procedure ShowMsg(const Text: string; DlgType: TMsgDlgType = mtInformation; Buttons: TMsgDlgButtons = [mbOK]; HelpContext: integer = 0);

procedure Msg(const Text: string);
procedure MsgInfo(const Text: string);
procedure MsgWarning(const Text: string);
procedure MsgError(const Text: string);
procedure MsgInt(const x: Int64);
procedure MsgStrVal(const Text: string; const x: Int64; TextSuffix: string = ' = '); overload; // string + integer msg
procedure MsgStrVal(const Text: string; const x: Extended; TextSuffix: string = ' = '; FormatStr: string = '0.0000'); overload; // string + float msg
procedure MsgFloat(x: Extended; FormatStr: string = '0.0000');


function ShowWinStr(const MemoText: string; Caption: string = ''; Width: integer = 500; Height: integer = 400; MemoFontName: string = 'Consolas'; MemoFontSize: integer = 9): string; overload;
function ShowWinStr(WinStrParams: TWinStrParams; UseDefaultMargins: Boolean = True): string; overload;

procedure MsgBool(const b: Boolean; TrueStr: string = 'True'; FalseStr: string = 'False');
procedure MsgBoolYN(const b: Boolean);
procedure MsgBoolTF(const b: Boolean);


implementation
uses
  JPL.Strings;


procedure MsgBool(const b: Boolean; TrueStr: string = 'True'; FalseStr: string = 'False');
begin
  if b then Msg(TrueStr) else Msg(FalseStr);
end;

procedure MsgBoolTF(const b: Boolean);
begin
  MsgBool(b, 'True', 'False');
end;

procedure MsgBoolYN(const b: Boolean);
begin
  MsgBool(b, 'Yes', 'No');
end;

function ShowWinStr(WinStrParams: TWinStrParams; UseDefaultMargins: Boolean = True): string; overload;
var
  Form: TForm;
  Memo: TMemo;
begin
  Form := TForm.Create(nil);
  Memo := TMemo.Create(Form);

  Form.Constraints.MinHeight := 100;
  Form.Constraints.MinWidth := 200;
  if WinStrParams.MemoHideScrollbars then Memo.ScrollBars := ssNone
  else Memo.ScrollBars := ssBoth;
  Memo.Parent := Form;
  if UseDefaultMargins then
  begin
    WinStrParams.MemoMarginLeft := 3;
    WinStrParams.MemoMarginRight := 3;
    WinStrParams.MemoMarginTop := 3;
    WinStrParams.MemoMarginBottom := 3;
  end;
  try

    Memo.Text := WinStrParams.MemoText;
    Memo.Font.Size := WinStrParams.MemoFontSize;
    Memo.Font.Name := WinStrParams.MemoFontName;
    Memo.Font.Color := WinStrParams.MemoFontColor;
    Memo.Color := WinStrParams.MemoBgColor;
    Memo.BorderStyle := bsNone;

    {$IFDEF DCC}
    Memo.Margins.Left := WinStrParams.MemoMarginLeft;
    Memo.Margins.Right := WinStrParams.MemoMarginRight;
    Memo.Margins.Top := WinStrParams.MemoMarginTop;
    Memo.Margins.Bottom := WinStrParams.MemoMarginBottom;
    Memo.AlignWithMargins := True;
    {$ENDIF}
    Memo.Align := alClient;
    //Form.FormStyle := fsStayOnTop;
    Form.BorderIcons := [biSystemMenu];
    Form.Caption := WinStrParams.FormCaption;
    Form.Width := WinStrParams.FormWidth;
    Form.Height := WinStrParams.FormHeight;
    Form.Color := WinStrParams.FormColor;
    Form.Position := poOwnerFormCenter;
    Memo.Align := alClient;
    Form.Repaint;
    Form.ShowModal;
  finally
    Result := Memo.Text;
    Form.Free;
  end;
end;


function ShowWinStr(const MemoText: string; Caption: string = ''; Width: integer = 500; Height: integer = 400; MemoFontName: string = 'Consolas'; MemoFontSize: integer = 9): string; overload;
var
  Form: TForm;
  Memo: TMemo;
  Arr: {$IFDEF FPC}specialize{$ENDIF} TArray<string>;
  s: string;
  i: integer;
begin
  if Pos('|', MemoFontName) > 0 then
  begin
    SplitStrToArray(MemoFontName, Arr, '|');
    for i := 0 to High(Arr) do
    begin
      s := Arr[i];
      if Screen.Fonts.IndexOf(s) > 0 then
      begin
        MemoFontName := s;
        Break;
      end;
    end;
  end;

  Form := TForm.Create(nil);
  Memo := TMemo.Create(Form);
  Memo.ScrollBars := ssBoth;
  Memo.Parent := Form;
  try
    Memo.Align := alClient;
    Memo.Text := MemoText;
    Memo.Font.Size := MemoFontSize;
    Memo.Font.Name := MemoFontName;
    Memo.ScrollBars := ssVertical;
    Form.FormStyle := fsStayOnTop;
    Form.BorderIcons := [biSystemMenu];
    Form.Caption := Caption;
    Form.Width := Width;
    Form.Height := Height;
    Form.Position := poOwnerFormCenter;
    Form.Repaint;
    Form.ShowModal;
  finally
    Result := Memo.Text;
    Form.Free;
  end;
end;

procedure ShowMsg(const Text: string; DlgType: TMsgDlgType = mtInformation; Buttons: TMsgDlgButtons = [mbOK]; HelpContext: integer = 0);
begin
  MessageDlg(Text, DlgType, Buttons, HelpContext);
end;

procedure Msg(const Text: string);
begin
  ShowMsg(Text, mtCustom, [mbOK]);
end;

procedure MsgInfo(const Text: string);
begin
  ShowMsg(Text, mtInformation, [mbOK]);
end;

procedure MsgWarning(const Text: string);
begin
  ShowMsg(Text, mtWarning, [mbOK]);
end;

procedure MsgError(const Text: string);
begin
  ShowMsg(Text, mtError, [mbOK]);
end;

procedure MsgInt(const x: Int64);
begin
  Msg(IntToStr(x));
end;

procedure MsgStrVal(const Text: string; const x: Int64; TextSuffix: string = ' = ');
begin
  Msg(Text + TextSuffix + IntToStr(x));
end;

procedure MsgFloat(x: Extended; FormatStr: string = '0.0000');
begin
  Msg(FormatFloat(FormatStr, x));
end;

procedure MsgStrVal(const Text: string; const x: Extended; TextSuffix: string = ' = '; FormatStr: string = '0.0000');
begin
  Msg(Text + TextSuffix + FormatFloat(FormatStr, x));
end;




end.

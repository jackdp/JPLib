unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls, ComCtrls,
  JPL.Win.Mouse;

type
  TForm1 = class(TForm)
    actEsc: TAction;
    actInitControls: TAction;
    ActionList1: TActionList;
    btnDisableSonar: TButton;
    btnEnableSonar: TButton;
    chSwapButtons: TCheckBox;
    chTrailsEnabled: TCheckBox;
    edScrollVertical: TEdit;
    edScrollHorizontal: TEdit;
    gbSonar: TGroupBox;
    gbTrails: TGroupBox;
    gbScrolling: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    lblScrollVerticalInfo: TLabel;
    lblScrollHorizontal: TLabel;
    lblTitle: TLabel;
    lblTrails_Cursors: TLabel;
    lblSonarInfo: TLabel;
    Memo1: TMemo;
    tbTrails: TTrackBar;
    udScrollVertical: TUpDown;
    udScrollHorizontal: TUpDown;
    procedure actEscExecute(Sender: TObject);
    procedure actInitControlsExecute(Sender: TObject);
    procedure btnDisableSonarClick(Sender: TObject);
    procedure btnEnableSonarClick(Sender: TObject);
    procedure chSwapButtonsChange(Sender: TObject);
    procedure chTrailsEnabledChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure InitControls;
    procedure tbTrailsChange(Sender: TObject);
    procedure udScrollHorizontalClick(Sender: TObject; {%H-}Button: TUDBtnType);
    procedure udScrollVerticalClick(Sender: TObject; {%H-}Button: TUDBtnType);
  private
    FMouse: TJPMouse;
    FUpdatingControls: Boolean;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}



procedure TForm1.FormCreate(Sender: TObject);
begin
  FMouse := TJPMouse.Create;
  InitControls;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FMouse.Free;
end;

procedure TForm1.InitControls;
begin
  FUpdatingControls := True;
  try
    FMouse.UpdateAllInformations;

    btnEnableSonar.Enabled := not FMouse.SonarEnabled;
    btnDisableSonar.Enabled := FMouse.SonarEnabled;

    chTrailsEnabled.Checked := FMouse.TrailsEnabled;
    tbTrails.Enabled := FMouse.TrailsEnabled;
    if FMouse.TrailsEnabled then tbTrails.Position := FMouse.TrailsCount;
    tbTrailsChange(Self);

    chSwapButtons.Checked := FMouse.ButtonsSwapped;

    udScrollVertical.Position := FMouse.WheelScrollLines;
    udScrollHorizontal.Position := FMouse.WheelScrollChars;

    Memo1.Text := FMouse.GetReportStr;
  finally
    FUpdatingControls := False;
  end;
end;

procedure TForm1.tbTrailsChange(Sender: TObject);
begin
  if not chTrailsEnabled.Checked then Exit;
  FMouse.TrailsCount := tbTrails.Position;
  lblTrails_Cursors.Caption := 'Number of cursors: ' + IntToStr(FMouse.TrailsCount);
end;

procedure TForm1.udScrollHorizontalClick(Sender: TObject; Button: TUDBtnType);
begin
  if FUpdatingControls then Exit;
  FMouse.WheelScrollChars := udScrollHorizontal.Position;
  InitControls;
end;

procedure TForm1.udScrollVerticalClick(Sender: TObject; Button: TUDBtnType);
begin
  if FUpdatingControls then Exit;
  FMouse.WheelScrollLines := udScrollVertical.Position;
  InitControls;
end;

procedure TForm1.btnDisableSonarClick(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  FMouse.SonarEnabled := False;
  InitControls;
end;

procedure TForm1.btnEnableSonarClick(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  FMouse.SonarEnabled := True;
  InitControls;
end;

procedure TForm1.chSwapButtonsChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  FMouse.ButtonsSwapped := chSwapButtons.Checked;
  InitControls;
end;

procedure TForm1.chTrailsEnabledChange(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  if chTrailsEnabled.Checked then FMouse.TrailsCount := tbTrails.Position
  else FMouse.TrailsCount := 1;
  InitControls;
end;


procedure TForm1.actEscExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.actInitControlsExecute(Sender: TObject);
begin
  InitControls;
end;

end.


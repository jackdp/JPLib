program MouseDemo;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1;

{$R *.res}

begin
  {$IF DECLARED(UseHeapTrace)}
	GlobalSkipIfNoLeaks := True; // supported as of debugger version 3.2.0
  {$ENDIF}

  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


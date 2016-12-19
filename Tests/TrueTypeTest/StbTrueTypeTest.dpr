program StbTrueTypeTest;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  FMain in 'FMain.pas' {FormMain},
  Neslib.Stb.TrueType in '..\..\Stb\Neslib.Stb.TrueType.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

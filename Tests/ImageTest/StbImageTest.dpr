program StbImageTest;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  FMain in 'FMain.pas' {FormMain},
  Neslib.Stb.Common in '..\..\Stb\Neslib.Stb.Common.pas',
  Neslib.Stb.Image in '..\..\Stb\Neslib.Stb.Image.pas',
  Neslib.Stb.ImageWrite in '..\..\Stb\Neslib.Stb.ImageWrite.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

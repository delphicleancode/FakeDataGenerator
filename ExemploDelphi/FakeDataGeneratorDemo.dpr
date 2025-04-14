program FakeDataGeneratorDemo;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form6},
  FakeDataGenerator in '..\FakeDataGenerator.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.

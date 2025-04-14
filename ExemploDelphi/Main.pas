unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm6 = class(TForm)
    memDadosFake: TMemo;
    lblTitulo: TLabel;
    btnGerar: TButton;
    procedure btnGerarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation
  uses FakeDataGenerator;

{$R *.dfm}

procedure TForm6.btnGerarClick(Sender: TObject);
var
  FakeData: TFakeDataGenerator;
  RazaoSocial: string;
begin
  try
    memDadosFake.Clear;
    Randomize;
    FakeData := TFakeDataGenerator.Create;
    memDadosFake.Lines.Add('=============================================');

    memDadosFake.Lines.Add('');
    memDadosFake.Lines.Add('.:: PESSOA FÍSICA ::.');
    memDadosFake.Lines.Add('CPF: '                      + FakeData.GerarCPF);
    memDadosFake.Lines.Add('RG: '                       + FakeData.GerarRG);
    memDadosFake.Lines.Add('PIS: '                      + FakeData.GerarPIS);
    memDadosFake.Lines.Add('Título de Eleitor: '        + FakeData.GerarTituloEleitor);
    memDadosFake.Lines.Add('Telefone: '                 + FakeData.GerarTelefone);
    memDadosFake.Lines.Add('Celular: '                  + FakeData.GerarCelular);
    memDadosFake.Lines.Add('Nome Completo: '            + FakeData.GerarNomeCompleto);
    memDadosFake.Lines.Add('Nome Completo Feminino: '   + FakeData.GerarNomeCompleto(True));
    memDadosFake.Lines.Add('Cartão Visa: '              + FakeData.GerarCartaoCredito('V'));
    memDadosFake.Lines.Add('Cartão aleatório: '         + FakeData.GerarCartaoCredito);
    memDadosFake.Lines.Add('Conta Bancária aleatória: ' + FakeData.GerarContaBancaria);
    memDadosFake.Lines.Add('Conta BB: '                 + FakeData.GerarContaBancaria('001', False));
    memDadosFake.Lines.Add('');

    memDadosFake.Lines.Add('.:: EMPRESA ::.');
    RazaoSocial := FakeData.GerarRazaoSocial;
    memDadosFake.Lines.Add('CNPJ: '         + FakeData.GerarCNPJ);
    memDadosFake.Lines.Add('Razão Social: ' + RazaoSocial);
    memDadosFake.Lines.Add('Nome Fantasia: '+ FakeData.GerarNomeFantasia(RazaoSocial));
    memDadosFake.Lines.Add('Endereço: '     + FakeData.GerarLogradouro + ', '+ FakeData.GerarNumero);
    memDadosFake.Lines.Add('Bairro: '       + FakeData.GerarBairro);
    memDadosFake.Lines.Add('Cidade/UF: '    + FakeData.GerarCidade+ '/'+ FakeData.GerarUF);
    memDadosFake.Lines.Add('CEP: '          + FakeData.GerarCEP);
    memDadosFake.Lines.Add('Email: '        + FakeData.GerarEmail(FakeData.GerarNomeCompleto));
    memDadosFake.Lines.Add('Documento: '    + FakeData.GerarDocumento);

    memDadosFake.Lines.Add('');
    memDadosFake.Lines.Add('.:: VEÍCULO ::.');
    memDadosFake.Lines.Add('RENAVAM: '             + FakeData.GerarRENAVAM);
    memDadosFake.Lines.Add('Placa (tradicional): ' + FakeData.GerarPlacaVeiculo);
    memDadosFake.Lines.Add('Placa (Mercosul): '    + FakeData.GerarPlacaVeiculo(True));

    memDadosFake.Lines.Add('');
    memDadosFake.Lines.Add('=============================================');
  finally
    FakeData.Free;
  end;
end;

end.

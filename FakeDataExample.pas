program FakeDataExample;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  FakeDataGenerator in 'FakeDataGenerator.pas';

var
  FakeData: TFakeDataGenerator;
  i: Integer;
  SQL: string;
  RazaoSocial: string;

begin
  try
    Randomize;
    FakeData := TFakeDataGenerator.Create;
    try
      WriteLn('Gerador de Dados Fictícios para ERP Financeiro');
      WriteLn('=============================================');
      WriteLn('');
      
      // Mostrar exemplos de dados gerados
      WriteLn('CPF: ', FakeData.GerarCPF);
      WriteLn('CNPJ: ', FakeData.GerarCNPJ);
      WriteLn('Telefone: ', FakeData.GerarTelefone);
      WriteLn('Celular: ', FakeData.GerarCelular);
      WriteLn('Nome Completo: ', FakeData.GerarNomeCompleto);
      WriteLn('Nome Completo Feminino: ', FakeData.GerarNomeCompleto(True));
      
      RazaoSocial := FakeData.GerarRazaoSocial;
      WriteLn('Razão Social: ', RazaoSocial);
      WriteLn('Nome Fantasia: ', FakeData.GerarNomeFantasia(RazaoSocial));
      WriteLn('Endereço: ', FakeData.GerarLogradouro, ', ', FakeData.GerarNumero);
      WriteLn('Bairro: ', FakeData.GerarBairro);
      WriteLn('Cidade/UF: ', FakeData.GerarCidade, '/', FakeData.GerarUF);
      WriteLn('CEP: ', FakeData.GerarCEP);
      WriteLn('Email: ', FakeData.GerarEmail(FakeData.GerarNomeCompleto));
      WriteLn('Documento: ', FakeData.GerarDocumento);
      
      WriteLn('');
      
      WriteLn('Pressione ENTER para sair...');
      ReadLn;
    finally
      FakeData.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('Erro: ', E.Message);
      ReadLn;
    end;
  end;
end.

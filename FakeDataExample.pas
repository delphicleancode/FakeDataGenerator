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
  NomeCompleto: string;

begin
  try
    Randomize;
    FakeData := TFakeDataGenerator.Create;
    try
      WriteLn('Gerador de Dados Fictícios para ERP Financeiro');
      WriteLn('=============================================');
      WriteLn('');
      
      // Mostrar exemplos de dados básicos
      WriteLn('-- Dados Básicos --');
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
      NomeCompleto := FakeData.GerarNomeCompleto;
      WriteLn('Email: ', FakeData.GerarEmail(NomeCompleto));
      WriteLn('Documento: ', FakeData.GerarDocumento);
      
      WriteLn('');
      
      // Exemplos de outros documentos
      WriteLn('-- Documentos Adicionais --');
      WriteLn('CNH: ', FakeData.GerarCNH);
      WriteLn('RG: ', FakeData.GerarRG);
      WriteLn('PIS/PASEP: ', FakeData.GerarPIS);
      WriteLn('Título de Eleitor: ', FakeData.GerarTituloEleitor);
      WriteLn('RENAVAM: ', FakeData.GerarRENAVAM);
      WriteLn('Placa de Veículo (Padrão Antigo): ', FakeData.GerarPlacaVeiculo(False));
      WriteLn('Placa de Veículo (Padrão Mercosul): ', FakeData.GerarPlacaVeiculo(True));
      WriteLn('Inscrição Estadual (SP): ', FakeData.GerarInscricaoEstadual('SP'));
      
      WriteLn('');
      
      // Dados pessoais adicionais
      WriteLn('-- Dados Pessoais Adicionais --');
      WriteLn('Data de Nascimento: ', DateToStr(FakeData.GerarDataNascimento));
      WriteLn('Perfil Instagram: ', FakeData.GerarPerfilRedeSocial(NomeCompleto, 'instagram'));
      WriteLn('Perfil LinkedIn: ', FakeData.GerarPerfilRedeSocial(NomeCompleto, 'linkedin'));
      WriteLn('Estado Civil: ', FakeData.GerarEstadoCivil);
      WriteLn('Profissão: ', FakeData.GerarProfissao);
      WriteLn('Escolaridade: ', FakeData.GerarEscolaridade);
      
      WriteLn('');
      
      // Dados empresariais e financeiros
      WriteLn('-- Dados Empresariais e Financeiros --');
      WriteLn('Cartão de Crédito (Visa): ', FakeData.GerarCartaoCredito('V'));
      WriteLn('Cartão de Crédito (Mastercard): ', FakeData.GerarCartaoCredito('M'));
      WriteLn('Conta Bancária: ', FakeData.GerarContaBancaria);
      WriteLn('CNAE: ', FakeData.GerarCNAE);
      WriteLn('Inscrição Municipal: ', FakeData.GerarInscricaoMunicipal);
      WriteLn('Departamento: ', FakeData.GerarDepartamento);
      WriteLn('Cargo: ', FakeData.GerarCargo);
      WriteLn('Chave PIX (CPF): ', FakeData.GerarChavePIX('cpf'));
      WriteLn('Chave PIX (Email): ', FakeData.GerarChavePIX('email'));
      WriteLn('Chave PIX (Aleatória): ', FakeData.GerarChavePIX('aleatoria'));
      
      WriteLn('');
      
      // Dados financeiros de documentos
      WriteLn('-- Documentos Financeiros --');
      WriteLn('Código de Barras do Boleto: ', FakeData.GerarCodigoBarrasBoleto);
      WriteLn('Linha Digitável do Boleto: ', FakeData.GerarLinhaDigitavelBoleto);
      WriteLn('Código de Rastreio: ', FakeData.GerarCodigoRastreio);
      WriteLn('Alíquota de ICMS: ', FormatFloat('0.00%', FakeData.GerarAliquotaImposto('ICMS')));
      WriteLn('Alíquota de ISS: ', FormatFloat('0.00%', FakeData.GerarAliquotaImposto('ISS')));
      WriteLn('Nota Fiscal: ', FakeData.GerarNotaFiscal);
      WriteLn('Data Aleatória (últimos 30 dias): ', DateToStr(FakeData.GerarData(Date-30, Date)));
      WriteLn('Valor Aleatório (R$ 100 a R$ 1000): R$ ', FormatFloat('#,##0.00', FakeData.GerarValor(100, 1000)));
      
      WriteLn('');
      
      // Dados de saúde
      WriteLn('-- Dados de Saúde --');
      WriteLn('Tipo Sanguíneo: ', FakeData.GerarTipoSanguineo);
      WriteLn('Altura: ', FakeData.GerarAltura, ' cm');
      WriteLn('Peso: ', FormatFloat('0.0', FakeData.GerarPeso), ' kg');
      WriteLn('Pressão Arterial: ', FakeData.GerarPressaoArterial);
      WriteLn('Medicamento: ', FakeData.GerarMedicamento);
      WriteLn('Especialidade Médica: ', FakeData.GerarEspecialidadeMedica);
      WriteLn('Plano de Saúde: ', FakeData.GerarPlanoSaude);
      
      WriteLn('');
      
      // Dados acadêmicos
      WriteLn('-- Dados Acadêmicos --');
      WriteLn('Instituição de Ensino: ', FakeData.GerarNomeInstituicaoEnsino);
      WriteLn('Curso de Graduação: ', FakeData.GerarCursoGraduacao);
      WriteLn('Área de Formação: ', FakeData.GerarAreaFormacao);
      WriteLn('Matrícula Acadêmica: ', FakeData.GerarMatriculaAcademica);
      WriteLn('Coeficiente de Rendimento: ', FormatFloat('0.00', FakeData.GerarCoeficienteRendimento));
      WriteLn('Data de Formatura: ', DateToStr(FakeData.GerarDataFormatura));
      WriteLn('Título da Monografia: ', FakeData.GerarTituloMonografia);
      
      WriteLn('');
      
      // Outros dados específicos
      WriteLn('-- Outros Dados Específicos --');
      WriteLn('Protocolo: ', FakeData.GerarProtocolo);
      WriteLn('SKU de Produto (Eletrônicos): ', FakeData.GerarSKU('ELET'));
      WriteLn('Código EAN-13: ', FakeData.GerarEAN13);
      WriteLn('Código CID: ', FakeData.GerarCID);
      WriteLn('Processo Judicial: ', FakeData.GerarProcessoJudicial);
      
      WriteLn('');
      
      // Exemplo de uso em SQL para cadastro de cliente
      WriteLn('-- Exemplo de SQL para Cadastro de Cliente --');
      WriteLn('INSERT INTO PESSOAS (');
      WriteLn('  TIPO, TIPO_CADASTRO, NOME_RAZAO, CPF_CNPJ, TELEFONE, CELULAR,');
      WriteLn('  EMAIL, ENDERECO, NUMERO, BAIRRO, CIDADE, UF, CEP, DATA_CADASTRO');
      WriteLn(') VALUES (');
      NomeCompleto := FakeData.GerarNomeCompleto;
      SQL := Format(
        '  ''F'', ''Cliente'', ''%s'', ''%s'', ''%s'', ''%s'','+
        '  ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', ''%s'', CURRENT_DATE',
        [
          NomeCompleto,
          FakeData.GerarCPF,
          FakeData.GerarTelefone,
          FakeData.GerarCelular,
          FakeData.GerarEmail(NomeCompleto),
          FakeData.GerarLogradouro,
          FakeData.GerarNumero,
          FakeData.GerarBairro,
          FakeData.GerarCidade,
          FakeData.GerarUF,
          FakeData.GerarCEP
        ]
      );
      WriteLn(SQL);
      WriteLn(');');
      
      WriteLn('');
      
      // Exemplo de uso em SQL para contas a receber
      WriteLn('-- Exemplo de SQL para Contas a Receber --');
      WriteLn('INSERT INTO CONTAS_RECEBER (');
      WriteLn('  ID_EMPRESA, ID_PESSOA, DOCUMENTO, EMISSAO, VENCIMENTO,');
      WriteLn('  VALOR, DESCRICAO, SITUACAO');
      WriteLn(') VALUES (');
      SQL := Format(
        '  1, 1, ''%s'', CURRENT_DATE, ''%s'','+
        '  %s, ''%s'', ''P''',
        [
          FakeData.GerarDocumento,
          FormatDateTime('yyyy-mm-dd', FakeData.GerarData(Date, Date + 30)),
          StringReplace(FormatFloat('0.00', FakeData.GerarValor(100, 5000)), ',', '.', [rfReplaceAll]),
          'Venda de produtos'
        ]
      );
      WriteLn(SQL);
      WriteLn(');');
      
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

# Biblioteca FakeDataGenerator para Delphi/Lazarus

Uma biblioteca Delphi/Lazarus para geração de dados fictícios brasileiros, ideal para preencher bancos de dados de teste ou demonstração.
Pode ser utilizado em sistemas CRM, ERP etc.

## 📋 Sumário

- [Recursos](#-recursos)
  - [Documentos Brasileiros](#-documentos-brasileiros)
  - [Dados Pessoais](#-dados-pessoais)
  - [Dados Empresariais e Financeiros](#-dados-empresariais-e-financeiros)
  - [Endereços Completos](#-endereços-completos)
  - [Valores e Datas](#-valores-e-datas)
  - [Dados de Saúde](#-dados-de-saúde)
  - [Dados Acadêmicos](#-dados-acadêmicos)
  - [Dados de Veículos](#-dados-de-veículos)
- [Instalação](#-instalação)
- [Como Usar](#-como-usar)
- [Exemplos](#-exemplos)
  - [Populando Empresas](#--populando-uma-tabela-de-empresas)
  - [Gerando Contas a Receber](#gerando-contas-a-receber)
- [Documentação da API](#-documentação-da-api)
  - [Métodos para Documentos](#métodos-para-documentos)
  - [Métodos para Dados Pessoais](#métodos-para-dados-pessoais)
  - [Métodos para Dados Empresariais e Financeiros](#métodos-para-dados-empresariais-e-financeiros)
  - [Métodos para Endereços](#métodos-para-endereços)
  - [Métodos para Valores e Datas](#métodos-para-valores-e-datas)
  - [Métodos para Dados de Saúde](#métodos-para-dados-de-saúde)
  - [Métodos para Dados Acadêmicos](#métodos-para-dados-acadêmicos)
  - [Métodos para Dados de Veículos](#métodos-para-dados-de-veículos)
  - [Métodos Utilitários](#métodos-utilitários)
- [Contribuindo](#-contribuindo)
- [Licença](#-licença)

[🔝](#-sumário)
## ✨ Recursos 

Esta biblioteca oferece métodos para gerar diversos tipos de dados fictícios, incluindo:

[🔝](#-sumário)
### 📄 Documentos Brasileiros 
- CPF com validação de dígitos verificadores
- CNPJ com validação de dígitos verificadores
- CNH (Carteira Nacional de Habilitação)
- RG (Registro Geral)
- Inscrição Estadual
- PIS/PASEP
- Título de Eleitor
- RENAVAM
- Placa de Veículo (padrão antigo e Mercosul)

[🔝](#-sumário)
### 👤 Dados Pessoais 
- Nomes e sobrenomes (masculinos/femininos)
- Nomes completos
- Telefones fixos com DDD
- Telefones celulares com DDD
- Endereços de e-mail

[🔝](#-sumário)
### 🏢 Dados Empresariais e Financeiros 
- Razão Social
- Nome Fantasia
- Números de documentos fiscais
- Contas bancárias
- Cartões de crédito (Visa, Mastercard, Amex, Discover)

[🔝](#-sumário)
### 🏠 Endereços Completos 
- Logradouros (Rua, Avenida, etc.)
- Número
- Complemento
- Bairro
- Cidade
- UF
- CEP formatado

[🔝](#-sumário)
### 💰 Valores e Datas 
- Datas aleatórias dentro de um intervalo
- Valores monetários
- Códigos de documentos comerciais

[🔝](#-sumário)
### 🏥 Dados de Saúde 
- Tipo sanguíneo (A+, A-, B+, B-, AB+, AB-, O+, O-)
- Altura (em centímetros)
- Peso (em quilogramas)
- Pressão arterial
- Medicamentos
- Especialidades médicas
- Planos de saúde

[🔝](#-sumário)
### 🎓 Dados Acadêmicos 
- Nome de instituições de ensino
- Nomes de cursos de graduação
- Áreas de formação acadêmica
- Números de matrículas acadêmicas
- Coeficientes de rendimento
- Datas de formatura
- Títulos de monografias ou trabalhos acadêmicos

[🔝](#-sumário)
### 🚗 Dados de Veículos
- Marcas de veículos populares no Brasil
- Modelos específicos para cada marca
- Anos de fabricação com distribuição realista
- Números de chassi válidos
- Cores de veículos
- Tipos de combustível (Flex, Gasolina, Diesel, etc.)
- Quilometragem proporcional à idade do veículo

[🔝](#-sumário)
## 💻 Instalação 

1. Faça o download dos arquivos fonte ou clone este repositório
2. Adicione o arquivo `FakeDataGenerator.pas` ao seu projeto Delphi/Lazarus
3. Adicione `FakeDataGenerator` na cláusula `uses` de suas units

[🔝](#-sumário)
## 🚀 Como Usar 

## - Exemplo básico de uso:

```pascal
uses
  ..., FakeDataGenerator;

var
  FakeData: TFakeDataGenerator;
begin
  // Inicie a geração de números aleatórios
  Randomize;
  
  // Crie uma instância do gerador
  FakeData := TFakeDataGenerator.Create;
  try
    // Gere e utilize os dados fictícios
    ShowMessage('CPF: ' + FakeData.GerarCPF);
    ShowMessage('Nome: ' + FakeData.GerarNomeCompleto);
    ShowMessage('Email: ' + FakeData.GerarEmail(FakeData.GerarNomeCompleto));
  finally
    // Libere os recursos
    FakeData.Free;
  end;
end;
```
[🔝](#-sumário)
## 📝 Exemplos 

[🔝](#-sumário)
## - Populando uma tabela de empresas 

```pascal

procedure PopularEmpresas(Quantidade: Integer);
var
  FakeData: TFakeDataGenerator;
  i: Integer;
  RazaoSocial: string;
begin
  FakeData := TFakeDataGenerator.Create;
  try
    for i := 1 to Quantidade do
    begin
      RazaoSocial := FakeData.GerarRazaoSocial;
      
      qryEmpresas.Close;
      qryEmpresas.SQL.Text := 'INSERT INTO EMPRESAS (RAZAO_SOCIAL, NOME_FANTASIA, CNPJ, ' +
                              'TELEFONE, EMAIL, ENDERECO, CIDADE, UF, CEP) ' +
                              'VALUES (:RAZAO, :FANTASIA, :CNPJ, :TEL, :EMAIL, ' + 
                              ':END, :CID, :UF, :CEP)';
      
      qryEmpresas.ParamByName('RAZAO').AsString := RazaoSocial;
      qryEmpresas.ParamByName('FANTASIA').AsString := FakeData.GerarNomeFantasia(RazaoSocial);
      qryEmpresas.ParamByName('CNPJ').AsString := FakeData.GerarCNPJ;
      qryEmpresas.ParamByName('TEL').AsString := FakeData.GerarTelefone;
      qryEmpresas.ParamByName('EMAIL').AsString := FakeData.GerarEmail(RazaoSocial);
      qryEmpresas.ParamByName('END').AsString := 
        FakeData.GerarLogradouro + ', ' + FakeData.GerarNumero;
      qryEmpresas.ParamByName('CID').AsString := FakeData.GerarCidade;
      qryEmpresas.ParamByName('UF').AsString := FakeData.GerarUF;
      qryEmpresas.ParamByName('CEP').AsString := FakeData.GerarCEP;
      
      qryEmpresas.ExecSQL;
    end;
  finally
    FakeData.Free;
  end;
end;

```

[🔝](#-sumário)
## Gerando contas a receber 

```pascal
procedure GerarContasReceber(IDEmpresa, IDPessoa: Integer; Quantidade: Integer);
var
  FakeData: TFakeDataGenerator;
  i: Integer;
  DataVencimento: TDateTime;
begin
  FakeData := TFakeDataGenerator.Create;
  try
    for i := 1 to Quantidade do
    begin
      // Gera uma data de vencimento nos próximos 60 dias
      DataVencimento := FakeData.GerarData(Date, Date + 60);
      
      qryContasReceber.Close;
      qryContasReceber.SQL.Text := 
        'INSERT INTO CONTAS_RECEBER (ID_EMPRESA, ID_PESSOA, DOCUMENTO, ' +
        'EMISSAO, VENCIMENTO, VALOR, DESCRICAO, SITUACAO) ' +
        'VALUES (:EMPRESA, :PESSOA, :DOC, :EMISSAO, :VENC, :VALOR, :DESC, :SIT)';
      
      qryContasReceber.ParamByName('EMPRESA').AsInteger := IDEmpresa;
      qryContasReceber.ParamByName('PESSOA').AsInteger := IDPessoa;
      qryContasReceber.ParamByName('DOC').AsString := FakeData.GerarDocumento;
      qryContasReceber.ParamByName('EMISSAO').AsDate := Date;
      qryContasReceber.ParamByName('VENC').AsDate := DataVencimento;
      qryContasReceber.ParamByName('VALOR').AsFloat := FakeData.GerarValor(100, 5000);
      qryContasReceber.ParamByName('DESC').AsString := 'Venda de mercadorias';
      qryContasReceber.ParamByName('SIT').AsString := 'P'; // Pendente
      
      qryContasReceber.ExecSQL;
    end;
  finally
    FakeData.Free;
  end;
end;

```

[🔝](#-sumário)
## - Populando dados de prontuário médico 

```pascal
procedure PopularProntuarios(Quantidade: Integer);
var
  FakeData: TFakeDataGenerator;
  i: Integer;
  IDPessoa: Integer;
begin
  FakeData := TFakeDataGenerator.Create;
  try
    for i := 1 to Quantidade do
    begin
      // Obter pessoa aleatória do banco de dados
      qryPessoas.Close;
      qryPessoas.Open;
      qryPessoas.First;
      qryPessoas.MoveBy(Random(qryPessoas.RecordCount));
      IDPessoa := qryPessoas.FieldByName('ID').AsInteger;
      
      qryProntuarios.Close;
      qryProntuarios.SQL.Text := 
        'INSERT INTO PRONTUARIOS (ID_PESSOA, TIPO_SANGUINEO, ALTURA, ' +
        'PESO, PRESSAO_ARTERIAL, ULTIMA_ATUALIZACAO) ' +
        'VALUES (:PESSOA, :TIPO_SANG, :ALTURA, :PESO, :PRESSAO, :DATA)';
      
      qryProntuarios.ParamByName('PESSOA').AsInteger := IDPessoa;
      qryProntuarios.ParamByName('TIPO_SANG').AsString := FakeData.GerarTipoSanguineo;
      qryProntuarios.ParamByName('ALTURA').AsInteger := FakeData.GerarAltura;
      qryProntuarios.ParamByName('PESO').AsFloat := FakeData.GerarPeso;
      qryProntuarios.ParamByName('PRESSAO').AsString := FakeData.GerarPressaoArterial;
      qryProntuarios.ParamByName('DATA').AsDateTime := Now;
      
      qryProntuarios.ExecSQL;
    end;
  finally
    FakeData.Free;
  end;
end;
```

[🔝](#-sumário)
## - Populando dados de veículos

```pascal
procedure PopularVeiculos(Quantidade: Integer);
var
  FakeData: TFakeDataGenerator;
  i: Integer;
  IDPessoa: Integer;
  Marca: string;
  Modelo: string;
  AnoFabricacao: Integer;
begin
  FakeData := TFakeDataGenerator.Create;
  try
    for i := 1 to Quantidade do
    begin
      // Obter pessoa aleatória do banco de dados
      qryPessoas.Close;
      qryPessoas.Open;
      qryPessoas.First;
      qryPessoas.MoveBy(Random(qryPessoas.RecordCount));
      IDPessoa := qryPessoas.FieldByName('ID').AsInteger;
      
      // Gerar dados do veículo
      Marca := FakeData.GerarMarcaVeiculo;
      Modelo := FakeData.GerarModeloVeiculo(Marca);
      AnoFabricacao := FakeData.GerarAnoVeiculo;
      
      qryVeiculos.Close;
      qryVeiculos.SQL.Text := 
        'INSERT INTO VEICULOS (ID_PROPRIETARIO, MARCA, MODELO, ANO_FABRICACAO, ' +
        'ANO_MODELO, CHASSI, COR, COMBUSTIVEL, PLACA, RENAVAM, QUILOMETRAGEM) ' +
        'VALUES (:PROP, :MARCA, :MODELO, :ANOFAB, :ANOMOD, :CHASSI, :COR, ' +
        ':COMB, :PLACA, :RENAVAM, :KM)';
      
      qryVeiculos.ParamByName('PROP').AsInteger := IDPessoa;
      qryVeiculos.ParamByName('MARCA').AsString := Marca;
      qryVeiculos.ParamByName('MODELO').AsString := Modelo;
      qryVeiculos.ParamByName('ANOFAB').AsInteger := AnoFabricacao;
      qryVeiculos.ParamByName('ANOMOD').AsInteger := AnoFabricacao + Random(2); // Ano modelo pode ser igual ou até 1 ano mais recente
      qryVeiculos.ParamByName('CHASSI').AsString := FakeData.GerarChassi;
      qryVeiculos.ParamByName('COR').AsString := FakeData.GerarCor;
      qryVeiculos.ParamByName('COMB').AsString := FakeData.GerarTipoCombustivel;
      qryVeiculos.ParamByName('PLACA').AsString := FakeData.GerarPlacaVeiculo(Random(2) = 1); // 50% chance de usar placa Mercosul
      qryVeiculos.ParamByName('RENAVAM').AsString := FakeData.GerarRENAVAM;
      qryVeiculos.ParamByName('KM').AsInteger := FakeData.GerarQuilometragem(AnoFabricacao);
      
      qryVeiculos.ExecSQL;
    end;
  finally
    FakeData.Free;
  end;
end;
```

[🔝](#-sumário)
## 📖 Documentação da API 

### TFakeDataGenerator

[🔝](#-sumário)
#### Métodos para Documentos 

```pascal
// Gera um CPF válido. Se Formatado = True, retorna no formato XXX.XXX.XXX-XX.
function GerarCPF(Formatado: Boolean = True): string;

// Gera um CNPJ válido. Se Formatado = True, retorna no formato XX.XXX.XXX/XXXX-XX.
function GerarCNPJ(Formatado: Boolean = True): string;

// Gera um número de CNH fictício.
function GerarCNH: string;

// Gera uma inscrição estadual para a UF informada.
function GerarInscricaoEstadual(UF: string): string;

// Gera um número PIS/PASEP válido. Se Formatado = True, retorna no formato XXX.XXXXX.XX-X.
function GerarPIS(Formatado: Boolean = True): string;

// Gera um número de RG válido. Se Formatado = True, retorna no formato XX.XXX.XXX-X.
function GerarRG(Formatado: Boolean = True): string;

// Gera um número RENAVAM válido.
function GerarRENAVAM: string;

// Gera um número de título de eleitor. Se Formatado = True, inclui espaços entre os grupos.
function GerarTituloEleitor(Formatado: Boolean = True): string;

// Gera uma placa de veículo. Se Mercosul = True, usa o padrão Mercosul (ABC1D23).
function GerarPlacaVeiculo(Mercosul: Boolean = False): string;
```

[🔝](#-sumário)
#### Métodos para Dados Pessoais 

```pascal
// Gera um nome (primeiro nome) aleatório.
function GerarNome(Feminino: Boolean = False): string;

// Gera um nome completo (nome + sobrenomes) aleatório.
function GerarNomeCompleto(Feminino: Boolean = False): string;

// Gera um número de telefone fixo com DDD.
function GerarTelefone(Formatado: Boolean = True): string;

// Gera um número de celular com DDD, iniciando com 9.
function GerarCelular(Formatado: Boolean = True): string;

// Gera um endereço de e-mail baseado no nome fornecido.
function GerarEmail(const Nome: string): string;

// Gera uma data de nascimento com idade entre IdadeMinima e IdadeMaxima.
function GerarDataNascimento(IdadeMinima: Integer = 18; IdadeMaxima: Integer = 80): TDateTime;

// Gera um perfil de rede social com base no nome.
function GerarPerfilRedeSocial(const Nome: string; RedeSocial: string = 'instagram'): string;

// Gera um estado civil aleatório.
function GerarEstadoCivil: string;

// Gera uma profissão aleatória.
function GerarProfissao: string;

// Gera um nível de escolaridade aleatório.
function GerarEscolaridade: string;
```

[🔝](#-sumário)
#### Métodos para Dados Empresariais e Financeiros 

```pascal
// Gera uma razão social para empresa.
function GerarRazaoSocial: string;

// Gera um nome fantasia baseado na razão social.
function GerarNomeFantasia(const RazaoSocial: string): string;

// Gera um número de documento fiscal ou comercial.
function GerarDocumento: string;

// Gera um número de cartão de crédito válido. Bandeira pode ser: V (Visa), M (Mastercard), A (Amex), D (Discover).
function GerarCartaoCredito(Bandeira: string = ''): string;

// Gera dados de conta bancária. Banco pode ser especificado ou deixado em branco para seleção aleatória.
function GerarContaBancaria(Banco: string = ''; Formatado: Boolean = True): string;

// Gera um código CNAE para atividade empresarial.
function GerarCNAE(Formatado: Boolean = True): string;

// Gera um número de inscrição municipal.
function GerarInscricaoMunicipal(Municipio: string = ''): string;

// Gera um nome de departamento empresarial.
function GerarDepartamento: string;

// Gera um cargo profissional.
function GerarCargo: string;

// Gera uma chave PIX de acordo com o tipo especificado.
function GerarChavePIX(TipoChave: string = 'aleatoria'): string;
```

[🔝](#-sumário)
#### Métodos para Endereços 

```pascal
// Gera um logradouro aleatório (tipo + nome da rua).
function GerarLogradouro: string;

// Gera um número para endereço.
function GerarNumero: string;

// Gera um complemento de endereço (pode retornar vazio).
function GerarComplemento: string;

// Gera um nome de bairro.
function GerarBairro: string;

// Gera um nome de cidade.
function GerarCidade: string;

// Gera uma sigla de estado (UF).
function GerarUF: string;

// Gera um CEP. Se Formatado = True, retorna no formato XXXXX-XXX.
function GerarCEP(Formatado: Boolean = True): string;
```

[🔝](#-sumário)
#### Métodos para Valores e Datas 

```pascal
// Gera uma data aleatória entre DataInicial e DataFinal.
function GerarData(DataInicial, DataFinal: TDateTime): TDateTime;

// Gera um valor aleatório entre ValorMinimo e ValorMaximo.
function GerarValor(ValorMinimo, ValorMaximo: Double): Double;

// Gera um código de barras para boleto.
function GerarCodigoBarrasBoleto(Banco: string = ''): string;

// Gera uma linha digitável para boleto.
function GerarLinhaDigitavelBoleto(Banco: string = ''): string;

// Gera um código de rastreio para encomendas.
function GerarCodigoRastreio: string;

// Gera uma alíquota de imposto do tipo especificado.
function GerarAliquotaImposto(TipoImposto: string = 'ICMS'): Double;

// Gera um número de nota fiscal.
function GerarNotaFiscal(UF: string = ''): string;
```

[🔝](#-sumário)
#### Métodos para Dados de Saúde 

```pascal
// Gera um tipo sanguíneo (A+, A-, B+, B-, AB+, AB-, O+, O-).
function GerarTipoSanguineo: string;

// Gera uma altura em centímetros entre Min e Max.
function GerarAltura(Min: Integer = 150; Max: Integer = 200): Integer;

// Gera um peso em quilogramas entre Min e Max.
function GerarPeso(Min: Integer = 50; Max: Integer = 120): Double;

// Gera uma pressão arterial no formato "120/80 mmHg".
function GerarPressaoArterial: string;

// Gera um nome de medicamento comum.
function GerarMedicamento: string;

// Gera uma especialidade médica.
function GerarEspecialidadeMedica: string;

// Gera um nome de plano de saúde.
function GerarPlanoSaude: string;
```

[🔝](#-sumário)
#### Métodos para Dados Acadêmicos 

```pascal
// Gera um nome de instituição de ensino aleatório.
function GerarNomeInstituicaoEnsino: string;

// Gera um nome de curso de graduação aleatório.
function GerarCursoGraduacao: string;

// Gera uma área de formação acadêmica aleatória.
function GerarAreaFormacao: string;

// Gera um número de matrícula acadêmica no formato AAANNNNND (AA=ano, NNNNN=sequencial, D=dígito verificador).
function GerarMatriculaAcademica: string;

// Gera um coeficiente de rendimento (CR) entre 0 e 10 com distribuição mais realista.
function GerarCoeficienteRendimento: Double; // 0 a 10

// Gera uma data de formatura com base no ano de início do curso.
function GerarDataFormatura(AnoInicio: Integer = 0): TDateTime;

// Gera um título de monografia ou trabalho acadêmico.
function GerarTituloMonografia: string;
```

[🔝](#-sumário)
#### Métodos para Dados de Veículos

```pascal
// Gera uma marca de veículo popular no Brasil.
function GerarMarcaVeiculo: string;

// Gera um modelo compatível com a marca especificada. Se a marca for vazia, escolhe aleatoriamente.
function GerarModeloVeiculo(const Marca: string = ''): string;

// Gera um ano de fabricação com distribuição realista (mais veículos novos). Limita a idade máxima.
function GerarAnoVeiculo(IdadeMaxima: Integer = 20): Integer;

// Gera um número de chassi no formato padrão de 17 caracteres.
function GerarChassi: string;

// Gera uma cor de veículo.
function GerarCor: string;

// Gera um tipo de combustível com distribuição baseada na frota brasileira.
function GerarTipoCombustivel: string;

// Gera uma quilometragem plausível baseada na idade do veículo.
function GerarQuilometragem(AnoVeiculo: Integer): Integer;
```

[🔝](#-sumário)
#### Métodos Utilitários

```pascal
// Remove todos os caracteres não-numéricos de uma string.
function ApenasNumeros(const Str: string): string;

// Calcula dígito verificador usando o algoritmo de módulo 11.
function GerarDigitosModulo11(const Digits: string; Peso: Integer): string;

// Calcula os dígitos verificadores do CPF.
function GerarDigitosCPF(const Digits: string): string;

// Calcula os dígitos verificadores do CNPJ.
function GerarDigitosCNPJ(const Digits: string): string;
```

[🔝](#-sumário)
## 🤝 Contribuindo
Contribuições são bem-vindas! Sinta-se à vontade para abrir issues ou enviar pull requests com melhorias, correções ou novas funcionalidades.

Faça um fork deste repositório
Crie uma branch para sua feature (git checkout -b feature/nova-funcionalidade)
Faça commit das suas alterações (git commit -m 'Adiciona nova funcionalidade')
Faça push para a branch (git push origin feature/nova-funcionalidade)
Abra um Pull Request

[🔝](#-sumário)
## 📄 Licença
Este projeto está licenciado sob a Licença MIT - veja o arquivo LICENSE para detalhes.
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


Desenvolvido para facilitar a criação de ambientes de teste e demonstração em sistemas de gestão empresarial.

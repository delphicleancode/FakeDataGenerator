unit FakeDataGenerator;

interface

uses
  SysUtils, Classes, Math, DateUtils, StrUtils;

type
  TFakeDataGenerator = class
  private
    FNomesMasculinos: TStringList;
    FSobrenomes     : TStringList;
    FNomesFemininos : TStringList;
    FLogradouros    : TStringList;
    FCidades        : TStringList;
    FBairros        : TStringList;
    FUFs            : TStringList;
    FMedicamentos   : TStringList;  
    FEspecialidades : TStringList;  
    FPlanosSaude    : TStringList;    
    FInstituicoesEnsino: TStringList; 
    FCursosGraduacao: TStringList;  
    FAreasFormacao  : TStringList;
    function GerarDigitosCPF(const Digits: string): string;
    function GerarDigitosCNPJ(const Digits: string): string;
    function GerarDigitosModulo11(const Digits: string; Peso: Integer): string;
    function ApenasNumeros(const Str: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    { Documentos Brasileiros }
    function GerarCPF(Formatado: Boolean = True): string;
    function GerarCNPJ(Formatado: Boolean = True): string;
    function GerarCNH: string;
    function GerarRG(Formatado: Boolean = True): string;
    function GerarInscricaoEstadual(UF: string): string;
    function GerarPIS(Formatado: Boolean = True): string;
    function GerarRENAVAM: string;
    function GerarTituloEleitor(Formatado: Boolean = True): string;
    function GerarPlacaVeiculo(Mercosul: Boolean = False): string;
    
    { Dados Pessoais }
    function GerarNome(Feminino: Boolean = False): string;
    function GerarNomeCompleto(Feminino: Boolean = False): string;
    function GerarTelefone(Formatado: Boolean = True): string;
    function GerarCelular(Formatado: Boolean = True): string;
    function GerarEmail(const Nome: string): string;
    function GerarDataNascimento(IdadeMinima: Integer = 18; IdadeMaxima: Integer = 80): TDateTime;
    function GerarPerfilRedeSocial(const Nome: string; RedeSocial: string = 'instagram'): string;
    function GerarEstadoCivil: string;
    function GerarProfissao: string;
    function GerarEscolaridade: string;
    
    { Dados Empresariais }
    function GerarRazaoSocial: string;
    function GerarNomeFantasia(const RazaoSocial: string): string;
    function GerarDocumento: string;
    function GerarCartaoCredito(Bandeira: string = ''): string;
    function GerarContaBancaria(Banco: string = ''; Formatado: Boolean = True): string;
    function GerarCNAE(Formatado: Boolean = True): string;
    function GerarInscricaoMunicipal(Municipio: string = ''): string;
    function GerarDepartamento: string;
    function GerarCargo: string;
    function GerarChavePIX(TipoChave: string = 'aleatoria'): string;

    { Dados de Saúde }
    function GerarTipoSanguineo: string;
    function GerarAltura(Min: Integer = 150; Max: Integer = 200): Integer; // em cm
    function GerarPeso(Min: Integer = 50; Max: Integer = 120): Double; // em kg
    function GerarPressaoArterial: string;
    function GerarMedicamento: string;
    function GerarEspecialidadeMedica: string;
    function GerarPlanoSaude: string;  

    { Dados Acadêmicos }
    function GerarNomeInstituicaoEnsino: string;
    function GerarCursoGraduacao: string;
    function GerarAreaFormacao: string;
    function GerarMatriculaAcademica: string;
    function GerarCoeficienteRendimento: Double; // 0 a 10
    function GerarDataFormatura(AnoInicio: Integer = 0): TDateTime;
    function GerarTituloMonografia: string;      
    
    { Dados de Endereço }
    function GerarLogradouro: string;
    function GerarNumero: string;
    function GerarComplemento: string;
    function GerarBairro: string;
    function GerarCidade: string;
    function GerarUF: string;
    function GerarCEP(Formatado: Boolean = True): string;
    
    { Dados Financeiros }
    function GerarData(DataInicial, DataFinal: TDateTime): TDateTime;
    function GerarValor(ValorMinimo, ValorMaximo: Double): Double;
    function GerarCodigoBarrasBoleto(Banco: string = ''): string;
    function GerarLinhaDigitavelBoleto(Banco: string = ''): string;
    function GerarCodigoRastreio: string;
    function GerarAliquotaImposto(TipoImposto: string = 'ICMS'): Double;
    function GerarNotaFiscal(UF: string = ''): string;
    
    { Outros Dados Específicos }
    function GerarProtocolo: string;
    function GerarSKU(Categoria: string = ''): string;
    function GerarEAN13: string;
    function GerarCID: string;
    function GerarProcessoJudicial: string;
  end;

implementation


constructor TFakeDataGenerator.Create;
begin
  inherited;
  
  FNomesMasculinos := TStringList.Create;
  FSobrenomes      := TStringList.Create;
  FNomesFemininos  := TStringList.Create;
  FLogradouros     := TStringList.Create;
  FCidades         := TStringList.Create;
  FBairros         := TStringList.Create;
  FUFs             := TStringList.Create;
  FMedicamentos    := TStringList.Create;
  FEspecialidades  := TStringList.Create;
  FPlanosSaude     := TStringList.Create;
  
  FInstituicoesEnsino := TStringList.Create;
  FCursosGraduacao    := TStringList.Create;
  FAreasFormacao      := TStringList.Create;

  // Inicializar listas com dados
  with FNomesMasculinos do
  begin
    Add('João'); Add('José'); Add('Antônio'); Add('Francisco'); Add('Carlos');
    Add('Paulo'); Add('Pedro'); Add('Lucas'); Add('Luiz'); Add('Marcos');
    Add('Gabriel'); Add('Rafael'); Add('Daniel'); Add('Marcelo'); Add('Bruno');
    Add('Eduardo'); Add('Felipe'); Add('Raimundo'); Add('Rodrigo'); Add('Sebastião');
  end;

  with FNomesFemininos do
  begin
    Add('Maria'); Add('Ana'); Add('Francisca'); Add('Juliana'); Add('Márcia');
    Add('Fernanda'); Add('Adriana'); Add('Patricia'); Add('Aline'); Add('Sandra');
    Add('Camila'); Add('Amanda'); Add('Bruna'); Add('Jéssica'); Add('Letícia');
    Add('Júlia'); Add('Luciana'); Add('Vanessa'); Add('Mariana'); Add('Gabriela');
  end;

  with FSobrenomes do
  begin
    Add('Silva'); Add('Santos'); Add('Oliveira'); Add('Souza'); Add('Lima');
    Add('Pereira'); Add('Ferreira'); Add('Costa'); Add('Rodrigues'); Add('Almeida');
    Add('Nascimento'); Add('Carvalho'); Add('Araújo'); Add('Ribeiro'); Add('Gomes');
    Add('Martins'); Add('Correia'); Add('Cavalcanti'); Add('Dias'); Add('Campos');
  end;

  with FLogradouros do
  begin
    Add('Rua'); Add('Avenida'); Add('Travessa'); Add('Alameda'); Add('Praça');
  end;

  with FBairros do
  begin
    Add('Centro'); Add('Jardim'); Add('Vila'); Add('Parque'); Add('Bela Vista');
    Add('Santo Antônio'); Add('São José'); Add('Industrial'); Add('Nova Esperança');
  end;

  with FCidades do
  begin
    Add('São Paulo'); Add('Rio de Janeiro'); Add('Belo Horizonte'); Add('Salvador');
    Add('Fortaleza'); Add('Brasília'); Add('Curitiba'); Add('Manaus'); Add('Recife');
    Add('Porto Alegre'); Add('Belém'); Add('Goiânia'); Add('São Luís'); Add('Maceió');
  end;

  with FUFs do
  begin
    Add('SP'); Add('RJ'); Add('MG'); Add('BA'); Add('CE'); Add('DF'); Add('PR');
    Add('AM'); Add('PE'); Add('RS'); Add('PA'); Add('GO'); Add('MA'); Add('AL');
    Add('SC'); Add('MT'); Add('MS'); Add('PB'); Add('PI'); Add('RN');
  end;
  
  // Preencher lista de medicamentos comuns
  with FMedicamentos do
  begin
    Add('Losartana'); Add('Dipirona'); Add('Amoxicilina'); Add('Omeprazol');
    Add('Paracetamol'); Add('Ibuprofeno'); Add('Atenolol'); Add('Metformina');
    Add('Fluoxetina'); Add('Enalapril'); Add('Anlodipino'); Add('Sinvastatina');
    Add('Levotiroxina'); Add('Azitromicina'); Add('Captopril'); Add('Clonazepam');
    Add('Nimesulida'); Add('Dexametasona'); Add('Cefalexina'); Add('Ranitidina');
  end;
  
  // Preencher lista de especialidades médicas
  with FEspecialidades do
  begin
    Add('Cardiologia'); Add('Dermatologia'); Add('Endocrinologia'); Add('Neurologia');
    Add('Ortopedia'); Add('Pediatria'); Add('Ginecologia'); Add('Psiquiatria');
    Add('Urologia'); Add('Oftalmologia'); Add('Otorrinolaringologia'); Add('Geriatria');
    Add('Oncologia'); Add('Anestesiologia'); Add('Clínica Médica'); Add('Cirurgia Geral');
    Add('Gastroenterologia'); Add('Infectologia'); Add('Nefrologia'); Add('Reumatologia');
  end;
  
  // Preencher lista de planos de saúde
  with FPlanosSaude do
  begin
    Add('Unimed'); Add('Amil'); Add('SulAmérica'); Add('Bradesco Saúde');
    Add('Notre Dame Intermédica'); Add('Hapvida'); Add('Golden Cross'); Add('Porto Seguro Saúde');
    Add('São Francisco Saúde'); Add('Prevent Senior'); Add('Mediservice'); Add('Care Plus');
    Add('CASSI'); Add('GEAP Saúde'); Add('Allianz Saúde'); Add('OneHealth');
  end;
  
  // Preencher lista de instituições de ensino
  with FInstituicoesEnsino do
  begin
    Add('Universidade de São Paulo');
    Add('Universidade Estadual de Campinas');
    Add('Universidade Federal do Rio de Janeiro');
    Add('Universidade Federal de Minas Gerais');
    Add('Pontifícia Universidade Católica de São Paulo');
    Add('Universidade de Brasília');
    Add('Universidade Federal do Rio Grande do Sul');
    Add('Universidade Estadual Paulista');
    Add('Universidade Federal da Bahia');
    Add('Universidade Federal de Pernambuco');
    Add('Universidade Federal do Paraná');
    Add('Universidade Federal de Santa Catarina');
    Add('Universidade Federal do Ceará');
    Add('Universidade Federal Fluminense');
    Add('Universidade Federal de São Carlos');
    Add('Universidade Federal de Goiás');
    Add('Universidade do Estado do Rio de Janeiro');
    Add('Universidade Federal do Pará');
    Add('Universidade Federal da Paraíba');
    Add('Universidade Federal de Santa Maria');
    Add('Centro Universitário FEI');
    Add('Faculdade de Tecnologia de São Paulo');
    Add('Instituto Federal de Educação, Ciência e Tecnologia');
    Add('Faculdades Integradas');
    Add('Fundação Getúlio Vargas');
  end;
  
  // Preencher lista de cursos de graduação
  with FCursosGraduacao do
  begin
    Add('Administração');
    Add('Direito');
    Add('Medicina');
    Add('Engenharia Civil');
    Add('Psicologia');
    Add('Ciência da Computação');
    Add('Odontologia');
    Add('Enfermagem');
    Add('Arquitetura e Urbanismo');
    Add('Engenharia Elétrica');
    Add('Farmácia');
    Add('Contabilidade');
    Add('Pedagogia');
    Add('Nutrição');
    Add('Fisioterapia');
    Add('Publicidade e Propaganda');
    Add('Sistemas de Informação');
    Add('Medicina Veterinária');
    Add('Engenharia de Produção');
    Add('Jornalismo');
    Add('Análise e Desenvolvimento de Sistemas');
    Add('Educação Física');
    Add('Engenharia Mecânica');
    Add('Ciências Econômicas');
    Add('Biomedicina');
    Add('Design');
    Add('Agronomia');
    Add('Relações Internacionais');
    Add('Química');
    Add('Física');
  end;
  
  // Preencher lista de áreas de formação
  with FAreasFormacao do
  begin
    Add('Ciências Exatas e da Terra');
    Add('Ciências Biológicas');
    Add('Engenharias');
    Add('Ciências da Saúde');
    Add('Ciências Agrárias');
    Add('Ciências Sociais Aplicadas');
    Add('Ciências Humanas');
    Add('Linguística, Letras e Artes');
    Add('Multidisciplinar');
  end;
end;

destructor TFakeDataGenerator.Destroy;
begin
  FNomesMasculinos.Free;
  FSobrenomes.Free;
  FNomesFemininos.Free;
  FLogradouros.Free;
  FCidades.Free;
  FBairros.Free;
  FUFs.Free;
  FMedicamentos.Free;
  FEspecialidades.Free;
  FPlanosSaude.Free;
  FInstituicoesEnsino.Free;
  FCursosGraduacao.Free;
  FAreasFormacao.Free;
  inherited;
end;

function TFakeDataGenerator.ApenasNumeros(const Str: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Str) do
    if CharInSet(Str[i], ['0'..'9']) then
      Result := Result + Str[i];
end;

function TFakeDataGenerator.GerarDigitosModulo11(const Digits: string; Peso: Integer): string;
var
  i, Sum, Remainder, Digit: Integer;
  Weight: Integer;
begin
  Sum := 0;
  Weight := Peso;
  
  for i := Length(Digits) downto 1 do
  begin
    Sum := Sum + (StrToInt(Digits[i]) * Weight);
    Dec(Weight);
    if Weight < 2 then
      Weight := Peso;
  end;
  
  Remainder := Sum mod 11;
  
  if Remainder < 2 then
    Digit := 0
  else
    Digit := 11 - Remainder;
  
  Result := IntToStr(Digit);
end;

function TFakeDataGenerator.GerarDigitosCPF(const Digits: string): string;
var
  Digit1, Digit2: string;
begin
  Digit1 := GerarDigitosModulo11(Digits, 10);
  Digit2 := GerarDigitosModulo11(Digits + Digit1, 11);
  Result := Digit1 + Digit2;
end;

function TFakeDataGenerator.GerarDigitosCNPJ(const Digits: string): string;
var
  Digit1, Digit2: string;
  i, Sum, Weight, Remainder, Digit: Integer;
begin
  // Primeiro dígito
  Sum := 0;
  Weight := 5;
  for i := 1 to 4 do
  begin
    Sum := Sum + StrToInt(Digits[i]) * Weight;
    Dec(Weight);
  end;
  
  Weight := 9;
  for i := 5 to 12 do
  begin
    Sum := Sum + StrToInt(Digits[i]) * Weight;
    Dec(Weight);
  end;
  
  Remainder := Sum mod 11;
  if Remainder < 2 then
    Digit1 := '0'
  else
    Digit1 := IntToStr(11 - Remainder);
  
  // Segundo dígito
  Sum := 0;
  Weight := 6;
  for i := 1 to 5 do
  begin
    Sum := Sum + StrToInt(Digits[i]) * Weight;
    Dec(Weight);
  end;
  
  Weight := 9;
  for i := 6 to 12 do
  begin
    Sum := Sum + StrToInt(Digits[i]) * Weight;
    Dec(Weight);
  end;
  
  Sum := Sum + StrToInt(Digit1) * 2;
  
  Remainder := Sum mod 11;
  if Remainder < 2 then
    Digit2 := '0'
  else
    Digit2 := IntToStr(11 - Remainder);
  
  Result := Digit1 + Digit2;
end;

function TFakeDataGenerator.GerarCPF(Formatado: Boolean = True): string;
var
  i: Integer;
  CPFNumeros: string;
  Digitos: string;
begin
  CPFNumeros := '';
  for i := 1 to 9 do
    CPFNumeros := CPFNumeros + IntToStr(Random(10));
    
  Digitos := GerarDigitosCPF(CPFNumeros);
  
  if Formatado then
    Result := Copy(CPFNumeros, 1, 3) + '.' + 
              Copy(CPFNumeros, 4, 3) + '.' + 
              Copy(CPFNumeros, 7, 3) + '-' + 
              Digitos
  else
    Result := CPFNumeros + Digitos;
end;

function TFakeDataGenerator.GerarCNPJ(Formatado: Boolean = True): string;
var
  i: Integer;
  BaseNumeros: string;
  Digitos: string;
begin
  BaseNumeros := '';
  for i := 1 to 8 do
    BaseNumeros := BaseNumeros + IntToStr(Random(10));
    
  BaseNumeros := BaseNumeros + '0001'; // Matriz
  
  Digitos := GerarDigitosCNPJ(BaseNumeros);
  
  if Formatado then
    Result := Copy(BaseNumeros, 1, 2) + '.' + 
              Copy(BaseNumeros, 3, 3) + '.' + 
              Copy(BaseNumeros, 6, 3) + '/' + 
              Copy(BaseNumeros, 9, 4) + '-' + 
              Digitos
  else
    Result := BaseNumeros + Digitos;
end;

function TFakeDataGenerator.GerarTelefone(Formatado: Boolean = True): string;
var
  DDD, Numero: string;
begin
  DDD := IntToStr(10 + Random(90)); // DDDs válidos entre 10 e 99
  Numero := '';
  
  // Gera número fixo (começa com 2, 3, 4 ou 5)
  Numero := IntToStr(2 + Random(4));
  
  // Complementa com mais 7 números
  while Length(Numero) < 8 do
    Numero := Numero + IntToStr(Random(10));
  
  if Formatado then
    Result := '(' + DDD + ') ' + Copy(Numero, 1, 4) + '-' + Copy(Numero, 5, 4)
  else
    Result := DDD + Numero;
end;

function TFakeDataGenerator.GerarCelular(Formatado: Boolean = True): string;
var
  DDD, Numero: string;
begin
  DDD := IntToStr(10 + Random(90)); // DDDs válidos entre 10 e 99
  
  // Celulares começam com 9 seguido de 8 dígitos
  Numero := '9';
  
  // Complementa com mais 8 números
  while Length(Numero) < 9 do
    Numero := Numero + IntToStr(Random(10));
  
  if Formatado then
    Result := '(' + DDD + ') ' + Copy(Numero, 1, 5) + '-' + Copy(Numero, 6, 4)
  else
    Result := DDD + Numero;
end;

function TFakeDataGenerator.GerarCNH: string;
var
  i: Integer;
  CNH: string;
begin
  CNH := '';
  for i := 1 to 11 do
    CNH := CNH + IntToStr(Random(10));
  
  Result := CNH;
end;

function TFakeDataGenerator.GerarNome(Feminino: Boolean = False): string;
begin
  if Feminino then
    Result := FNomesFemininos[Random(FNomesFemininos.Count)]
  else
    Result := FNomesMasculinos[Random(FNomesMasculinos.Count)];
end;

function TFakeDataGenerator.GerarNomeCompleto(Feminino: Boolean = False): string;
var
  Nome: string;
  NumSobrenomes: Integer;
  i: Integer;
begin
  if Feminino then
    Nome := FNomesFemininos[Random(FNomesFemininos.Count)]
  else
    Nome := FNomesMasculinos[Random(FNomesMasculinos.Count)];
  
  NumSobrenomes := 1 + Random(2); // 1 a 2 sobrenomes
  
  for i := 1 to NumSobrenomes do
    Nome := Nome + ' ' + FSobrenomes[Random(FSobrenomes.Count)];
  
  Result := Nome;
end;

function TFakeDataGenerator.GerarRazaoSocial: string;
var
  Prefixos: array[0..9] of string;
  Sufixos: array[0..9] of string;
begin
  Prefixos[0] := 'Tech';
  Prefixos[1] := 'Grupo';
  Prefixos[2] := 'Industria';
  Prefixos[3] := 'Comercial';
  Prefixos[4] := 'Centro';
  Prefixos[5] := 'Mega';
  Prefixos[6] := 'Super';
  Prefixos[7] := 'Multi';
  Prefixos[8] := 'Nova';
  Prefixos[9] := 'Global';
  
  Sufixos[0] := 'Sistemas';
  Sufixos[1] := 'Soluções';
  Sufixos[2] := 'Tecnologia';
  Sufixos[3] := 'Comércio';
  Sufixos[4] := 'Distribuição';
  Sufixos[5] := 'Serviços';
  Sufixos[6] := 'Produtos';
  Sufixos[7] := 'Indústria';
  Sufixos[8] := 'Logística';
  Sufixos[9] := 'Consultoria';
  
  Result := Prefixos[Random(10)] + ' ' + 
            FSobrenomes[Random(FSobrenomes.Count)] + ' ' + 
            Sufixos[Random(10)] + ' ' + 
            'LTDA';
end;

function TFakeDataGenerator.GerarNomeFantasia(const RazaoSocial: string): string;
begin
  Result := StringReplace(RazaoSocial, ' LTDA', '', [rfReplaceAll, rfIgnoreCase]);
end;

function TFakeDataGenerator.GerarLogradouro: string;
var
  NomesRuas: array[0..9] of string;
begin
  NomesRuas[0] := 'das Flores';
  NomesRuas[1] := 'São João';
  NomesRuas[2] := 'Brasil';
  NomesRuas[3] := 'dos Bandeirantes';
  NomesRuas[4] := 'Quinze de Novembro';
  NomesRuas[5] := 'Sete de Setembro';
  NomesRuas[6] := 'Dom Pedro I';
  NomesRuas[7] := 'Santos Dumont';
  NomesRuas[8] := 'Tiradentes';
  NomesRuas[9] := 'José de Alencar';
  
  Result := FLogradouros[Random(FLogradouros.Count)] + ' ' + NomesRuas[Random(10)];
end;

function TFakeDataGenerator.GerarNumero: string;
begin
  Result := IntToStr(1 + Random(9999));
end;

function TFakeDataGenerator.GerarComplemento: string;
var
  Complementos: array[0..5] of string;
begin
  Complementos[0] := 'Apto '  + IntToStr(Random(999));
  Complementos[1] := 'Sala '  + IntToStr(Random(999));
  Complementos[2] := 'Bloco ' + Chr(65 + Random(26));
  Complementos[3] := 'Casa '  + IntToStr(Random(99));
  Complementos[4] := 'Loja '  + IntToStr(Random(99));
  Complementos[5] := '';  // Vazio

  Result := Complementos[Random(6)];
end;

function TFakeDataGenerator.GerarBairro: string;
begin
  Result := FBairros[Random(FBairros.Count)] + ' ' + 
            FSobrenomes[Random(FSobrenomes.Count)];
end;

function TFakeDataGenerator.GerarCidade: string;
begin
  Result := FCidades[Random(FCidades.Count)];
end;

function TFakeDataGenerator.GerarUF: string;
begin
  Result := FUFs[Random(FUFs.Count)];
end;

function TFakeDataGenerator.GerarCEP(Formatado: Boolean = True): string;
var
  CEP: string;
  i: Integer;
begin
  CEP := '';
  for i := 1 to 8 do
    CEP := CEP + IntToStr(Random(10));
  
  if Formatado then
    Result := Copy(CEP, 1, 5) + '-' + Copy(CEP, 6, 3)
  else
    Result := CEP;
end;

function TFakeDataGenerator.GerarEmail(const Nome: string): string;
var
  Dominios: array[0..4] of string;
  NomeSemAcentos, Parte1, Parte2: string;
  i: Integer;
begin
  Dominios[0] := 'gmail.com';
  Dominios[1] := 'hotmail.com';
  Dominios[2] := 'outlook.com';
  Dominios[3] := 'yahoo.com.br';
  Dominios[4] := 'uol.com.br';
  
  // Remove espaços e acentos
  NomeSemAcentos := AnsiLowerCase(Nome);
  NomeSemAcentos := StringReplace(NomeSemAcentos, ' ', '.', [rfReplaceAll]);
  
  // Substituições de caracteres acentuados
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'á', 'a', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'à', 'a', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'â', 'a', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'ã', 'a', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'é', 'e', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'ê', 'e', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'í', 'i', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'ó', 'o', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'ô', 'o', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'õ', 'o', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'ú', 'u', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'ç', 'c', [rfReplaceAll]);
  
  // Adiciona número aleatório opcional
  if Random(2) = 1 then
    NomeSemAcentos := NomeSemAcentos + IntToStr(Random(1000));
  
  Result := NomeSemAcentos + '@' + Dominios[Random(5)];
end;

function TFakeDataGenerator.GerarData(DataInicial, DataFinal: TDateTime): TDateTime;
var
  Dias: Integer;
begin
  Dias := DaysBetween(DataInicial, DataFinal);
  Result := IncDay(DataInicial, Random(Dias + 1));
end;

function TFakeDataGenerator.GerarValor(ValorMinimo, ValorMaximo: Double): Double;
begin
  Result := ValorMinimo + Random * (ValorMaximo - ValorMinimo);
  Result := RoundTo(Result, -2); // Arredonda para 2 casas decimais
end;

function TFakeDataGenerator.GerarDocumento: string;
var
  Tipos: array[0..4] of string;
begin
  Tipos[0] := 'NF-';
  Tipos[1] := 'FAT-';
  Tipos[2] := 'BOL-';
  Tipos[3] := 'REC-';
  Tipos[4] := 'DUP-';
  
  Result := Tipos[Random(5)] + IntToStr(Random(100000));
end;

function TFakeDataGenerator.GerarInscricaoEstadual(UF: string): string;
var
  i: Integer;
  IE: string;
begin
  IE := '';
  // Formato simplificado para exemplificar - na prática cada UF tem sua regra
  for i := 1 to 9 do
    IE := IE + IntToStr(Random(10));
  
  Result := IE;
end;

function TFakeDataGenerator.GerarPIS(Formatado: Boolean = True): string;
var
  i: Integer;
  PIS: string;
  Soma, Resto, Digito: Integer;
  Peso: array[1..10] of Integer;
begin
  // Inicializa os pesos para cálculo do dígito verificador
  Peso[1] := 3;
  Peso[2] := 2;
  Peso[3] := 9;
  Peso[4] := 8;
  Peso[5] := 7;
  Peso[6] := 6;
  Peso[7] := 5;
  Peso[8] := 4;
  Peso[9] := 3;
  Peso[10] := 2;

  // Gera os 10 primeiros dígitos
  PIS := '';
  for i := 1 to 10 do
    PIS := PIS + IntToStr(Random(10));
    
  // Calcula o dígito verificador
  Soma := 0;
  for i := 1 to 10 do
    Soma := Soma + StrToInt(PIS[i]) * Peso[i];
    
  Resto := Soma mod 11;
  
  if Resto < 2 then
    Digito := 0
  else
    Digito := 11 - Resto;
    
  PIS := PIS + IntToStr(Digito);
  
  if Formatado then
    Result := Copy(PIS, 1, 3) + '.' + 
              Copy(PIS, 4, 5) + '.' + 
              Copy(PIS, 9, 2) + '-' + 
              Copy(PIS, 11, 1)
  else
    Result := PIS;
end;

function TFakeDataGenerator.GerarRENAVAM: string;
var
  i: Integer;
  RENAVAM: string;
  Soma, Resto, Digito: Integer;
  Peso: array[1..10] of Integer;
begin
  // Inicializa os pesos para cálculo do dígito verificador
  Peso[1] := 3;
  Peso[2] := 2;
  Peso[3] := 9;
  Peso[4] := 8;
  Peso[5] := 7;
  Peso[6] := 6;
  Peso[7] := 5;
  Peso[8] := 4;
  Peso[9] := 3;
  Peso[10] := 2;

  // Gera os 10 primeiros dígitos, começando com um número não-zero
  RENAVAM := IntToStr(1 + Random(9)); // Primeiro dígito não pode ser zero
  
  for i := 2 to 10 do
    RENAVAM := RENAVAM + IntToStr(Random(10));
    
  // Calcula o dígito verificador
  Soma := 0;
  for i := 1 to 10 do
    Soma := Soma + StrToInt(RENAVAM[i]) * Peso[i];
    
  Resto := Soma mod 11;
  
  if Resto < 2 then
    Digito := 0
  else
    Digito := 11 - Resto;
    
  Result := RENAVAM + IntToStr(Digito);
end;

function TFakeDataGenerator.GerarPlacaVeiculo(Mercosul: Boolean = False): string;
const
  Letras = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  i: Integer;
  Placa: string;
begin
  // Gera três letras para a parte alfabética
  Placa := '';
  for i := 1 to 3 do
    Placa := Placa + Letras[Random(Length(Letras)) + 1];
  
  if Mercosul then
  begin
    // Formato Mercosul: ABC1D23 (3 letras, 1 número, 1 letra, 2 números)
    Placa := Placa + IntToStr(Random(10)) + 
             Letras[Random(Length(Letras)) + 1] + 
             IntToStr(Random(10)) + IntToStr(Random(10));
  end
  else
  begin
    // Formato antigo: ABC1234 (3 letras, 4 números)
    Placa := Placa + IntToStr(Random(10)) + 
             IntToStr(Random(10)) + IntToStr(Random(10)) + 
             IntToStr(Random(10));
  end;
  
  Result := Placa;
end;

function TFakeDataGenerator.GerarCartaoCredito(Bandeira: string = ''): string;
var
  Prefixo: string;
  Comprimento: Integer;
  Cartao: string;
  i, Soma, Digito, ValorDigito: Integer;
  Dobro: Boolean;
begin
  // Define prefixo e comprimento baseado na bandeira
  if Bandeira = '' then
    Bandeira := Copy('VMAD', Random(4) + 1, 1); // Visa, Mastercard, Amex, Discover
    
  case UpCase(Bandeira[1]) of
    'V': begin // Visa
          Prefixo := '4';
          Comprimento := 16;
         end;
    'M': begin // Mastercard
          Prefixo := '5' + IntToStr(1 + Random(5)); // 51-55
          Comprimento := 16;
         end;
    'A': begin // American Express
          if Random(2) = 0 then
            Prefixo := '34'
          else
            Prefixo := '37';
          Comprimento := 15;
         end;
    'D': begin // Discover
          Prefixo := '6011';
          Comprimento := 16;
         end;
    else begin // Padrão - Visa
          Prefixo := '4';
          Comprimento := 16;
         end;
  end;
  
  // Gera os dígitos restantes (exceto o último que é verificador)
  Cartao := Prefixo;
  while Length(Cartao) < Comprimento - 1 do
    Cartao := Cartao + IntToStr(Random(10));
    
  // Implementa o algoritmo de Luhn para calcular o dígito verificador
  Soma := 0;
  Dobro := False;
  
  for i := Length(Cartao) downto 1 do
  begin
    ValorDigito := StrToInt(Cartao[i]);
    
    if Dobro then
    begin
      ValorDigito := ValorDigito * 2;
      if ValorDigito > 9 then
        ValorDigito := ValorDigito - 9;
    end;
    
    Soma := Soma + ValorDigito;
    Dobro := not Dobro;
  end;
  
  Digito := (10 - (Soma mod 10)) mod 10;
  
  Result := Cartao + IntToStr(Digito);
  
  // Formata o número do cartão, se desejar adicionar formatação
  // Ex: XXXX-XXXX-XXXX-XXXX
end;

function TFakeDataGenerator.GerarContaBancaria(Banco: string = ''; Formatado: Boolean = True): string;
var
  BancoNum, Agencia, Conta, DigitoAgencia, DigitoConta: string;
  i, Soma, Resto, Digito, Valor: Integer;
  BancosComuns: array[0..4] of string;
begin
  // Lista dos bancos mais comuns
  BancosComuns[0] := '001'; // Banco do Brasil
  BancosComuns[1] := '341'; // Itaú
  BancosComuns[2] := '033'; // Santander
  BancosComuns[3] := '104'; // Caixa Econômica
  BancosComuns[4] := '237'; // Bradesco
  
  // Se não foi especificado um banco, escolhe um aleatoriamente
  if Banco = '' then
    BancoNum := BancosComuns[Random(5)]
  else
    BancoNum := Banco;
  
  // Gera número da agência (4 dígitos)
  Agencia := '';
  for i := 1 to 4 do
    Agencia := Agencia + IntToStr(Random(10));
  
  // Gera dígito verificador da agência (alguns bancos usam)
  Soma := 0;
  for i := 1 to 4 do
  begin
    Valor := StrToInt(Agencia[i]) * (5 - i);
    Soma := Soma + Valor;
  end;
  
  Resto := Soma mod 11;
  if Resto = 0 then
    DigitoAgencia := '0'
  else if Resto = 1 then
    DigitoAgencia := 'X'
  else
    DigitoAgencia := IntToStr(11 - Resto);
    
  // Gera número da conta (entre 5 e 8 dígitos, dependendo do banco)
  Conta := '';
  for i := 1 to 6 + Random(3) do // 6 a 8 dígitos
    Conta := Conta + IntToStr(Random(10));
  
  // Gera dígito verificador da conta
  Soma := 0;
  for i := 1 to Length(Conta) do
  begin
    Valor := StrToInt(Conta[i]) * ((Length(Conta) + 1) - i);
    Soma := Soma + Valor;
  end;
  
  Resto := Soma mod 11;
  if Resto = 0 then
    DigitoConta := '0'
  else if Resto = 1 then
    DigitoConta := 'X'
  else
    DigitoConta := IntToStr(11 - Resto);
  
  if Formatado then
    Result := 'Banco: ' + BancoNum + ' - Agência: ' + Agencia + '-' + DigitoAgencia + 
              ' - Conta: ' + Conta + '-' + DigitoConta
  else
    Result := BancoNum + Agencia + DigitoAgencia + Conta + DigitoConta;
end;

function TFakeDataGenerator.GerarTituloEleitor(Formatado: Boolean = True): string;
var
  i, Soma, Resto, Digito1, Digito2: Integer;
  UF, Titulo, Zona, Secao: string;
  UFCodigos: array[0..26] of string;
begin
  // Códigos das UFs para título de eleitor 
  UFCodigos[0] := '01'; // SP
  UFCodigos[1] := '02'; // MG
  UFCodigos[2] := '03'; // RJ
  UFCodigos[3] := '04'; // RS
  UFCodigos[4] := '05'; // BA
  UFCodigos[5] := '06'; // PR
  UFCodigos[6] := '07'; // CE
  UFCodigos[7] := '08'; // PE
  UFCodigos[8] := '09'; // SC
  UFCodigos[9] := '10'; // GO
  UFCodigos[10] := '11'; // MA
  UFCodigos[11] := '12'; // PB
  UFCodigos[12] := '13'; // PA
  UFCodigos[13] := '14'; // ES
  UFCodigos[14] := '15'; // PI
  UFCodigos[15] := '16'; // RN
  UFCodigos[16] := '17'; // AL
  UFCodigos[17] := '18'; // MT
  UFCodigos[18] := '19'; // MS
  UFCodigos[19] := '20'; // DF
  UFCodigos[20] := '21'; // SE
  UFCodigos[21] := '22'; // AM
  UFCodigos[22] := '23'; // RO
  UFCodigos[23] := '24'; // AC
  UFCodigos[24] := '25'; // AP
  UFCodigos[25] := '26'; // RR
  UFCodigos[26] := '27'; // TO
  
  // Seleciona UF aleatória
  UF := UFCodigos[Random(27)];
  
  // Gera 8 dígitos para o número do título
  Titulo := '';
  for i := 1 to 8 do
    Titulo := Titulo + IntToStr(Random(10));
  
  // Calcula o primeiro dígito verificador (baseado no estado)
  Soma := StrToInt(UF) * 9 + 
          StrToInt(Titulo[1]) * 8 + 
          StrToInt(Titulo[2]) * 7 + 
          StrToInt(Titulo[3]) * 6 + 
          StrToInt(Titulo[4]) * 5 + 
          StrToInt(Titulo[5]) * 4 + 
          StrToInt(Titulo[6]) * 3 + 
          StrToInt(Titulo[7]) * 2 +
          StrToInt(Titulo[8]) * 1;
  
  Resto := Soma mod 11;
  if Resto = 10 then
    Digito1 := 0
  else
    Digito1 := Resto;
  
  // Calcula o segundo dígito verificador
  Soma := StrToInt(Titulo[1]) * 9 + 
          StrToInt(Titulo[2]) * 8 + 
          StrToInt(Titulo[3]) * 7 + 
          StrToInt(Titulo[4]) * 6 + 
          StrToInt(Titulo[5]) * 5 + 
          StrToInt(Titulo[6]) * 4 + 
          StrToInt(Titulo[7]) * 3 + 
          StrToInt(Titulo[8]) * 2 +
          Digito1 * 1;
  
  Resto := Soma mod 11;
  if Resto = 10 then
    Digito2 := 0
  else
    Digito2 := Resto;
  
  // Gera zona e seção
  Zona := Format('%.3d', [Random(999) + 1]);   // Zona de 001 a 999
  Secao := Format('%.4d', [Random(9999) + 1]); // Seção de 0001 a 9999
  
  if Formatado then
    Result := Titulo + Format('%.2d%.2d', [Digito1, Digito2]) + ' ' + Zona + ' ' + Secao
  else
    Result := Titulo + Format('%.2d%.2d', [Digito1, Digito2]) + Zona + Secao;
end;

function TFakeDataGenerator.GerarRG(Formatado: Boolean = True): string;
var
  i, Soma, Digito: Integer;
  RG: string;
  DigitoStr: string;
begin
  // Gera os primeiros dígitos (normalmente de 7 a 9 dígitos)
  RG := '';
  for i := 1 to 8 do // Usaremos 8 dígitos + DV
    RG := RG + IntToStr(Random(10));
    
  // Calcula o dígito verificador (algoritmo simplificado, sem especificidade por UF)
  Soma := 0;
  for i := 1 to Length(RG) do
    Soma := Soma + StrToInt(RG[i]) * (10 - i);
    
  Digito := Soma mod 11;
  
  case Digito of
    0..9: DigitoStr := IntToStr(Digito);
    10: DigitoStr := 'X';
  else
    DigitoStr := '0';
  end;
  
  // Formata o RG se necessário
  if Formatado then
    Result := Copy(RG, 1, 2) + '.' + 
              Copy(RG, 3, 3) + '.' + 
              Copy(RG, 6, 3) + '-' + 
              DigitoStr
  else
    Result := RG + DigitoStr;
end;

function TFakeDataGenerator.GerarDataNascimento(IdadeMinima: Integer = 18; IdadeMaxima: Integer = 80): TDateTime;
var
  DiasAno: Integer;
  DataAtual: TDateTime;
  AnosAleatorios: Integer;
begin
  DataAtual := Date;
  DiasAno := 365;
  AnosAleatorios := IdadeMinima + Random(IdadeMaxima - IdadeMinima + 1);
  Result := IncDay(DataAtual, -AnosAleatorios * DiasAno - Random(DiasAno)); // Subtrai anos e dias aleatórios
end;

function TFakeDataGenerator.GerarPerfilRedeSocial(const Nome: string; RedeSocial: string = 'instagram'): string;
var
  NomeSemAcentos, NomeUsuario: string;
  Separadores: array[0..3] of string;
  PossivelSufixo: Boolean;
begin
  // Remove espaços e acentos
  NomeSemAcentos := AnsiLowerCase(Nome);
  NomeSemAcentos := StringReplace(NomeSemAcentos, ' ', '', [rfReplaceAll]);
  
  // Substituições de caracteres acentuados
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'á', 'a', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'à', 'a', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'â', 'a', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'ã', 'a', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'é', 'e', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'ê', 'e', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'í', 'i', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'ó', 'o', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'ô', 'o', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'õ', 'o', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'ú', 'u', [rfReplaceAll]);
  NomeSemAcentos := StringReplace(NomeSemAcentos, 'ç', 'c', [rfReplaceAll]);
  
  // Define variações possíveis para nome de usuário
  Separadores[0] := '';
  Separadores[1] := '.';
  Separadores[2] := '_';
  Separadores[3] := '';
  
  // Cria nome de usuário básico
  NomeUsuario := NomeSemAcentos;
  
  // Adiciona sufixo aleatório em alguns casos
  PossivelSufixo := Random(3) > 0; // 2/3 de chance de adicionar sufixo
  
  if PossivelSufixo then
  begin
    case Random(5) of
      0: NomeUsuario := NomeUsuario + Separadores[Random(4)] + IntToStr(Random(100));
      1: NomeUsuario := NomeUsuario + Separadores[Random(4)] + IntToStr(YearOf(Date) - Random(30));
      2: NomeUsuario := NomeUsuario + Separadores[Random(4)] + 'real';
      3: NomeUsuario := NomeUsuario + Separadores[Random(4)] + 'br';
      4: NomeUsuario := NomeUsuario + Separadores[Random(4)] + 'oficial';
    end;
  end;
  
  // Formata de acordo com a rede social
  RedeSocial := AnsiLowerCase(RedeSocial);
  if (RedeSocial = 'twitter') or (RedeSocial = 'x') then
    Result := '@' + NomeUsuario
  else if RedeSocial = 'instagram' then
    Result := '@' + NomeUsuario
  else if RedeSocial = 'linkedin' then
    Result := 'linkedin.com/in/' + NomeUsuario
  else if RedeSocial = 'facebook' then
    Result := 'facebook.com/' + NomeUsuario
  else
    Result := '@' + NomeUsuario;
end;

function TFakeDataGenerator.GerarEstadoCivil: string;
var
  EstadosCivis: array[0..4] of string;
  Probabilidades: array[0..4] of Integer;
  ValorSorteio, SomaProbabilidades, i: Integer;
begin
  // Estados civis com suas respectivas probabilidades
  EstadosCivis[0] := 'Solteiro(a)';
  EstadosCivis[1] := 'Casado(a)';
  EstadosCivis[2] := 'Divorciado(a)';
  EstadosCivis[3] := 'Viúvo(a)';
  EstadosCivis[4] := 'União Estável';
  
  // Probabilidades relativas (ajuste conforme necessário)
  Probabilidades[0] := 40; // 40% de chance de ser solteiro
  Probabilidades[1] := 35; // 35% de chance de ser casado
  Probabilidades[2] := 10; // 10% de chance de ser divorciado
  Probabilidades[3] := 5;  // 5% de chance de ser viúvo
  Probabilidades[4] := 10; // 10% de chance de estar em união estável
  
  // Calcula soma das probabilidades
  SomaProbabilidades := 0;
  for i := 0 to 4 do
    SomaProbabilidades := SomaProbabilidades + Probabilidades[i];
  
  // Sorteia um valor dentro do range de probabilidades
  ValorSorteio := Random(SomaProbabilidades) + 1;
  
  // Determina qual estado civil foi sorteado
  SomaProbabilidades := 0;
  for i := 0 to 4 do
  begin
    SomaProbabilidades := SomaProbabilidades + Probabilidades[i];
    if ValorSorteio <= SomaProbabilidades then
    begin
      Result := EstadosCivis[i];
      Break;
    end;
  end;
end;

function TFakeDataGenerator.GerarProfissao: string;
var
  Profissoes: array[0..19] of string;
begin
  Profissoes[0] := 'Administrador(a)';
  Profissoes[1] := 'Advogado(a)';
  Profissoes[2] := 'Analista de Sistemas';
  Profissoes[3] := 'Arquiteto(a)';
  Profissoes[4] := 'Contador(a)';
  Profissoes[5] := 'Designer';
  Profissoes[6] := 'Economista';
  Profissoes[7] := 'Enfermeiro(a)';
  Profissoes[8] := 'Engenheiro(a)';
  Profissoes[9] := 'Farmacêutico(a)';
  Profissoes[10] := 'Médico(a)';
  Profissoes[11] := 'Professor(a)';
  Profissoes[12] := 'Programador(a)';
  Profissoes[13] := 'Psicólogo(a)';
  Profissoes[14] := 'Técnico(a) em Informática';
  Profissoes[15] := 'Vendedor(a)';
  Profissoes[16] := 'Gerente Comercial';
  Profissoes[17] := 'Fisioterapeuta';
  Profissoes[18] := 'Nutricionista';
  Profissoes[19] := 'Empresário(a)';
  
  Result := Profissoes[Random(20)];
end;

function TFakeDataGenerator.GerarEscolaridade: string;
var
  NiveisEscolaridade: array[0..6] of string;
  Probabilidades: array[0..6] of Integer;
  ValorSorteio, SomaProbabilidades, i: Integer;
begin
  // Níveis de escolaridade
  NiveisEscolaridade[0] := 'Ensino Fundamental Incompleto';
  NiveisEscolaridade[1] := 'Ensino Fundamental Completo';
  NiveisEscolaridade[2] := 'Ensino Médio Incompleto';
  NiveisEscolaridade[3] := 'Ensino Médio Completo';
  NiveisEscolaridade[4] := 'Ensino Superior Incompleto';
  NiveisEscolaridade[5] := 'Ensino Superior Completo';
  NiveisEscolaridade[6] := 'Pós-graduação';
  
  // Probabilidades relativas (ajuste conforme necessário)
  Probabilidades[0] := 5;  // 5% Fundamental Incompleto
  Probabilidades[1] := 10; // 10% Fundamental Completo
  Probabilidades[2] := 10; // 10% Médio Incompleto
  Probabilidades[3] := 30; // 30% Médio Completo
  Probabilidades[4] := 15; // 15% Superior Incompleto
  Probabilidades[5] := 20; // 20% Superior Completo
  Probabilidades[6] := 10; // 10% Pós-graduação
  
  // Calcula soma das probabilidades
  SomaProbabilidades := 0;
  for i := 0 to 6 do
    SomaProbabilidades := SomaProbabilidades + Probabilidades[i];
  
  // Sorteia um valor dentro do range de probabilidades
  ValorSorteio := Random(SomaProbabilidades) + 1;
  
  // Determina qual nível de escolaridade foi sorteado
  SomaProbabilidades := 0;
  for i := 0 to 6 do
  begin
    SomaProbabilidades := SomaProbabilidades + Probabilidades[i];
    if ValorSorteio <= SomaProbabilidades then
    begin
      Result := NiveisEscolaridade[i];
      Break;
    end;
  end;
end;

{ Dadpos Empresariais }
function TFakeDataGenerator.GerarCNAE(Formatado: Boolean = True): string;
var
  CNAEPrincipais: array[0..9] of string;
  CNAE: string;
begin
  // Exemplos de CNAEs principais mais comuns
  CNAEPrincipais[0] := '4751201'; // Comércio varejista especializado de equipamentos e suprimentos de informática
  CNAEPrincipais[1] := '4781400'; // Comércio varejista de artigos do vestuário e acessórios
  CNAEPrincipais[2] := '5611201'; // Restaurantes e similares
  CNAEPrincipais[3] := '4711301'; // Comércio varejista de mercadorias em geral - hipermercados
  CNAEPrincipais[4] := '4530703'; // Comércio a varejo de peças e acessórios novos para veículos automotores
  CNAEPrincipais[5] := '8599604'; // Treinamento em desenvolvimento profissional e gerencial
  CNAEPrincipais[6] := '6201501'; // Desenvolvimento de programas de computador sob encomenda
  CNAEPrincipais[7] := '6920601'; // Atividades de contabilidade
  CNAEPrincipais[8] := '8610101'; // Atividades de atendimento hospitalar
  CNAEPrincipais[9] := '4120400'; // Construção de edifícios

  // Seleciona um CNAE principal aleatório
  CNAE := CNAEPrincipais[Random(10)];
  
  if Formatado then
    Result := Copy(CNAE, 1, 4) + '-' + Copy(CNAE, 5, 1) + '/' + Copy(CNAE, 6, 2)
  else
    Result := CNAE;
end;

function TFakeDataGenerator.GerarInscricaoMunicipal(Municipio: string = ''): string;
var
  i: Integer;
  InscMunicipal: string;
  Digito: Integer;
  Soma: Integer;
begin
  // Gera um número de inscrição municipal com 8 dígitos + DV
  InscMunicipal := '';
  for i := 1 to 8 do
    InscMunicipal := InscMunicipal + IntToStr(Random(10));
  
  // Calcula dígito verificador (algoritmo simplificado)
  Soma := 0;
  for i := 1 to 8 do
    Soma := Soma + StrToInt(InscMunicipal[i]) * (9 - i);
  
  Digito := Soma mod 11;
  if Digito = 10 then
    Digito := 0;
  
  Result := InscMunicipal + IntToStr(Digito);
end;

function TFakeDataGenerator.GerarDepartamento: string;
var
  Departamentos: array[0..14] of string;
begin
  Departamentos[0] := 'Administrativo';
  Departamentos[1] := 'Recursos Humanos';
  Departamentos[2] := 'Financeiro';
  Departamentos[3] := 'Contabilidade';
  Departamentos[4] := 'Comercial';
  Departamentos[5] := 'Marketing';
  Departamentos[6] := 'Compras';
  Departamentos[7] := 'Logística';
  Departamentos[8] := 'Tecnologia da Informação';
  Departamentos[9] := 'Jurídico';
  Departamentos[10] := 'Produção';
  Departamentos[11] := 'Qualidade';
  Departamentos[12] := 'Pesquisa e Desenvolvimento';
  Departamentos[13] := 'Atendimento ao Cliente';
  Departamentos[14] := 'Manutenção';
  
  Result := Departamentos[Random(15)];
end;

function TFakeDataGenerator.GerarCargo: string;
var
  NiveisCargos: array[0..3] of string;
  AreasCargos: array[0..8] of string;
  PrefixosCargos: array[0..3] of string;
begin
  NiveisCargos[0] := '';
  NiveisCargos[1] := 'Assistente de ';
  NiveisCargos[2] := 'Analista de ';
  NiveisCargos[3] := 'Coordenador de ';
  
  AreasCargos[0] := 'Vendas';
  AreasCargos[1] := 'Marketing';
  AreasCargos[2] := 'RH';
  AreasCargos[3] := 'Financeiro';
  AreasCargos[4] := 'TI';
  AreasCargos[5] := 'Administrativo';
  AreasCargos[6] := 'Comercial';
  AreasCargos[7] := 'Operações';
  AreasCargos[8] := 'Projetos';
  
  PrefixosCargos[0] := 'Gerente de ';
  PrefixosCargos[1] := 'Diretor de ';
  PrefixosCargos[2] := 'Supervisor de ';
  PrefixosCargos[3] := 'Consultor de ';
  
  if Random(10) > 6 then // 30% de chance de ser cargo de gestão
    Result := PrefixosCargos[Random(4)] + AreasCargos[Random(9)]
  else
    Result := NiveisCargos[Random(4)] + AreasCargos[Random(9)];
    
  // Algumas exceções para cargos específicos
  if Random(10) = 0 then // 10% de chance de ser um cargo específico
  begin
    case Random(5) of
      0: Result := 'Desenvolvedor';
      1: Result := 'Contador';
      2: Result := 'Advogado';
      3: Result := 'Designer';
      4: Result := 'Recepcionista';
    end;
  end;
end;

function TFakeDataGenerator.GerarChavePIX(TipoChave: string = 'aleatoria'): string;
var
  TiposChave: array[0..3] of string;
begin
  TiposChave[0] := 'cpf';
  TiposChave[1] := 'cnpj';
  TiposChave[2] := 'email';
  TiposChave[3] := 'telefone';
  
  // Se não for especificado ou for aleatório, escolhe um tipo aleatoriamente
  if (TipoChave = 'aleatoria') or (TipoChave = '') then
    TipoChave := TiposChave[Random(4)];
  
  TipoChave := AnsiLowerCase(TipoChave);
  
  if TipoChave = 'cpf' then
    Result := GerarCPF(False)
  else if TipoChave = 'cnpj' then
    Result := GerarCNPJ(False)
  else if TipoChave = 'email' then
    Result := GerarEmail(GerarNomeCompleto)
  else if TipoChave = 'telefone' then
    Result := '+55' + GerarCelular(False)
  else if TipoChave = 'aleatoria' then
  begin
    // Gera uma chave aleatória de 32 caracteres (formato UUID)
    Result := '';
    while Length(Result) < 32 do
    begin
      case Random(3) of
        0: Result := Result + Chr(Ord('0') + Random(10)); // Dígito
        1: Result := Result + Chr(Ord('a') + Random(6));  // a-f
        2: Result := Result + Chr(Ord('0') + Random(10)); // Dígito
      end;
    end;
    
    // Formata como UUID
    Result := Copy(Result, 1, 8) + '-' + 
              Copy(Result, 9, 4) + '-' + 
              Copy(Result, 13, 4) + '-' + 
              Copy(Result, 17, 4) + '-' + 
              Copy(Result, 21, 12);
  end;
end;

{ Dados Financeiros}
function TFakeDataGenerator.GerarCodigoBarrasBoleto(Banco: string = ''): string;
var
  CodigoBarras: string;
  CodigoBanco, Moeda, FatorVencimento, Valor: string;
  DV, i: Integer;
  DataVencimento: TDateTime;
  ValorBoleto: Double;
  Soma: Integer;
  Peso: Integer;
begin
  // Se o banco não for especificado, escolhe um dos principais
  if Banco = '' then
  begin
    case Random(5) of
      0: Banco := '001'; // Banco do Brasil
      1: Banco := '341'; // Itaú
      2: Banco := '033'; // Santander
      3: Banco := '104'; // Caixa
      4: Banco := '237'; // Bradesco
    end;
  end;
  
  CodigoBanco := Banco;
  Moeda := '9'; // Real
  
  // Gera uma data de vencimento aleatória (entre hoje e 60 dias à frente)
  DataVencimento := GerarData(Date, Date + 60);
  // Calcula o fator de vencimento (base: 07/10/1997)
  FatorVencimento := Format('%.4d', [DaysBetween(EncodeDate(1997, 10, 7), DataVencimento)]);
  
  // Gera um valor aleatório entre R$ 10,00 e R$ 10.000,00
  ValorBoleto := Random * 9990 + 10;
  // Formata o valor (10 dígitos, com zeros à esquerda)
  Valor := Format('%.10d', [Round(ValorBoleto * 100)]);
  
  // Gera o campo livre (20 dígitos)
  CodigoBarras := CodigoBanco + Moeda;
  for i := 1 to 20 do
    CodigoBarras := CodigoBarras + IntToStr(Random(10));
  
  // Insere fator de vencimento e valor
  CodigoBarras := Copy(CodigoBarras, 1, 4) + FatorVencimento + Valor + Copy(CodigoBarras, 5, 20);
  
  // Calcula dígito verificador do código de barras (Módulo 11)
  Soma := 0;
  Peso := 2;
  for i := Length(CodigoBarras) downto 1 do
  begin
    Soma := Soma + (StrToInt(CodigoBarras[i]) * Peso);
    Inc(Peso);
    if Peso > 9 then
      Peso := 2;
  end;
  
  DV := 11 - (Soma mod 11);
  if (DV = 0) or (DV = 10) or (DV = 11) then
    DV := 1;
  
  // Insere o DV na posição 5
  Result := Copy(CodigoBarras, 1, 4) + IntToStr(DV) + Copy(CodigoBarras, 5, 39);
end;

function TFakeDataGenerator.GerarLinhaDigitavelBoleto(Banco: string = ''): string;
var
  CodigoBarras: string;
  Campo1, Campo2, Campo3, Campo4, Campo5: string;
  DV1, DV2, DV3, DV4: Integer;
  i, Soma, Resto: Integer;
  Peso: Integer;
begin
  // Gera o código de barras
  CodigoBarras := GerarCodigoBarrasBoleto(Banco);
  
  // Divide o código de barras em campos para a linha digitável
  Campo1 := Copy(CodigoBarras, 1, 3) + Copy(CodigoBarras, 4, 1) + Copy(CodigoBarras, 20, 5);
  Campo2 := Copy(CodigoBarras, 25, 10);
  Campo3 := Copy(CodigoBarras, 35, 10);
  Campo4 := Copy(CodigoBarras, 5, 1); // DV do código de barras
  Campo5 := Copy(CodigoBarras, 6, 14); // Fator de vencimento (4) + Valor (10)
  
  // Calcula os DVs de cada campo usando Módulo 10
  // Campo 1
  Soma := 0;
  Peso := 2;
  for i := Length(Campo1) downto 1 do
  begin
    Soma := Soma + IfThen(StrToInt(Campo1[i]) * Peso > 9, 
                          (StrToInt(Campo1[i]) * Peso) div 10 + (StrToInt(Campo1[i]) * Peso) mod 10, 
                          StrToInt(Campo1[i]) * Peso);
    Peso := IfThen(Peso = 2, 1, 2);
  end;
  Resto := Soma mod 10;
  DV1 := IfThen(Resto = 0, 0, 10 - Resto);
  
  // Campo 2
  Soma := 0;
  Peso := 2;
  for i := Length(Campo2) downto 1 do
  begin
    Soma := Soma + IfThen(StrToInt(Campo2[i]) * Peso > 9, 
                          (StrToInt(Campo2[i]) * Peso) div 10 + (StrToInt(Campo2[i]) * Peso) mod 10, 
                          StrToInt(Campo2[i]) * Peso);
    Peso := IfThen(Peso = 2, 1, 2);
  end;
  Resto := Soma mod 10;
  DV2 := IfThen(Resto = 0, 0, 10 - Resto);
  
  // Campo 3
  Soma := 0;
  Peso := 2;
  for i := Length(Campo3) downto 1 do
  begin
    Soma := Soma + IfThen(StrToInt(Campo3[i]) * Peso > 9, 
                          (StrToInt(Campo3[i]) * Peso) div 10 + (StrToInt(Campo3[i]) * Peso) mod 10, 
                          StrToInt(Campo3[i]) * Peso);
    Peso := IfThen(Peso = 2, 1, 2);
  end;
  Resto := Soma mod 10;
  DV3 := IfThen(Resto = 0, 0, 10 - Resto);
  
  // Monta a linha digitável formatada
  Result := Copy(Campo1, 1, 5) + '.' + Copy(Campo1, 6, 4) + IntToStr(DV1) + ' ' +
            Copy(Campo2, 1, 5) + '.' + Copy(Campo2, 6, 5) + IntToStr(DV2) + ' ' +
            Copy(Campo3, 1, 5) + '.' + Copy(Campo3, 6, 5) + IntToStr(DV3) + ' ' +
            Campo4 + ' ' +
            Campo5;
end;

function TFakeDataGenerator.GerarCodigoRastreio: string;
const
  Letras = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
var
  Codigo: string;
  i: Integer;
begin
  // Códigos de rastreio dos Correios seguem o padrão: 2 letras + 9 dígitos + BR
  Codigo := '';
  
  // 2 letras iniciais
  for i := 1 to 2 do
    Codigo := Codigo + Letras[Random(Length(Letras)) + 1];
  
  // 9 dígitos
  for i := 1 to 9 do
    Codigo := Codigo + IntToStr(Random(10));
  
  // Sufixo BR
  Codigo := Codigo + 'BR';
  
  Result := Codigo;
end;

function TFakeDataGenerator.GerarAliquotaImposto(TipoImposto: string = 'ICMS'): Double;
begin
  TipoImposto := UpperCase(TipoImposto);
  
  if TipoImposto = 'ICMS' then
    // Alíquotas comuns de ICMS: 4%, 7%, 12%, 17%, 18%, 25%
    case Random(6) of
      0: Result := 4.0;
      1: Result := 7.0;
      2: Result := 12.0;
      3: Result := 17.0;
      4: Result := 18.0;
      5: Result := 25.0;
    end
  else if TipoImposto = 'IPI' then
    // Alíquotas comuns de IPI: entre 0% e 20%
    Result := Random(20) + Random
  else if TipoImposto = 'ISS' then
    // Alíquotas comuns de ISS: entre 2% e 5%
    Result := 2.0 + Random(3) + Random
  else if TipoImposto = 'PIS' then
    // Alíquota PIS: 0.65% ou 1.65%
    if Random(2) = 0 then
      Result := 0.65
    else
      Result := 1.65
  else if TipoImposto = 'COFINS' then
    // Alíquota COFINS: 3% ou 7.6%
    if Random(2) = 0 then
      Result := 3.0
    else
      Result := 7.6
  else
    // Alíquota genérica entre 1% e 30%
    Result := 1.0 + Random(29) + Random;
  
  // Arredonda para 2 casas decimais
  Result := RoundTo(Result, -2);
end;

function TFakeDataGenerator.GerarNotaFiscal(UF: string = ''): string;
var
  Serie, Numero, Modelo, AnoEmissao, UF_Sigla: string;
begin
  // Gera a série da nota (geralmente 1 a 999)
  Serie := Format('%.3d', [Random(999) + 1]);
  
  // Gera o número da nota (pode ter até 9 dígitos)
  Numero := Numero + IntToStr(1 + Random(9));
  while Length(Numero) < 9 do
    Numero := Numero + IntToStr(Random(10));

  // Modelos mais comuns de NF
  case Random(4) of
    0: Modelo := '55'; // NF-e
    1: Modelo := '65'; // NFC-e
    2: Modelo := '57'; // CT-e
    3: Modelo := '59'; // SAT
  end;
  
  // Ano de emissão (últimos 2 dígitos)
  AnoEmissao := Copy(IntToStr(YearOf(Date)), 3, 2);
  
  // Define a UF se não especificada
  if UF = '' then
    UF_Sigla := FUFs[Random(FUFs.Count)]
  else
    UF_Sigla := UF;
  
  if Modelo = '55' then // NF-e
    Result := Format('NF-e nº %s, Série %s, Emissão: %s/%s',
      [Numero, Serie, AnoEmissao, UF_Sigla])
  else if Modelo = '65' then // NFC-e
    Result := Format('NFC-e nº %s, Série %s, Emissão: %s/%s',
      [Numero, Serie, AnoEmissao, UF_Sigla])
  else if Modelo = '57' then // CT-e
    Result := Format('CT-e nº %s, Série %s, Emissão: %s/%s',
      [Numero, Serie, AnoEmissao, UF_Sigla])
  else // SAT ou outros
    Result := Format('CF-e-SAT nº %s, Emissão: %s/%s',
      [Numero, AnoEmissao, UF_Sigla]);
end;

{ Outros Dados Específicos}
function TFakeDataGenerator.GerarProtocolo: string;
var
  Ano, Sequencia: string;
  i: Integer;
begin
  // Formatos comuns de protocolos:
  // AAAA/NNNNNN ou AAAA-NNNNNN ou NNNNNNNNNN/AAAA
  
  // Ano atual ou ano recente
  if Random(2) = 0 then
    Ano := IntToStr(YearOf(Date))
  else
    Ano := IntToStr(YearOf(Date) - Random(3));
  
  // Sequência numérica
  Sequencia := '';
  for i := 1 to 6 + Random(4) do // 6 a 9 dígitos
    Sequencia := Sequencia + IntToStr(Random(10));
  
  // Formata o protocolo
  case Random(3) of
    0: Result := Ano + '/' + Sequencia;
    1: Result := Ano + '-' + Sequencia;
    2: Result := Sequencia + '/' + Ano;
  end;
end;

function TFakeDataGenerator.GerarSKU(Categoria: string = ''): string;
var
  Prefixo, CodigoProduto, CodigoVariacao: string;
  Categorias: array[0..9] of string;
  i: Integer;
begin
  // Categorias comuns de produtos
  Categorias[0] := 'ELET'; // Eletrônicos
  Categorias[1] := 'VEST'; // Vestuário
  Categorias[2] := 'MOVEL'; // Móveis
  Categorias[3] := 'LIVRO'; // Livros
  Categorias[4] := 'AUTO'; // Automotivo
  Categorias[5] := 'ALIM'; // Alimentos
  Categorias[6] := 'BEBID'; // Bebidas
  Categorias[7] := 'ESPO'; // Esportes
  Categorias[8] := 'INFO'; // Informática
  Categorias[9] := 'BRINQ'; // Brinquedos
  
  // Define o prefixo da categoria
  if Categoria = '' then
    Prefixo := Categorias[Random(10)]
  else
    Prefixo := Copy(UpperCase(Categoria), 1, 5);
    
  // Código do produto (4-6 dígitos)
  CodigoProduto := '';
  for i := 1 to 4 + Random(3) do
    CodigoProduto := CodigoProduto + IntToStr(Random(10));
    
  // Código de variação (1-2 dígitos) para cor/tamanho/modelo
  CodigoVariacao := '';
  for i := 1 to 1 + Random(2) do
    CodigoVariacao := CodigoVariacao + IntToStr(Random(10));
    
  // Formato final: PREFIXO-PRODUTO-VARIACAO
  Result := Prefixo + '-' + CodigoProduto + '-' + CodigoVariacao;
end;

function TFakeDataGenerator.GerarEAN13: string;
var
  EAN: string;
  i, Soma, Resto, DigitoVerif: Integer;
  Peso: Integer;
begin
  // Gera os 12 primeiros dígitos do EAN-13
  // Os primeiros 3 dígitos são o código do país (Brasil: 789 ou 790)
  if Random(2) = 0 then
    EAN := '789'
  else
    EAN := '790';
    
  // Completa até 12 dígitos
  while Length(EAN) < 12 do
    EAN := EAN + IntToStr(Random(10));
    
  // Calcula o dígito verificador (13º dígito)
  Soma := 0;
  Peso := 1;
  
  for i := 12 downto 1 do
  begin
    Soma := Soma + StrToInt(EAN[i]) * (1 + 2 * (i mod 2));
  end;
  
  Resto := Soma mod 10;
  if Resto = 0 then
    DigitoVerif := 0
  else
    DigitoVerif := 10 - Resto;
    
  Result := EAN + IntToStr(DigitoVerif);
end;

function TFakeDataGenerator.GerarCID: string;
var
  Capitulos: array[0..20] of string;
  Grupo, Categoria: string;
  NumeroCategoria: Integer;
begin
  // Exemplos de capítulos do CID-10
  Capitulos[0] := 'A'; // Algumas doenças infecciosas e parasitárias
  Capitulos[1] := 'B'; // Algumas doenças infecciosas e parasitárias
  Capitulos[2] := 'C'; // Neoplasias
  Capitulos[3] := 'D'; // Neoplasias / Doenças do sangue
  Capitulos[4] := 'E'; // Doenças endócrinas, nutricionais e metabólicas
  Capitulos[5] := 'F'; // Transtornos mentais e comportamentais
  Capitulos[6] := 'G'; // Doenças do sistema nervoso
  Capitulos[7] := 'H'; // Doenças do olho e anexos / Doenças do ouvido
  Capitulos[8] := 'I'; // Doenças do aparelho circulatório
  Capitulos[9] := 'J'; // Doenças do aparelho respiratório
  Capitulos[10] := 'K'; // Doenças do aparelho digestivo
  Capitulos[11] := 'L'; // Doenças da pele e do tecido subcutâneo
  Capitulos[12] := 'M'; // Doenças do sistema osteomuscular e do tecido conjuntivo
  Capitulos[13] := 'N'; // Doenças do aparelho geniturinário
  Capitulos[14] := 'O'; // Gravidez, parto e puerpério
  Capitulos[15] := 'P'; // Afecções do período perinatal
  Capitulos[16] := 'Q'; // Malformações congênitas
  Capitulos[17] := 'R'; // Sintomas, sinais e achados anormais
  Capitulos[18] := 'S'; // Lesões, envenenamento e outras causas externas
  Capitulos[19] := 'T'; // Lesões, envenenamento e outras causas externas
  Capitulos[20] := 'Z'; // Fatores que influenciam o estado de saúde
  
  // Seleciona um capítulo aleatório
  Grupo := Capitulos[Random(21)];
  
  // Gera o número da categoria (0-99)
  NumeroCategoria := Random(100);
  Categoria := Format('%.2d', [NumeroCategoria]);
  
  // Alguns CIDs têm subcategorias com 1 dígito adicional
  if Random(3) = 0 then // ~33% de chance
    Result := Grupo + Categoria + '.' + IntToStr(Random(10))
  else
    Result := Grupo + Categoria;
end;

function TFakeDataGenerator.GerarProcessoJudicial: string;
var
  Numero, DigitoVerificador, AnoProcesso, Segmento, Origem, UF: string;
  i, Soma, Resto, Digito: Integer;
  UFCodigos: array[0..26] of string;
begin
  // Estrutura CNJ: NNNNNNN-DD.AAAA.J.TR.OOOO
  // N = Número do processo
  // D = Dígito verificador
  // A = Ano do ajuizamento
  // J = Órgão ou segmento do Judiciário
  // TR = Tribunal do respectivo segmento
  // O = Unidade de origem do processo
  
  // Códigos de UF para processo judicial
  UFCodigos[0] := '01'; // AC
  UFCodigos[1] := '02'; // AL
  UFCodigos[2] := '03'; // AM
  UFCodigos[3] := '04'; // AP
  UFCodigos[4] := '05'; // BA
  UFCodigos[5] := '06'; // CE
  UFCodigos[6] := '07'; // DF
  UFCodigos[7] := '08'; // ES
  UFCodigos[8] := '09'; // GO
  UFCodigos[9] := '10'; // MA
  UFCodigos[10] := '11'; // MG
  UFCodigos[11] := '12'; // MS
  UFCodigos[12] := '13'; // MT
  UFCodigos[13] := '14'; // PA
  UFCodigos[14] := '15'; // PB
  UFCodigos[15] := '16'; // PE
  UFCodigos[16] := '17'; // PI
  UFCodigos[17] := '18'; // PR
  UFCodigos[18] := '19'; // RJ
  UFCodigos[19] := '20'; // RN
  UFCodigos[20] := '21'; // RO
  UFCodigos[21] := '22'; // RR
  UFCodigos[22] := '23'; // RS
  UFCodigos[23] := '24'; // SC
  UFCodigos[24] := '25'; // SE
  UFCodigos[25] := '26'; // SP
  UFCodigos[26] := '27'; // TO
  
  // Gera o número sequencial de 7 dígitos
  Numero := '';
  for i := 1 to 7 do
    Numero := Numero + IntToStr(Random(10));
  
  // Ano do processo (entre 2010 e ano atual)
  AnoProcesso := IntToStr(2010 + Random(YearOf(Date) - 2010 + 1));
  
  // Segmento do Judiciário
  case Random(5) of
    0: Segmento := '1'; // Justiça Federal
    1: Segmento := '2'; // Justiça do Trabalho
    2: Segmento := '3'; // Justiça Estadual
    3: Segmento := '4'; // Justiça Eleitoral
    4: Segmento := '5'; // Justiça Militar
  end;
  
  // Tribunal (UF do Tribunal)
  UF := UFCodigos[Random(27)];
  
  // Unidade de origem (Fórum/Vara)
  Origem := Format('%.4d', [Random(9999) + 1]);
  
  // Calcula o dígito verificador
  Soma := 0;
  for i := 1 to Length(Numero) do
    Soma := Soma + StrToInt(Numero[i]) * (i+1);
  
  Resto := Soma mod 97;
  Digito := 98 - Resto;
  DigitoVerificador := Format('%.2d', [Digito]);
  
  // Monta o número completo no formato CNJ
  Result := Numero + '-' + DigitoVerificador + '.' + 
            AnoProcesso + '.' + Segmento + '.' + 
            UF + '.' + Origem;
end;

function TFakeDataGenerator.GerarTipoSanguineo: string;
const
  Tipos: array[0..7] of string = ('A+', 'A-', 'B+', 'B-', 'AB+', 'AB-', 'O+', 'O-');
  // Distribuição aproximada na população brasileira
  Probabilidades: array[0..7] of Integer = (36, 6, 9, 2, 3, 1, 38, 5);
var
  ValorSorteio, SomaProbabilidades, i: Integer;
begin
  // Calcula soma das probabilidades
  SomaProbabilidades := 0;
  for i := 0 to 7 do
    SomaProbabilidades := SomaProbabilidades + Probabilidades[i];
  
  // Sorteia um valor dentro do range de probabilidades
  ValorSorteio := Random(SomaProbabilidades) + 1;
  
  // Determina qual tipo sanguíneo foi sorteado
  SomaProbabilidades := 0;
  for i := 0 to 7 do
  begin
    SomaProbabilidades := SomaProbabilidades + Probabilidades[i];
    if ValorSorteio <= SomaProbabilidades then
    begin
      Result := Tipos[i];
      Break;
    end;
  end;
end;

function TFakeDataGenerator.GerarAltura(Min: Integer = 150; Max: Integer = 200): Integer;
var
  Media, DesvioPadrao: Double;
  Altura: Double;
begin
  // Utilize distribuição normal para maior realismo
  Media := (Min + Max) / 2;
  DesvioPadrao := (Max - Min) / 6; // ~ 99.7% dos valores entre Min e Max
  
  // Gerar usando distribuição normal
  repeat
    // Box-Muller transform para gerar distribuição normal
    Altura := Media + DesvioPadrao * Sqrt(-2 * Ln(Random + 0.000001)) * 
              Cos(2 * PI * Random);
  until (Altura >= Min) and (Altura <= Max);
  
  Result := Round(Altura);
end;

function TFakeDataGenerator.GerarPeso(Min: Integer = 50; Max: Integer = 120): Double;
var
  Media, DesvioPadrao: Double;
  Peso: Double;
begin
  // Utilize distribuição normal para maior realismo
  Media := (Min + Max) / 2;
  DesvioPadrao := (Max - Min) / 6; // ~ 99.7% dos valores entre Min e Max
  
  // Gerar usando distribuição normal
  repeat
    // Box-Muller transform para gerar distribuição normal
    Peso := Media + DesvioPadrao * Sqrt(-2 * Ln(Random + 0.000001)) * 
            Sin(2 * PI * Random);
  until (Peso >= Min) and (Peso <= Max);
  
  // Arredondar para 1 casa decimal
  Result := RoundTo(Peso, -1);
end;

function TFakeDataGenerator.GerarPressaoArterial: string;
var
  // Valores aproximados para adultos saudáveis
  SistolicaMin, SistolicaMax: Integer;
  DiastolicaMin, DiastolicaMax: Integer;
  Sistolica, Diastolica: Integer;
  Chance: Integer;
begin
  // Define ranges normais de pressão (pode ser modificado para criar casos específicos)
  Chance := Random(100);
  
  if Chance < 10 then
    begin // Hipertensão
      SistolicaMin := 140;
      SistolicaMax := 179;
      DiastolicaMin := 90;
      DiastolicaMax := 109;
    end
  else if Chance < 15 then
    begin // Hipotensão
      SistolicaMin := 90;
      SistolicaMax := 100;
      DiastolicaMin := 50;
      DiastolicaMax := 65;
    end
  else
    begin // Normal
      SistolicaMin := 110;
      SistolicaMax := 139;
      DiastolicaMin := 70;
      DiastolicaMax := 89;
    end;
  
  Sistolica := SistolicaMin + Random(SistolicaMax - SistolicaMin + 1);
  Diastolica := DiastolicaMin + Random(DiastolicaMax - DiastolicaMin + 1);
  
  Result := Format('%d/%d mmHg', [Sistolica, Diastolica]);
end;

function TFakeDataGenerator.GerarMedicamento: string;
begin
  Result := FMedicamentos[Random(FMedicamentos.Count)];
end;

function TFakeDataGenerator.GerarEspecialidadeMedica: string;
begin
  Result := FEspecialidades[Random(FEspecialidades.Count)];
end;

function TFakeDataGenerator.GerarPlanoSaude: string;
var
  Plano: string;
  TipoPlano: string;
begin
  Plano := FPlanosSaude[Random(FPlanosSaude.Count)];
  
  // Adicionar tipo/categoria do plano
  case Random(5) of
    0: TipoPlano := 'Básico';
    1: TipoPlano := 'Essencial';
    2: TipoPlano := 'Premium';
    3: TipoPlano := 'Master';
    4: TipoPlano := 'Executivo';
  end;
  
  if Random(2) = 0 then
    Result := Format('%s %s', [Plano, TipoPlano])
  else
    Result := Plano;
end;

{ Dados Acadêmicos }
function TFakeDataGenerator.GerarNomeInstituicaoEnsino: string;
begin
  Result := FInstituicoesEnsino[Random(FInstituicoesEnsino.Count)];
end;

function TFakeDataGenerator.GerarCursoGraduacao: string;
begin
  Result := FCursosGraduacao[Random(FCursosGraduacao.Count)];
end;

function TFakeDataGenerator.GerarAreaFormacao: string;
begin
  Result := FAreasFormacao[Random(FAreasFormacao.Count)];
end;

function TFakeDataGenerator.GerarMatriculaAcademica: string;
var
  AnoBase: Integer;
  NumeroSequencial: Integer;
  Digito: Integer;
  Matricula: string;
  Soma: Integer;
  i: Integer;
begin
  // Formato comum: AAANNNNND
  // AA: ano de ingresso (últimos 2 dígitos)
  // NNNNN: número sequencial (5 dígitos)
  // D: dígito verificador

  // Define o ano base (de 5 anos atrás até o ano atual)
  AnoBase := YearOf(Date) - Random(6);
  
  // Gera número sequencial
  NumeroSequencial := 10000 + Random(90000); // 5 dígitos: 10000 a 99999
  
  // Forma a matrícula base
  Matricula := Copy(IntToStr(AnoBase), 3, 2) + IntToStr(NumeroSequencial);
  
  // Calcula dígito verificador (soma ponderada)
  Soma := 0;
  for i := 1 to Length(Matricula) do
    Soma := Soma + StrToInt(Matricula[i]) * i;
  
  Digito := Soma mod 10;
  
  // Adiciona o dígito verificador
  Result := Matricula + IntToStr(Digito);
end;

function TFakeDataGenerator.GerarCoeficienteRendimento: Double;
var
  Media, DesvioPadrao: Double;
  CR: Double;
begin
  // A maioria dos alunos tem CR entre 6.0 e 8.5
  Media := 7.25;
  DesvioPadrao := 1.0;
  
  // Gerar usando distribuição normal
  repeat
    // Box-Muller transform para gerar distribuição normal
    CR := Media + DesvioPadrao * Sqrt(-2 * Ln(Random + 0.000001)) * 
          Cos(2 * PI * Random);
  until (CR >= 0) and (CR <= 10);
  
  // Arredondar para 2 casas decimais
  Result := RoundTo(CR, -2);
end;

function TFakeDataGenerator.GerarDataFormatura(AnoInicio: Integer = 0): TDateTime;
var
  AnoBase, MesBase, DiaBase: Word;
  DuracaoCurso: Integer;
  DataBase: TDateTime;
begin
  // Se não foi informado o ano de início, gera aleatoriamente
  if AnoInicio = 0 then
    AnoInicio := YearOf(Date) - Random(10) - 3; // Entre 3 e 12 anos atrás
  
  // Duração típica do curso (em anos)
  case Random(6) of
    0, 1, 2: DuracaoCurso := 4; // 50% de chance: cursos de 4 anos (maioria)
    3, 4: DuracaoCurso := 5;    // 33% de chance: cursos de 5 anos (engenharias, etc)
    5: DuracaoCurso := 6;       // 17% de chance: cursos de 6 anos (medicina, etc)
  end;
  
  // Adiciona uma variação para considerar possíveis atrasos ou adiantamentos
  if Random(10) < 7 then // 70% chance de ter atraso
    DuracaoCurso := DuracaoCurso + Random(2); // Adiciona até 1 ano de atraso
  
  // Define o ano de formatura
  AnoBase := AnoInicio + DuracaoCurso;
  
  // A maioria das formaturas ocorre em dezembro ou julho
  if Random(100) < 70 then
    MesBase := 12 // 70% dezembro
  else
    MesBase := 7; // 30% julho
  
  // Dia típico de formatura (15 a 30 do mês)
  DiaBase := 15 + Random(16);
  
  // Cria a data completa
  Result := EncodeDate(AnoBase, MesBase, DiaBase);
end;

function TFakeDataGenerator.GerarTituloMonografia: string;
const
  Inicios: array[0..14] of string = (
    'Análise de', 'Estudo sobre', 'Uma abordagem sobre', 'Investigação de', 
    'Perspectivas de', 'Desenvolvimento de', 'O impacto de', 'Avaliação de',
    'Contribuições para', 'Aspectos de', 'Reflexões sobre', 'Aplicação de',
    'Metodologia para', 'Os desafios de', 'Tendências em'
  );
  
  Temas: array[0..14] of string = (
    'sustentabilidade', 'inteligência artificial', 'tecnologias emergentes',
    'gestão de processos', 'comportamento organizacional', 'saúde pública',
    'comunicação digital', 'desenvolvimento sustentável', 'políticas públicas',
    'educação inclusiva', 'metodologias ativas', 'mercado financeiro',
    'sistemas integrados', 'inovação tecnológica', 'análise de dados'
  );
  
  Contextos: array[0..14] of string = (
    'no contexto brasileiro', 'em ambientes corporativos', 'na sociedade contemporânea',
    'no cenário atual', 'em instituições públicas', 'em organizações de saúde',
    'no setor educacional', 'na indústria 4.0', 'em economias emergentes',
    'no comércio eletrônico', 'na tomada de decisão', 'em projetos sociais',
    'nas relações internacionais', 'em pequenas e médias empresas', 'na transformação digital'
  );
var
  Titulo: string;
begin
  // Estrutura básica do título: Início + Tema + Contexto
  // Ex: "Análise de inteligência artificial no cenário atual"
  Titulo := Inicios[Random(15)] + ' ' + Temas[Random(15)] + ' ' + Contextos[Random(15)];
  
  // Em 30% dos casos, adiciona um subtítulo
  if Random(10) < 3 then
    Titulo := Titulo + ': um estudo de caso';
  
  Result := Titulo;
end;

end.

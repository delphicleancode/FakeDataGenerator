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

    function GerarDigitosCPF(const Digits: string): string;
    function GerarDigitosCNPJ(const Digits: string): string;
    function GerarDigitosModulo11(const Digits: string; Peso: Integer): string;
    function ApenasNumeros(const Str: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function GerarCPF(Formatado: Boolean = True): string;
    function GerarCNPJ(Formatado: Boolean = True): string;
    function GerarTelefone(Formatado: Boolean = True): string;
    function GerarCelular(Formatado: Boolean = True): string;
    function GerarCNH: string;
    function GerarNome(Feminino: Boolean = False): string;
    function GerarNomeCompleto(Feminino: Boolean = False): string;
    function GerarRazaoSocial: string;
    function GerarNomeFantasia(const RazaoSocial: string): string;
    function GerarLogradouro: string;
    function GerarNumero: string;
    function GerarComplemento: string;
    function GerarBairro: string;
    function GerarCidade: string;
    function GerarUF: string;
    function GerarCEP(Formatado: Boolean = True): string;
    function GerarEmail(const Nome: string): string;
    function GerarData(DataInicial, DataFinal: TDateTime): TDateTime;
    function GerarValor(ValorMinimo, ValorMaximo: Double): Double;
    function GerarDocumento: string;
    function GerarInscricaoEstadual(UF: string): string;
    function GerarPIS(Formatado: Boolean = True): string;
    function GerarRENAVAM: string;
    function GerarPlacaVeiculo(Mercosul: Boolean = False): string;
    function GerarCartaoCredito(Bandeira: string = ''): string;
    function GerarContaBancaria(Banco: string = ''; Formatado: Boolean = True): string;
    function GerarTituloEleitor(Formatado: Boolean = True): string;
    function GerarRG(Formatado: Boolean = True): string;

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

end.

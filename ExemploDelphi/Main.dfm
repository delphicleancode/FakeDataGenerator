object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'Fake Data Generator Demo'
  ClientHeight = 700
  ClientWidth = 916
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object lblTitulo: TLabel
    Left = 249
    Top = 8
    Width = 417
    Height = 28
    Caption = 'Gerador de Dados Fict'#237'cios para sistemas'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -20
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object memDadosFake: TMemo
    Left = 29
    Top = 42
    Width = 857
    Height = 591
    Lines.Strings = (
      '')
    TabOrder = 0
  end
  object btnGerar: TButton
    Left = 320
    Top = 648
    Width = 217
    Height = 33
    Caption = 'Gerar'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = btnGerarClick
  end
end

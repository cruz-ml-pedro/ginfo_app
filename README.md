
<h1>
Em andamento…
</h1>

## Shiny app - coleta e apresenta informações do Google Trends e da página de pesquisa do google

### O que o app faz:

- Primeira aba
  - emula as funcionalidades da página do google trends
  - previsão do volume de busca para as próximas horas, dias ou mêses
    (prophet package)
  - análise das componentes - diarias, semanais, anuais e trend
  - análise espectral dos dados temporais
- Segunda aba
  - emula a função de comparação do google trends para duas palavras
    chave por vez
  - análise de correlação
  - análise de coerência espectral (wavelets)
- Terceira aba
  - coleta informações dos resultados de buscas no google
    - títulos, texto descritivo e link
  - word cloud
  - lista com frequência de cada palavra
  - correlação entre palavras
  - opção de upload de lista de palavras (.txt) para exclusão durante a
    análise de correlação

### Próximos passos:

- reestruturar o app usando módulos
- criar mais funções
- melhorar o layout
- criar os relatórios (botão de download) para cada aba
- tornar os controles para o forecast responsivos de acordo com a
  escolha do usuário
- remover o mapa de NA nos resultados comparativos
- criar uma imagem (docker) para o app

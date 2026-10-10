# Project Checkpoint — IdentiFlight audit (BSH, DGY, Zarafshan)

_Ponto de restauro técnico. Lê isto primeiro numa sessão nova antes de continuar o trabalho. Estado em `main` @ 58692eb (2026-10-07)._

## 1. Objetivo do projeto

Pipeline R que audita o sistema IdentiFlight (IDF) dos parques eólicos da ACWA (Bash WPP = BSH, DGY, Zarafshan = ZRF) — disponibilidade do sistema, eficácia da resposta a curtailments (paragem automática de turbinas por deteção de aves), e investigação de fatalidades de aves prioritárias — para apoiar relatórios técnicos e ferramentas de decisão operacional que equilibram proteção de aves e produção de energia.

Três linhas de trabalho, com lançadores separados:

| Linha | Script principal | Lançadores | Template do relatório |
|---|---|---|---|
| Anual BSH/DGY | `IDF_analysis.R` | `run_annual_analysis_BSH.R`, `run_annual_analysis_DGY.R` | `report/report_template.rmd` |
| Mensal BSH/DGY | `IDF_monthly_report.R` | `run_monthly_report_BSH.R`, `run_monthly_report_DGY.R` | `report/monthly_report_template.rmd` |
| Incidente de fatalidade (ZRF) | `run_incident_zrshan.R` | `run_incident_zrshan_T94_20261001.R`, `run_incident_zrshan_T23_20260503.R` | `report/incident_report_template.rmd` |

**Regional assessment (ZRF + BSH + DGY)** — `run_regional_assessment.R` (script autónomo, lê TODOS os tracks dos 3 parques via os respetivos userSettings, `data-raw` + pasta remota) + `R/regional_assessment.R` (funções) + `tests/test_regional_assessment.R`. (1) Padrão anual: máximo semanal do mínimo de indivíduos em bins de 2 min, Egyptian-Vulture e Steppe-Eagle; (2) antecipação ZRF→BSH/DGY (outono) e BSH/DGY→ZRF (primavera): timing da passagem (onset/mediana/fim), correlação cruzada defasada, pulsos seguidos no outro parque. Dias sem tracks não são 0: dia só conta como monitorizado com `min_tracks_per_day` tracks; semanas com poucos dias monitorizados ficam NA. Settings próprios em `inputs/userSettings_regional_assessment.R` (só leitura de tracks — padrões `TrackReport_Default.*<ZRF|BSH|DGY>`, `ini`/`end` comuns, 2 espécies, bins de 2 min e distância de fusão 200 m; cache própria `cache/regional/`); parâmetros da análise (épocas, lags, pulsos, esforço) no bloco "PARAMETROS DA ANALISE" do script. Outputs em `outputs/<AAAAMMDD>_REGIONAL/` com sufixo `_REGIONAL_<AAAAMMDD>`. Pendente: pasta de rede de ZRF (`databases_dir_alt` de ZRF no userSettings, hoje `NA`).

Cada lançador define `project_settings_file` (em `inputs/`) antes de dar `source` ao script principal — nunca copiar linhas de um lançador para dentro do script principal.

## 2. Convenções críticas de trabalho (não esquecer)

- **Ambiente Claude**: por omissão sem R instalado (validação por leitura cuidadosa + traço manual); nalgumas sessões `apt-get install r-base-core r-cran-data.table r-cran-ggplot2 r-cran-lubridate r-cran-scales` funciona e permite correr os testes sintéticos (`fst`/`janitor`/`writexl` NÃO ficam disponíveis — CRAN bloqueado — por isso a leitura dos brutos e a escrita xlsx só se testam com stubs). O Paulo corre sempre o pipeline real no RStudio. Também sem LibreOffice funcional — `.docx`/`.xlsx` são validados estruturalmente (`unzip`, `pandoc`), não visualmente.
- **Git**: o Paulo trabalha localmente (RStudio); Claude trabalha numa branch `claude/...` atribuída pela sessão. No fim de uma tarefa: `commit+push` na branch claude, o Paulo faz `git pull` dessa branch e, quando validado, o merge para `main` (fast-forward quando `main` é antepassada). Branches claude antigas podem ser apagadas depois do merge. Confirmar `git status`/`git log` e `git fetch` no início de cada sessão — o Paulo pode ter commitado entretanto na mesma branch (o `push` é recusado e resolve-se com `fetch` + `merge`).
- **`outputs/`** está no `.gitignore` normalmente — o Paulo força a adição (`git add -f`) da pasta de uma corrida quando necessário, excluindo `coverage_3d/` (muito pesada).
- **`CLAUDE.md`** (raiz do repo) tem as regras de estilo para relatórios técnicos (factual, sem comentar mudanças entre versões, texto uniforme dentro de cada secção, metodologia forense de fatalidades por tracks e não por log de curtailments, `incident_date` = data em que a carcaça foi encontrada), a regra de que todos os xlsx exportados são inteiramente em inglês, a regra de e-mails em inglês na voz do Paulo, e a de nomes `_test` para objetos sintéticos em `tests/`. Ler antes de escrever qualquer relatório.
- **Comentários e mensagens de consola em português**; texto do relatório e xlsx em inglês.
- **Padrão recorrente — fuso horário**: `as.Date()` sem `tz=` usa UTC e, em Asia/Samarkand (UTC+5), uma meia-noite local cai no dia anterior. Usar sempre `as.Date(x, tz = proj_timezone)`. Corrigido em `run_incident_zrshan.R`, `IDF_analysis.R` e `IDF_monthly_report.R` (`report_start`/`report_end`). Também `writexl::write_xlsx()` perde `tzone` — usar `write_xlsx_local()`.

## 3. Arquitetura do código R

**Pacotes principais**: data.table, dplyr/tidyverse, lubridate, sf, ggplot2, readxl, writexl, fst, suncalc, plotly, terra/RANN (coverage 3D), flextable/rmarkdown (relatórios), webshot2 (PNG dos plots 3D; precisa de Chrome/Edge — a mensagem `websocketpp ... End of File` do `chromote` no fim é inofensiva).

**Ficheiros `R/`** (cada um com cabeçalho próprio a documentar uso/dependências):

| Ficheiro | Função |
|---|---|
| `read_*.R`, `read_utils.R`, `data_cache.R`, `write_utils.R`, `dataset_summary.R` | Leitura dos 4 datasets brutos (`force_tz()`, não `with_tz()`), cache `fst`, `write_xlsx_local()` |
| `output_paths.R` | Pasta e sufixo de ficheiros por incidente: `make_incident_tag()`, `incident_output_folder()`, `out_name()` (ver secção 5) |
| `curtailment_response*.R`, `curtailment_shutdown_time.R`, `curtailment_safe_distance.R`, `curtailment_forensic_trace.R`, `curtailment_short_track.R`, `curtailment_removal_risk.R`, `curtailment_species.R`, ... | Resposta/latência a curtailments, tempo de paragem, distância segura (KNE), reconstrução forense por turbina+dia, curtailments de tracks curtos |
| `id_transitions.R` | Transições de classificação de espécie dentro do mesmo `track_id` (P→NP, NP→P, risco "late") |
| `availability_daylight.R`, `offline_curtailment_check.R` | Disponibilidade das unidades IDF em horas de luz; classificação de gaps de heartbeat por evidência (curtailment/SCADA); `summarise_net_availability()` (percentagens positivas nunca arredondadas a 0) |
| `coverage_3d_topography.R` | Cobertura 3D com topografia (DEM); `save_coverage_3d_plots(..., file_suffix)`; métrica de baixo relevo (`pct_low_terrain_covered`, `.flag_low_terrain()`, `low_terrain_levels = 2`: os 2 níveis de malha mais baixos acima do terreno por coluna x/y) |
| `turbine_idf_coverage.R` | Matriz turbina↔IDF geométrica (buffers) + comparação com matriz manual ACWA |
| `fatality_track_investigation.R`, `fatality_window_analysis.R` | Tracks candidatos a colisão por incidente (janela de dias); disponibilidade/resposta na janela vs. baseline global; abundância pré/pós |
| `track_min_individuals.R` | Nº mínimo de indivíduos por bin de 2 min; `plot_daily_max_individuals(..., x_text_size)` |
| `turbine_recent_activity.R`, `turbine_*`, `track_*`, `curtailment_*cluster*`, `bio_flight_metrics.R` | Atividade recente, clusters espaciais, risco por espécie, métricas de voo |
| `monthly_report_utils.R`, `monthly_technical_summary.R`, `report.R` | Utilitários do relatório mensal; `build_idf_report()` (render com `reference_docx` da empresa) |

`tests/test_*.R` — um por módulo principal, dados sintéticos com resultado calculado à mão, sem `testthat`; objetos sintéticos com sufixo `_test`.

## 4. Settings por linha de trabalho (`inputs/`)

- **BSH anual** (`userSettings_BSH.R`): `ini = 2025-01-01`, `end = 2026-08-30`; `turbinas_scada = c('BSH54','BSH62','BSH14')`; `fatality_incidents`: BSH_0002, BSH_0004, BSH_0012. **DGY**: `userSettings_DGY.R`. **Mensal**: `monthlyReportSettings_BSH.R`/`_DGY.R` (`ini`/`end` vêm de `report_month_bounds`).
- **ZRF (um incidente por settings file)**: `userSettings_ZRF.R` (T35, 2026-05-11), `userSettings_ZRF_T35_20260503.R`/`userSettings_ZRF_T23_20260503.R` (T23, 2026-05-03), `userSettings_ZRF_T94_20261001.R` (T94, Egyptian-Vulture, 2026-10-01, `incident_id = "ZRF_Oct2026"`, `ini = 2026-09-01`, `end = 2026-10-01`, IDF60/58/53/66). `farm_code = "ZRF"`; a cache `cache/ZRF/` é partilhada entre incidentes.
- `proj_timezone = "Asia/Samarkand"` em todos.

## 5. Relatório de incidente (ZRF) — estado atual

**Estrutura do relatório** (`incident_report_template.rmd`, alinhada com o índice apresentado aos Lenders em Zarafshan): Performance/Accuracy (ID Transitions) → Efficacy ("Not covered in this report" + definição) → Effectiveness (IDF Unit Availability — janela vs. baseline, Short-Track Curtailments, Curtailment Response Time & Latency, Shutdown Time, Turbine/IDF Unit Coverage + 3D Coverage) → Fatality Investigation (Candidate Tracks by Signal, Top Candidate Tracks + exemplos, Curtailment Response Around the Incident, Abundance Before/After, `<espécie>` Activity) → Annexes.

**Espaçamento**: um parágrafo `&nbsp;` entre qualquer bloco texto/tabela/figura onde um dos lados é tabela ou figura (texto-texto já tem espaçamento do estilo); títulos a negrito de tabela/figura ficam colados ao que nomeiam; `annex_note()` acrescenta o espaço antes da nota "full table is available in the accompanying workbook".

**Outputs por incidente** (`R/output_paths.R`): `incident_tag = <farm_code>_<turbina>_<data do incidente AAAAMMDD>` (ex: `ZRF_T94_20261001`); pasta `outputs/<AAAAMMDD de execução>_<incident_tag>/`; todos os ficheiros (xlsx, docx, HTML/PNG de `coverage_3d/`) com sufixo `_<incident_tag>_<AAAAMMDD de execução>`; repetir o mesmo incidente no mesmo dia sobrescreve a própria pasta. Os nomes impressos nas notas de cada tabela vêm da mesma função, por isso coincidem com os ficheiros gravados. Validado numa corrida real do T94.

**Pontos a ter presentes**
- `report_start`/`report_end`/janela de investigação impressas com `as.Date(..., tz = proj_timezone)`.
- Tabela de cobertura 3D mostra as colunas de baixo relevo (também no relatório anual).
- Gráfico Daily Peak do incidente: `date_breaks = "1 day"`, `x_text_size = 6`.
- Chunks do template que usam `cat()` num ramo `else` têm `results='asis'` (evita mensagens impressas como código).

## 6. Estado do GitHub

- `main` @ 58692eb contém tudo o descrito acima (fast-forward a partir de `ccdf1e1`, 2026-10-07). As branches `claude/keen-fermat-4grurh` e `claude/idf-bsh-dgy-reports-wvz611` estão no mesmo commit ou atrás de `main` — podem ser avançadas para `main` ou apagadas.
- Esta lista pode ficar desatualizada: confirmar com `git fetch` + `git log` no início da próxima sessão.

## 7. ⚠️ Deliverables externos — aviso de efemeridade

Ficheiros gerados em `/tmp/` (ex: relatório Word `build_report_en.js`, matriz de decisão do protocolo de outages `build_decision_matrix.py` + CSVs de apoio) não sobrevivem ao fim da sessão — só o repositório git é persistente. Se for preciso regenerar algum deles, os scripts-fonte terão de ser recriados; considerar movê-los para o repo (ex: pasta `tools/`) para deixarem de depender do `/tmp`.

## 8. Próximos passos / itens em aberto

- **Imediato**: o Paulo corre o incidente T23 (`run_incident_zrshan_T23_20260503.R`) com o código novo e confirma: pasta `outputs/<data>_ZRF_T23_20260503/`, sufixos nos ficheiros, datas do relatório corretas (início da janela = dia seguinte ao que aparecia antes), gráfico Daily Peak com datas diárias legíveis, percentagens da Unavailability Summary diferentes de 0.
- Decidir se as pastas/sufixos por execução (e a ordem de capítulos) se estendem aos relatórios BSH/DGY — hoje só o relatório de incidente os usa.
- `summarise_net_availability_by_idf()` (`R/offline_curtailment_check.R`, linha ~438) ainda arredonda `net_offline_pct` a 1 casa decimal (pode dar 0 para valores muito pequenos) — decidir se alinha com `summarise_net_availability()`.
- Validar os limiares da folha `Assumptions` da matriz de decisão do protocolo de outages com a equipa de biodiversidade ACWA.
- Revisitar a secção "performance vs. fenologia" quando o SCADA cobrir uma época de migração completa.

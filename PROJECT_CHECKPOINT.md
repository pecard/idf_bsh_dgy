# Project Checkpoint — Bash WPP IdentiFlight audit

_Ponto de restauro técnico. Gerado no fim de uma sessão longa, por falta de espaço de contexto. Lê isto primeiro numa sessão nova antes de continuar o trabalho._

## 1. Objetivo do projeto

Pipeline R que audita o sistema IdentiFlight (IDF) do parque eólico Bash WPP (ACWA) — disponibilidade do sistema, eficácia da resposta a curtailments (paragem automática de turbinas por deteção de aves), e investigação de fatalidades de aves prioritárias — para apoiar relatórios técnicos e ferramentas de decisão operacional que equilibram proteção de aves e produção de energia.

## 2. Convenções críticas de trabalho (não esquecer)

- **Ambiente Claude sem R instalado**: todo o código R é validado por leitura cuidadosa + traço manual de casos sintéticos (nunca execução real). Também sem LibreOffice funcional (não converte para PDF nem recalcula xlsx) — ficheiros `.docx`/`.xlsx` são validados estruturalmente via `unzip -l`/`unzip -p ... | grep`, não visualmente.
- **Git dual-branch**: Paulo trabalha em `main` (RStudio, local). Claude trabalha em `claude/repo-access-yhcwim`. Workflow repetido a cada alteração: `commit+push` na branch claude → `checkout main` → `pull` → `merge claude branch` → `push` → `checkout claude branch` → `fetch+merge origin/main` → `push`. As duas branches devem ficar sempre sincronizadas no fim de cada tarefa.
- **`outputs/`** está no `.gitignore` normalmente — o Paulo força a adição (`git add -f`) da pasta do dia depois de cada corrida do `IDF_analysis.R`, excluindo `coverage_3d/` (muito pesada, não usada nos relatórios).
- **`CLAUDE.md`** (raiz do repo) tem as regras de estilo para relatórios técnicos (não comentar mudanças entre versões, lettering uniforme, metodologia forense de fatalidades por tracks não por log de curtailments, incident_date = data de deteção não de morte). Ler antes de escrever qualquer relatório.

## 3. Arquitetura do código R

**Pacotes principais**: data.table, dplyr/tidyverse, lubridate, sf, ggplot2, readxl, writexl, fst, suncalc, plotly, terra/RANN (coverage 3D).

**Ficheiros `R/`** (cada um com cabeçalho próprio a documentar uso/dependências):

| Ficheiro | Função |
|---|---|
| `read_tracks.R`, `read_curtailments.R`, `read_scada.R`, `read_heartbeats.R`, `read_utils.R` | Leitura dos 4 datasets brutos — usam `force_tz()` (não `with_tz()`), dados já vêm em hora local do portal IdentiFlight |
| `data_cache.R` | Cache `fst` dos 4 datasets grandes, com fix de perda de `tzone` |
| `write_utils.R` | `write_xlsx_local()` — fix de perda de `tzone` ao exportar para xlsx (ver secção 4) |
| `curtailment_response.R` | `assess_curtailment_response()`, `classify_curtailment_response()` |
| `curtailment_shutdown_time.R` | `time_to_rpm_thresholds()` |
| `curtailment_safe_distance.R` | `compute_safe_distance()` (metodologia KNE) |
| `curtailment_response_classify.R` | `classify_response_flag()` (missed/no_data/delayed/ok, ver secção 4) + `summarise_response_by_flag()` |
| `curtailment_response_timeline.R` | Evolução temporal farm-wide (semanal) de missed/delayed + sobreposição com abundância |
| `curtailment_forensic_trace.R` | Reconstrução forense por turbina+dia (plot RPM estilo portal IdentiFlight) |
| `availability_daylight.R` | Disponibilidade das unidades IDF em horas de luz do dia |
| `data_coverage.R` | Cobertura temporal/lacunas nos 4 datasets |
| `coverage_3d_topography.R` | Cobertura 3D com topografia (DEM) |
| `track_min_individuals.R` | Nº mínimo de indivíduos por bin de 2min farm-wide (+ timeline diária/semanal + auditoria de picos) |
| `turbine_idf_coverage.R` | Matriz turbina↔IDF geométrica (buffers) + comparação com matriz manual ACWA |
| `turbine_recent_activity.R` | Atividade recente de aves por turbina (novo — alimenta a matriz de decisão do protocolo de outages) |
| `fatality_track_investigation.R` | Tracks candidatos a colisão por incidente de fatalidade (janela de dias) |
| `fatality_window_analysis.R` | Disponibilidade + resposta a curtailments na janela do incidente, global vs. janela, + abundância pré/pós |
| `dataset_summary.R` | Sumário de nº de linhas/período por dataset (para contexto de relatórios) |

`tests/test_*.R` — um por módulo principal, dados sintéticos com resultado calculado à mão, sem `testthat`.

## 4. Settings atuais (`inputs/userSettings_BSH.R`)

- `ini = 2025-01-01`, `end = 2026-08-15`
- `run_sections`: curtailment_response / fatality_investigation / coverage_3d / min_individuals = todos `TRUE`
- `turbinas_scada = c('BSH54','BSH62','BSH14')`
- `fatality_incidents`: BSH_0002 (BSH54, Steppe-Eagle, 2025-10-31), BSH_0004 (BSH62, Egyptian-Vulture, 2026-03-19), BSH_0012 (BSH14, Egyptian-Vulture, 2026-08-03) — `days_before=8` cada
- `track_proximity_threshold_m=100`, `min_individuals_merge_dist_m=200`/`bin_min=2`, `response_timeline_unit="week"`, `recent_activity_days=14`
- `turbine_idf_matrix_filename = "ACWA_IDF_Coverage_Matrix.xlsx"` (em `inputs/`)

## 5. Últimas alterações (mais recentes primeiro)

- **Matriz de decisão Excel** para o `IDF_Response_Protocol_20260727.docx` (protocolo de resposta a outages do IdentiFlight, documento externo) — script Python standalone, **não está no repo git** (ver secção 7, aviso de efemeridade)
- **2 bugs de fuso horário corrigidos** (padrão recorrente neste projeto — atenção a isto no futuro):
  - `writexl::write_xlsx()` não preserva `tzone` → timestamps saíam 5h atrasados nos `.xlsx`. Fix: `write_xlsx_local()` em `write_utils.R`, usado em todas as chamadas do `IDF_analysis.R`.
  - `as.Date()` sem `tz=` usa UTC por omissão → limites de dia/semana podiam saltar 1 dia perto da meia-noite local. Fix em `availability_daylight.R`, `data_coverage.R`, `curtailment_response_timeline.R`, `fatality_window_analysis.R`.
- **Bug `no_data` vs `missed`**: `classify_response_flag()` juntava os dois na mesma categoria "missed", inflacionando essa contagem. Corrigido — agora 4 categorias (missed/no_data/delayed/ok) + `pct_of_known` (exclui no_data) adicionado a `summarise_response_by_flag()`.
- **Bug `bind_rows()`** em `read_curtailments.R` — um ficheiro com a coluna `turbine` totalmente vazia era lido como `logical` em vez de `character`, partindo o `bind_rows()`. Fix: `as.character()` forçado em `turbine`/`track_id`/`species`.
- **Relatório Word reescrito**: sem secção "dia do incidente" (metodologia agora foca só na janela de 8 dias), cobre os 3 incidentes (não só 2), nova secção "performance vs. fenologia" (sem sinal claro ainda — pico histórico de Steppe-Eagle é anterior ao início do SCADA).
- Módulos mais antigos desta sessão (por ordem): `run_sections` (switches de análise), `turbine_idf_coverage.R` (matriz geométrica), `track_min_individuals.R` (contagem mínima de indivíduos), `curtailment_response_timeline.R` (fenologia vs. performance), `fatality_window_analysis.R` (janela vs. global).

## 6. Status do GitHub

- Branch de trabalho: `claude/repo-access-yhcwim`. Branch do Paulo: `main`.
- No fim da última tarefa (matriz de decisão), o código R (`turbine_recent_activity.R` + secção 8 do `IDF_analysis.R`) já estava commitado e sincronizado nas duas branches — **confirmar com `git status`/`git log` no início da próxima sessão**, já que este ficheiro de checkpoint só será commitado depois deste ponto.
- O script Python da matriz de decisão (`/tmp/idf_report/build_decision_matrix.py`) **NÃO está no git** — só existe no filesystem efémero da sessão anterior (ver secção 7).

## 7. ⚠️ Deliverables externos — aviso de efemeridade

Ficheiros gerados em `/tmp/idf_report/` (relatório Word `build_report_en.js`, matriz de decisão `build_decision_matrix.py` + CSVs de apoio) **não sobrevivem ao fim da sessão** — só o repositório git é persistente. Ambos os ficheiros finais (`.docx` e `.xlsx`) já foram enviados ao Paulo via `SendUserFile`, mas se ele quiser voltar a editar/regenerar qualquer um deles numa sessão nova, os scripts-fonte terão de ser recriados — **considerar mover estes scripts para o repo** (ex: pasta `reports/` ou `tools/`) numa próxima sessão, para deixarem de depender do `/tmp`.

## 8. Último bloco de código atualizado

`IDF_analysis.R`, secção 8 (nova):
```r
##
## 8. Turbine recent activity (apoio a matriz de decisao do protocolo de
##    resposta a outages do IdentiFlight) ----
##
source("R/turbine_recent_activity.R")

turbine_recent_activity_dt <- summarise_turbine_recent_activity(
  track_dt, wtg, species = prioritysp,
  date_from = end - lubridate::days(recent_activity_days), date_to = end,
  proximity_threshold_m = track_proximity_threshold_m
)

write_xlsx_local(
  list(Turbine_recent_activity = turbine_recent_activity_dt),
  file.path(folder_output, "turbine_recent_activity.xlsx")
)
```

## 9. Próximos passos / itens em aberto

- **Imediato**: correr `IDF_analysis.R` de novo para gerar `turbine_recent_activity.xlsx` (novo, secção 8) e popular a folha `Raw_Recent_Activity` da matriz de decisão (atualmente vazia).
- Validar os limiares da folha `Assumptions` da matriz de decisão com a equipa de biodiversidade ACWA (não estão definidos no protocolo original, são propostas a confirmar).
- `fatality_track_investigation.xlsx`: análise aprofundada ainda pendente (ficou "reservada para mais tarde" numa fase anterior da sessão).
- Revisitar a secção "performance vs. fenologia" do relatório quando o SCADA cobrir uma época de migração completa (atualmente sem sobreposição com o maior pico histórico de Steppe-Eagle).
- Considerar mover os scripts de geração de relatório/matriz de decisão para dentro do repo (ver secção 7).

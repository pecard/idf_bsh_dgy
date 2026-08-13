##
## Teste com dados simulados para R/data_cache.R
##
## Verifica load_or_read_cache(): 1a chamada le e grava; 2a chamada (mesma
## cache, force_reread=FALSE) tem de usar a cache SEM voltar a chamar
## read_fn(); force_reread=TRUE tem de voltar a chamar read_fn() e regravar.
##
## Correr: source("tests/test_data_cache.R")
##
## Depende de: data.table, fst
##

source("R/data_cache.R")

cache_file <- file.path(tempdir(), "test_data_cache.fst")
if (file.exists(cache_file)) file.remove(cache_file)

n_calls <- 0L

## read_fn muda o conteudo a cada chamada (via n_calls), para conseguirmos
## detetar se load_or_read_cache() esta mesmo a evitar chama-la quando ha cache
make_read_fn <- function() {
  function() {
    n_calls <<- n_calls + 1L
    data.table(id = 1:3, value = n_calls) # value = numero da chamada, muda sempre
  }
}


## 1a chamada -- sem cache -- tem de chamar read_fn() e gravar o ficheiro ----
dt1 <- load_or_read_cache(cache_file, make_read_fn(), force_reread = FALSE)

cat("\n===== 1a chamada (sem cache previa) =====\n")
cat(sprintf("n_calls: %d (esperado: 1) -- %s\n", n_calls, if (n_calls == 1L) "OK" else "FALHOU"))
cat(sprintf("ficheiro de cache criado: %s (esperado: TRUE) -- %s\n",
           file.exists(cache_file), if (file.exists(cache_file)) "OK" else "FALHOU"))
cat(sprintf("dt1$value[1]: %d (esperado: 1) -- %s\n",
           dt1$value[1], if (dt1$value[1] == 1L) "OK" else "FALHOU"))


## 2a chamada -- cache existe, force_reread=FALSE -- NAO pode chamar read_fn() ----
dt2 <- load_or_read_cache(cache_file, make_read_fn(), force_reread = FALSE)

cat("\n===== 2a chamada (cache existe, force_reread=FALSE) =====\n")
cat(sprintf("n_calls: %d (esperado: 1, NAO deve ter aumentado) -- %s\n",
           n_calls, if (n_calls == 1L) "OK" else "FALHOU"))
cat(sprintf("dt2$value[1]: %d (esperado: 1, valor da 1a leitura, vindo da cache) -- %s\n",
           dt2$value[1], if (dt2$value[1] == 1L) "OK" else "FALHOU"))
cat(sprintf("dt2 identico a dt1: %s (esperado: TRUE)\n", isTRUE(all.equal(as.data.frame(dt1), as.data.frame(dt2)))))


## 3a chamada -- force_reread=TRUE -- TEM de chamar read_fn() e regravar ----
dt3 <- load_or_read_cache(cache_file, make_read_fn(), force_reread = TRUE)

cat("\n===== 3a chamada (force_reread=TRUE) =====\n")
cat(sprintf("n_calls: %d (esperado: 2, tem de ter aumentado) -- %s\n",
           n_calls, if (n_calls == 2L) "OK" else "FALHOU"))
cat(sprintf("dt3$value[1]: %d (esperado: 2, nova leitura) -- %s\n",
           dt3$value[1], if (dt3$value[1] == 2L) "OK" else "FALHOU"))


## 4a chamada -- confirma que a cache foi mesmo regravada com o novo conteudo ----
dt4 <- load_or_read_cache(cache_file, make_read_fn(), force_reread = FALSE)

cat("\n===== 4a chamada (cache regravada na 3a chamada, force_reread=FALSE) =====\n")
cat(sprintf("n_calls: %d (esperado: 2, NAO deve ter aumentado) -- %s\n",
           n_calls, if (n_calls == 2L) "OK" else "FALHOU"))
cat(sprintf("dt4$value[1]: %d (esperado: 2, cache atualizada na 3a chamada) -- %s\n",
           dt4$value[1], if (dt4$value[1] == 2L) "OK" else "FALHOU"))

file.remove(cache_file)
cat("\n(ficheiro de teste removido)\n")

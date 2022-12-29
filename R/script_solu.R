# Peixes Rio Madeira ------------------------------------------------------

## importanto dados
peixes <- readr::read_csv("dados/peixes_rio_madeira.csv")

## vislumbre do dataset

peixes |> tibble::view()

peixes |> tibble::glimpse()

## distribuição de frequência da variável `ordem`

peixes |>
  dplyr::group_by(ordem) |>
  dplyr::select(ordem) |>
  dplyr::count() |>
  dplyr::arrange(n)

## gráfico de barras

peixes |>
  dplyr::group_by(ordem) |>
  dplyr::select(ordem) |>
  dplyr::count() |>
  ggplot2::ggplot() +
  ggplot2::aes(x = n, y = forcats::fct_reorder(ordem, n)) +
  ggplot2::geom_col() +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    x = "",
    y = "",
    title = "Frequência da variável ordem"
  )

## Questão 02

peixes |>
  dplyr::group_by(ordem) |>
  dplyr::filter(bacia == "Rio Guaporé") |>
  dplyr::select(ordem, peso_g) |>
  tidyr::drop_na() |>
  dplyr::summarise(
    media_peso = mean(peso_g)
  )

peixes |>
  dplyr::group_by(ordem) |>
  dplyr::filter(bacia == "Rio Guaporé") |>
  dplyr::select(ordem, peso_g) |>
  tidyr::drop_na() |>
  dplyr::summarise(
    media_peso = mean(peso_g),
    desvio_p   = sd(peso_g),
    cv         = (desvio_p/ media_peso)*100
  ) |>
  dplyr::arrange(cv)

## Questão 03

peixes |>
  dplyr::distinct(sexo)

peixes_recode <- peixes |>
  dplyr::mutate(
    sexo_recode = dplyr::recode(
      sexo,
      "Fêmea" = "Fêmea",
      "Macho" = "Macho",
      "fêmea" = "Fêmea"
    )
  ) |>
  dplyr::filter(sexo_recode == "Macho" | sexo_recode == "Fêmea")

peixes_recode |>
  dplyr::count(sexo_recode)

peixes_recode |>
  dplyr::select(peso_g, sexo_recode) |>
  dplyr::arrange(desc(peso_g))

peixes_recode |>
  dplyr::group_by(bacia) |>
  dplyr::count(sexo_recode) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = sexo_recode, y = n) +
  ggplot2::geom_col() +
  ggplot2::facet_grid(.~bacia)

# Questão 04

peixes |>
  dplyr::group_by(bacia) |>
  dplyr::select(ordem, peso_g, cp_cm) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = peso_g/1000, y = cp_cm/100) +
  ggplot2::geom_point(
    ggplot2::aes(color = ordem), alpha = 0.5
  ) +
  ggplot2::facet_grid(bacia~.) +
  ggplot2::geom_smooth()

# Contracheque ------------------------------------------------------------
salarios <- readr::read_csv("dados/contracheque.csv")

salarios |> tibble::view()
salarios |> tibble::glimpse()

## Questão 05
salarios |>
  dplyr::select(rendimento_liquido) |>
  dplyr::arrange(desc(rendimento_liquido))

salarios |>
  dplyr::select(rendimento_liquido) |>
  tidyr::drop_na() |>
  dplyr::summarise(
    max_salario = max(rendimento_liquido)
  )

# Questão 06
salarios |>
  dplyr::select(rendimento_liquido) |>
  dplyr::filter(rendimento_liquido >= 39293.32) |>
  dplyr::count()

salarios |>
  dplyr::select(rendimento_liquido) |>
  dplyr::filter(rendimento_liquido >= 100000) |>
  dplyr::count()

salarios |>
  dplyr::group_by(tribunal) |>
  dplyr::summarise(
    media_salarios = mean(rendimento_liquido),
    dp_salarios    = sd(rendimento_liquido),
    cv_salarios    = (dp_salarios/media_salarios)*100
  ) |>
  dplyr::arrange(desc(cv_salarios)) |>
  dplyr::top_n(1) |>
  dplyr::select(tribunal)

# Prouni ------------------------------------------------------------------

notas_cursos <- readr::read_csv("dados/cursos-prouni.csv")

notas_cursos |> tibble::view()
notas_cursos |> tibble::glimpse()

## Questão 07
notas_cursos |>
  dplyr::group_by(turno) |>
  dplyr::select(turno, nota_integral_ampla) |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::aes(x = turno, nota_integral_ampla) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(
    title = "Comparação das notas por turno",
    x     = "",
    y     = ""
  )

notas_cursos |>
  dplyr::filter(turno == "Integral") |>
  dplyr::select(nota_integral_ampla) |>
  tidyr::drop_na() |>
  dplyr::summarise(
    media_notas = mean(nota_integral_ampla),
    mediana_notas = median(nota_integral_ampla)
  )

notas_cursos |>
  dplyr::group_by(turno) |>
  dplyr::select(nota_integral_ampla) |>
  tidyr::drop_na() |>
  dplyr::summarise(
    media_notas = mean(nota_integral_ampla),
    dp_notas    = sd(nota_integral_ampla),
    cv_notas    = (dp_notas/media_notas) * 100
  ) |>
  dplyr::arrange(desc(cv_notas))

notas_cursos |>
  dplyr::group_by(turno) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = nota_integral_ampla, fill = turno) +
  ggplot2::geom_density() +
  ggplot2::facet_grid(turno~.)

# Questão 08
notas_cursos |>
  dplyr::group_by(uf_busca) |>
  dplyr::select(uf_busca) |>
  dplyr::count(sort = TRUE)

notas_cursos |>
  dplyr::group_by(uf_busca) |>
  dplyr::select(uf_busca) |>
  dplyr::count() |>
  dplyr::arrange(desc(n))

# Questão 09
notas_cursos |>
  dplyr::distinct(nome) |>
  dplyr::count()

# Questão 10
notas_cursos |>
  dplyr::select(nome, nota_integral_ampla) |>
  tidyr::drop_na() |>
  dplyr::filter(nome == "Medicina" | nome == "Direito") |>
  ggplot2::ggplot() +
  ggplot2::aes(nota_integral_ampla, nome, fill = nome) +
  ggridges::geom_density_ridges(show.legend = FALSE) +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::labs(
    title = "Comparação entre Medicina e Direito",
    x = "",
    y = ""
  ) +
  ggplot2::theme_minimal()

# Generation Tracker

**Pairing age distribution data with lifetime experiences to get hints on dominant motivations and relative power of generations**

**Generation Tracker** is an analytical prototype for exploring how major historical events shape the demographic weight of different generations.

The idea is simple: many economic and cultural attitudes are partly formed by lived experience. People who were adults during a sovereign default, children during a war, or born after a political transformation may carry very different reference points. This project turns that intuition into a reproducible demographic tool.

![App screenshot](assets/screen.jpg)

The app lets a user construct questions such as:

-   How many people in a country were adults during a sovereign default?
-   What share of today’s population was school-aged when a war began?
-   How large is the generation that never lived under a certain political regime?
-   How do different historically exposed groups compare within one country?

The project combines demographic data, structured event histories, and transparent cohort logic. It does not claim to prove causal effects on behavior. Instead, it helps analysts quantify the demographic scale of plausible historical-experience hypotheses.

## Why this project exists

Many analytical workflows begin with a qualitative intuition: a historical event may have shaped the experience of a particular generation. But before using that intuition in economic analysis, it is useful to ask a simpler question: how large is that group, and how does its size change over time?

This project is an attempt to make that step explicit and reproducible. It turns informal cohort definitions into structured demographic calculations, helping analysts move from an interesting historical hypothesis to a chart, a dataset, and a documented set of assumptions.

The broader motivation is to reduce manual work in recurring analytical tasks while keeping the logic transparent enough for discussion, criticism, and revision.

## Core idea

A user builds a sentence-like query:

> Men in Russia who were school-aged when the war in Afghanistan began.

Internally, the app translates this into a reproducible recipe: country, sex, age range, event, event timing rule, and output metric. The result is a time series showing the size or population share of the selected cohort.

## Intended use

The app is designed for:

-   economists and analysts testing historical-experience hypotheses;
-   researchers preparing charts for reports or presentations;
-   data storytellers who need a transparent way to compare generations;
-   anyone interested in the demographic structure of collective memory.

## Status

This is an early-stage concept and development plan. The first version focuses on a clean analytical workflow, transparent assumptions, and reproducible exports rather than a large feature set.

## Data files and startup

The Shiny app expects a **prepared** population artifact, not raw WPP Excel at startup.

| File | Role |
|----|----|
| `data/population.rds` | **App input** — filtered to `countries.csv`, built offline. This is what `runApp()` loads by default. |
| `data/population_build_manifest.json` | Build metadata; when present and matching `population.rds`, the app skips the expensive duplicate-key validation pass. |
| `data/population_cache.rds` | **Offline cache** — full WPP Excel import while building prepared data. Do not point the app at this file for normal use. |
| WPP `*.xlsx` in `data/` | Source files for `scripts/build_prepared_population.R` only. |

**Build prepared data once** (after placing WPP Male/Female Excel and `countries.csv` in `data/`):

``` bash
Rscript scripts/build_prepared_population.R
```

By default `GEN_TRACKER_ALLOW_EXCEL_SOURCES=FALSE`, so missing `population.rds` fails fast with build instructions instead of a long Excel import at startup. Set `GEN_TRACKER_ALLOW_EXCEL_SOURCES=TRUE` only for deliberate one-off imports.

After changing compression or validation, rebuild or re-save `population.rds` (see [`docs/dev_workflow.md`](docs/dev_workflow.md) for startup profiling).

### Event and country dictionaries

| File | Role |
|----|----|
| `data/events.csv` | Event catalogue (years, scope, type). Required at startup. Override: `GEN_TRACKER_EVENTS_PATH`. |
| `data/countries.csv` | Countries included in analysis. Required at startup. Override: `GEN_TRACKER_COUNTRIES_PATH`. |
| `data/event_countries.csv` | **Event–country links** for national / multi-country compatibility. Recommended; without it, national events are not hard-blocked. Override: **`GEN_TRACKER_EVENT_COUNTRIES_PATH`**. |

Build links after `events.csv` and `countries.csv` are in place:

``` bash
Rscript scripts/build_event_countries.R
```

A committed schema example lives at [`data/event_countries.template.csv`](data/event_countries.template.csv). Full workflow, environment variables, and validation rules: [`docs/dev_workflow.md`](docs/dev_workflow.md).

## Ключевые принципы разработки

-   язык разработки — **R и Shiny**;

-   предпочтителен современный tidyverse;

-   используется native pipe `|>`;

-   функции должны быть детерминированными;

-   бизнес-логика, подготовка данных и слой представления должны быть разделены;

-   использование `eval(parse())` не допускается;

-   для названия объектов используется `snake_case`, для названия функций — `camelCase`;

-   предпочтительны читаемость, явность и предсказуемость поведения кода;

-   при программировании с колонками и data-masking следует использовать современные tidy-eval-паттерны (`{{ }}`, `.data[[...]]`, `all_of()`, `any_of()`);

-   в новом коде предпочтительны явные и безопасные `join`-паттерны; в критичных местах ожидания по кардинальности должны задаваться явно;

-   предпочтительны type-stable-подходы и предсказуемые возвращаемые типы;

-   побочные эффекты (запись файлов, вывод, сетевые запросы) должны быть по возможности изолированы от вычислительной логики;

-   оптимизация делается после профилирования, а не по интуиции;

-   новый код должен быть пригоден для тестирования и по возможности покрываться простыми проверками;

-   приоритет отдается решениям, которые легко сопровождать, расширять и переиспользовать в проекте.

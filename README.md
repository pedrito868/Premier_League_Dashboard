# Dashboard: Statystyki piłkarzy Premier League 2022-2023

Jest to prosty dashboard w Shiny analizujący statystyki zawodników z sezonu 2022-2023 Premier League.

## Źródło danych
Dane pochodzą ze strony [fbref.com](https://fbref.com/en/comps/9/2022-2023/stats/2022-2023-Premier-League-Stats).

## Jak uruchomić ten projekt?

1.  **Sklonuj repozytorium:**
    ```bash
    git clone [https://github.com/TWOJA_NAZWA_UŻYTKOWNIKA/premier-league-player-dashboard.git](https://github.com/TWOJA_NAZWA_UŻYTKOWNIKA/premier-league-player-dashboard.git)
    ```
2.  **Otwórz projekt** w RStudio lub VS Code (z rozszerzeniem R).

3.  **Zainstaluj wymagane pakiety:**
    Uruchom poniższą komendę w konsoli R:
    ```r
    install.packages(c("shiny", "ggplot2", "plotly", "formatR", "stringr", "shinythemes", "dplyr", "shinydashboard", "readr", "readxl", "Hmisc"))
    ```
4.  **Uruchom aplikację:**
    Otwórz plik `Piotr_Niemira_dashboard_kod.R` i uruchom aplikację (np. klikając "Run App" w RStudio).s
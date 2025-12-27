
# Projekt-statystyka - Analiza Uzależnienia Studentów od Mediów Społecznościowych
## Projekt wykonany we współpracy z Patrycją Rutkowską
## Skrypt w języku R bada relacje między czasem spędzanym online, wyborem platformy, płcią, statusem związku a poziomem uzależnienia, zdrowiem psychicznym i snem.

Zachęcam do zapoznania się z prezentacją w repozytorium. 

## Wymagane biblioteki:

tidyverse (manipulacja danymi i wizualizacja)

ggpubr, ggcorrplot (zaawansowane wykresy publikacyjne)

psych, car, tseries, nortest (statystyka i testy założeń)

factoextra, cluster (analiza skupień/klastrowanie)

Analiza opiera się na zbiorze danych Students Social Media Addiction.csv.

## Kluczowe zmienne:

Gender (Płeć)

Age (Wiek)

Most_Used_Platform (Najczęściej używana platforma)

Avg_Daily_Usage_Hours (Średni dzienny czas użytkowania)

Addicted_Score (Wynik punktowy uzależnienia)

Sleep_Hours_Per_Night (Długość snu)

Mental_Health_Score (Ocena zdrowia psychicznego)

## Analiza danych:
Przed przystąpieniem do testów hipotez, sprawdzono założenia o normalności rozkładu i jednorodności wariancji:

Test Shapiro-Wilka: Wykazał brak rozkładu normalnego dla zmiennych kluczowych (p < 0.05).

Test Levene'a: Wykazał brak równości wariancji w grupach.

Decyzja: Ze względu na niespełnienie założeń testów parametrycznych, w dalszej części analizy zastosowano testy nieparametryczne (Wilcoxon, Kruskal-Wallis, Spearman).

## Zwerfyfikowano następujące hipotezy badawcze:
-Czy płeć różnicuje poziom uzależnienia?
-Czy istnieje zależność między płcią a wyborem aplikacji?
-Czy bycie w związku chroni przed uzależnieniem?
-Która platforma jest najbardziej uzależniająca?
-Czy wysoki czas ekranowy pogarsza zdrowie psychiczne?
-Użytkownicy której aplikacji śpią najkrócej?
-Czy wykształcenie wpływa na uzależnienie?
-Czy uzależnienie prowadzi do konfliktów z otoczeniem?

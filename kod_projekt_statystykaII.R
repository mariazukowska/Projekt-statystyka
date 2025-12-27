#wczytanie pakietów
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(psych)){install.packages("psych")} 
if(!require(factoextra)){install.packages("factoextra")}
if(!require(car)){install.packages("car")}          
if(!require(tseries)){install.packages("tseries")}   
if(!require(nortest)){install.packages("nortest")}  
if(!require(cluster)){install.packages("cluster")}   
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(ggcorrplot)){install.packages("ggcorrplot")}

library(ggpubr)
library(tidyverse)
library(psych)
library(factoextra)
library(car)
library(tseries)
library(cluster)
library(ggcorrplot)

options(scipen=999)

#wczytanie danych
baza<-read.csv("C:/Users/rutko/Desktop/Projekt_stata2/Students Social Media Addiction.csv")
baza<-na.omit(baza)
#analiza danych
str(baza)
summary(baza)

#graficzna analiza danych

#Strukura płci
df_plec <- baza %>% 
  count(Gender) %>% 
  mutate(prop = n / sum(n) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5 * prop)

ggplot(df_plec, aes(x = "", y = prop, fill = Gender)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() + 
  scale_fill_brewer(palette = "Pastel1") +
  geom_text(aes(y = ypos, label = paste0(round(prop, 1), "%")), color = "black", size = 5) +
  labs(title = "Struktura badanej grupy wg płci")
#Rozkład wieku
  ggplot(baza, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(Age)), color = "red", size = 1) +
  annotate("text", x = mean(baza$Age) + 0.5, y = 50, label = "Średnia", color = "red") +
  theme_minimal() +
  labs(title = "W jakim wieku byli badani?", 
       subtitle = paste("Średnia wieku:", round(mean(baza$Age), 1), "lat"),
       x = "Wiek", y = "Liczba osób")
#Preferencje platform wg płci
  ggplot(baza, aes(x = Gender, fill = Most_Used_Platform)) +
    geom_bar(position = "fill") + 
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3") +
    labs(title = "Procentowy udział platform w grupach kobiet i mężczyzn",
         x = "Płeć", y = "Udział (%)", fill = "Platforma")
#czas vs uzaleznienie
  ggscatter(baza, x = "Avg_Daily_Usage_Hours", y = "Addicted_Score",
            add = "reg.line",                     
            conf.int = TRUE,                         
            color = "lightblue", 
            shape = 21, size = 2, fill = "lightgray", 
            cor.coef = TRUE,                          
            cor.method = "spearman",
            xlab = "Czas spędzany dziennie (h)", ylab = "Wynik uzależnienia") +
    labs(title = "Czas a poziom uzależnienia")
  
##Histogram czasu spędzanego w social mediach
ggplot(baza,aes(x=Avg_Daily_Usage_Hours))+
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black")+
  theme_minimal()+
  labs(title="Rozkład czasu spędzanego w social mediach przez uczniów i studentów",x="Średnia liczba godzin dziennie",y="Liczba osób")

#wykres słupkowy platfrom
ggplot(baza,aes(x=Most_Used_Platform))+
  geom_bar(fill="violet",color="black")+
  theme_minimal()+
  labs(title="Które aplikacje są najpopularniejsze wśród badanych?",x="Platforma",y="Liczba użytkowników")

#histogram snu
ggplot(baza, aes(x = Sleep_Hours_Per_Night)) +geom_histogram(binwidth = 1, fill = "lightyellow", color = "black") +
  theme_minimal() +
  labs(title = "Ile godzin śpią studenci?",
       x = "Godziny snu", 
       y = "Liczba osób")

#mapa ciepła - korelacje spearmana  
baza_numeric <- baza %>%
  select(Avg_Daily_Usage_Hours,Sleep_Hours_Per_Night,Mental_Health_Score,Addicted_Score)

ggcorrplot(cor(baza_numeric, method = "spearman"),lab=TRUE,type="lower",colors=c("blue","white","red"), 
           title = "Macierz korelacji Spearmana")



# ==============================================================================
# CZĘŚĆ 1: WERYFIKACJA ZAŁOŻEŃ (TESTY STATYSTYCZNE)
# ==============================================================================

# --- 1. SPRAWDZENIE DLA PŁCI (Kobiety vs Mężczyźni) ---
# Sprawdzamy normalność w obu grupach osobno
# Shapiro-Wilk Normality test
# H0: Zmienna ma rozklad normalny
# H1: Zmienna nie ma rozkladu normalnego
shapiro.test(baza$Addicted_Score[baza$Gender == "Female"]) 
shapiro.test(baza$Addicted_Score[baza$Gender == "Male"])

# Sprawdzamy równość wariancji (Test Levene'a)
#Test Levene bardziej odporny na brak zalozenia o normalnosci rozkladu

leveneTest(Addicted_Score ~ Gender, data = baza)

# --- 2. SPRAWDZENIE DLA ZWIĄZKÓW (Single vs In Relationship) ---
baza_zwiazki <- baza %>% 
  filter(Relationship_Status %in% c("Single", "In Relationship"))

# -------Normalnosc Zwiazki Test Shapiro
shapiro.test(baza_zwiazki$Addicted_Score[baza_zwiazki$Relationship_Status == "Single"])
shapiro.test(baza_zwiazki$Addicted_Score[baza_zwiazki$Relationship_Status == "In Relationship"])

#-----------Równośc wariancji:Związki Levene
leveneTest(Addicted_Score ~ Relationship_Status, data = baza_zwiazki)


# --- 3. SPRAWDZENIE DLA PLATFORM (Wiele grup) ---
# Tu musimy sprawdzić normalność reszt modelu ANOVA oraz równość wariancji
cat("\n--- ZAŁOŻENIA: PLATFORMY ---\n")

# Budujemy model pomocniczy tylko po to, by wyciągnąć z niego reszty (błędy)
model_pomocniczy <- aov(Addicted_Score ~ Most_Used_Platform, data = baza)

# A) Test normalności reszt
cat("-> Test normalności reszt:\n")
shapiro.test(residuals(model_pomocniczy))

# B) Test równości wariancji (Levene)
cat("-> Test równości wariancji:\n")
leveneTest(Addicted_Score ~ Most_Used_Platform, data = baza)


# --- 4. SPRAWDZENIE DLA KORELACJI (Zmienne ciągłe) ---
# Sprawdzamy to, żeby wiedzieć, czy użyć korelacji Pearsona (dla normalnych) czy Spearmana (dla nienormalnych)
cat("\n--- NORMALNOŚĆ: ZMIENNE CIĄGŁE ---\n")
shapiro.test(baza$Addicted_Score)
shapiro.test(baza$Avg_Daily_Usage_Hours)

#Ze wszystkich testów wynikło że zmienne nie mają rozkładu normalnego i test Levena wykazał brak równości wariancji
# w związku z tym używam metod nieparametrycznych

#################################################################################
#----------------------Płeć-kobiety vs mężczyźni--------------------------------#
#################################################################################

#Test Wilcoxona H0:Kobiety i mężczyźni różnią się znacząco względem uzależnienia od SM
test_plec <- wilcox.test(Addicted_Score ~ Gender, data = baza)
print(test_plec)

#wykres
ggboxplot(baza, 
          x = "Gender", 
          y = "Addicted_Score", 
          color = "Gender", 
          shape = "Gender",
        )+stat_compare_means(method = "wilcox.test", 
                             label.x = 1.4,
                             size = 5)+labs(title = "Czy płeć różnicuje poziom uzależnienia?",
                                            subtitle = "Test Wilcoxona",
                                            y = "Poziom uzależnienia", 
                                            x = "Płeć")

# ==============================================================================
# CZY PŁEĆ JEST ZWIĄZANA Z WYBOREM PLATFORMY? (TEST CHI-KWADRAT)
# ==============================================================================
# Zgodnie z teorią (analiza_zaleznosci.pdf), testujemy niezależność dwóch zmiennych jakościowych.
# H0: Płeć i wybór platformy są niezależne (brak związku)
# H1: Istnieje istotna statystycznie zależność między płcią a wyborem platformy

# 1. Tworzymy tabelę krzyżową (kontyngencji)
tabela_plec_platforma <- table(baza$Gender, baza$Most_Used_Platform)
print("Tabela liczebności:")
print(tabela_plec_platforma)

# 2. Wykonujemy test Chi-kwadrat
test_chi_plec <- chisq.test(tabela_plec_platforma)
print(test_chi_plec)

# Interpretacja:
# Jeśli p-value < 0.05 -> Odrzucamy H0. Płeć MA WPŁYW na to, jakiej platformy używamy najczęściej.

# 3. Wizualizacja zależności (Wykres balonowy - opcja z ggpubr)
ggballoonplot(as.data.frame(tabela_plec_platforma), fill = "value") +
  labs(title = "Czy płeć jest związana z wyborem platformy",
       subtitle = "Test Chi-kwadrat",
       x = "Płeć", 
       y = "Platforma",
       fill = "Liczba osób")

#################################################################################
#----------------------Związki-Single vs In Relationship------------------------#
#################################################################################
#H0:Status związku ma istotny wpływ na wynik
baza_zwiazki <- baza %>% 
  filter(Relationship_Status %in% c("Single", "In Relationship"))

test_zwiazki <- wilcox.test(Addicted_Score ~ Relationship_Status, data = baza_zwiazki)
print(test_zwiazki)

ggboxplot(baza_zwiazki, 
          x = "Relationship_Status", 
          y = "Addicted_Score",
          color = "Relationship_Status", 
          ) +
  
  # Test Wilcoxona dla tych dwóch grup
  stat_compare_means(method = "wilcox.test", 
                     label.y = max(baza$Addicted_Score) + 1,
                     size = 5) +
  
  labs(title = "Czy bycie w związku chroni przed uzależnieniem?",
       subtitle = "Test Wilcoxona",
       x = "Status związku", 
       y = "Poziom uzależnienia")

#################################################################################
#---------------------RANKING PLATFORM------------------------------------------#
#################################################################################
#Nierówne wariancje i brak normalności -> Test Kruskala-Wallisa
#H0: Wszystkie platformy są tak samo uzależniające

test_platformy <- kruskal.test(Addicted_Score ~ Most_Used_Platform, data = baza)
print(test_platformy)

baza_sorted <- baza %>%
  group_by(Most_Used_Platform) %>%
  mutate(mediana = median(Addicted_Score, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(mediana))

ggboxplot(baza_sorted, 
          x = "Most_Used_Platform", 
          y = "Addicted_Score", 
          fill = "Most_Used_Platform", 
          alpha = 0.3,           # Lekka przezroczystość koloru
        ) +
  
  # Test Kruskala-Wallisa (dla wielu grup)
  stat_compare_means(method = "kruskal.test", 
                     label.y = max(baza$Addicted_Score) + 2,
                     size = 4) +
  
  labs(title = "Która platforma uzależnia najbardziej?",
       x = "Platforma",
       y = "Poziom uzależnienia") +
  theme(legend.position = "none")

#ten wykres wzięlam z gemini jako alternatywny do boxplotów
# --- ALTERNATYWA 1: RANKING JAKO LIZAKI (Lollipop Chart) ---
# 1. Przygotowujemy średnie wyniki
baza_ranking <- baza %>%
  group_by(Most_Used_Platform) %>%
  summarise(Srednie_Uzaleznienie = mean(Addicted_Score, na.rm = TRUE)) %>%
  arrange(Srednie_Uzaleznienie) %>%                 # Sortujemy
  mutate(Most_Used_Platform = factor(Most_Used_Platform, levels = Most_Used_Platform)) # Utrwalamy kolejność

# 2. Rysujemy lizaki
ggplot(baza_ranking, aes(x = Most_Used_Platform, y = Srednie_Uzaleznienie)) +
  geom_segment(aes(x = Most_Used_Platform, xend = Most_Used_Platform, 
                   y = 0, yend = Srednie_Uzaleznienie), 
               color = "gray", size = 1) +          # Patyczek
  geom_point(size = 5, color = "#FC4E07") +         # Główka lizaka
  
  # Dodajemy etykietę z liczbą na końcu, żeby było pro
  geom_text(aes(label = round(Srednie_Uzaleznienie, 1)), vjust = -1.5, size = 3.5) +
  
  theme_minimal() +
  labs(title = "Ranking Platform (Wykres Lizakowy)",
       subtitle = "Średni poziom uzależnienia na platformę",
       x = "Platforma",
       y = "Średni wynik uzależnienia")
#################################################################################
#---Zdrowie psychiczne osób używających bardzo mało vs bardzo dużo SM-----------#
#################################################################################
#Rekodowanie i test Wilcoxona
baza_skrajni <- baza %>%
  filter(Avg_Daily_Usage_Hours < 2.5 | Avg_Daily_Usage_Hours > 4.5) %>%
  mutate(Typ_Uzytkownika = ifelse(Avg_Daily_Usage_Hours > 4, "Heavy User", "Light User"))
# Wykres i Test
ggboxplot(baza_skrajni, 
          x = "Typ_Uzytkownika", 
          y = "Mental_Health_Score", 
          fill = "Typ_Uzytkownika", 
          ) +
  stat_compare_means(method = "wilcox.test", label.y = max(baza$Mental_Health_Score) + 1) +
  labs(title = "Czy czas ekranowy niszczy psychikę?",
       x = "Typ użytkownika",
       y = "Zdrowie psychiczne")
#################################################################################
#--Wpływ platformy na sen-------------------------------------------------------#
##################################################################################
#Kruskal-Wallis
baza_sen <- baza %>%
  group_by(Most_Used_Platform) %>%
  mutate(Sredni_Sen = mean(Sleep_Hours_Per_Night, na.rm=T)) %>%
  arrange(Sredni_Sen) # Od najmniej śpiących

# 2. Wykres
ggboxplot(baza_sen, 
          x = "Most_Used_Platform", 
          y = "Sleep_Hours_Per_Night", 
          fill = "Most_Used_Platform", 
          alpha = 0.5) +
  stat_compare_means(method = "kruskal.test", label.y = 10) +
  labs(title = "Złodzieje snu: Użytkownicy której aplikacji śpią najkrócej?",
       y = "Godziny snu", x = "Platforma") +
  theme(legend.position = "none")
#################################################################################
#Czy osoby deklarujące szkodliwy wpływ social mediów na naukę rzeczywiście są uzależnione?
##################################################################################
ggboxplot(baza, 
          x = "Affects_Academic_Performance", 
          y = "Addicted_Score", 
          fill = "Affects_Academic_Performance", 
) +
  stat_compare_means(method = "wilcox.test", label.x = 1.4) +
  labs(title = "Czy osoby deklarujące szkodliwy wpływ social mediów na naukę rzeczywiście są uzależnione??",
       x = "Czy social media wpływają na twoje wyniki w nauce?",
       y = "Poziom uzależnienia")
#################################################################################
#--Uzależnienie a poziom edukacji-----------------------------------------------#
##################################################################################
baza$Academic_Level <- factor(baza$Academic_Level, 
                              levels = c("High School", "Undergraduate", "Graduate"))

ggboxplot(baza, 
          x = "Academic_Level", 
          y = "Addicted_Score", 
          color = "Academic_Level", 
 ) +
  stat_compare_means(method = "kruskal.test", label.y = max(baza$Addicted_Score) + 2) +
  labs(title = "Czy poziom edukacji chroni przed uzależnieniem?",
       subtitle = "Test Kruskala-Wallisa",
       x = "Poziom edukacji",
       y = "Poziom uzależnienia") +
  theme(legend.position = "none")

#################################################################################
#--Czy uzależnienie powoduje konflikty------------------------------------------#
#################################################################################

baza$Conflicts_Over_Social_Media <- as.factor(baza$Conflicts_Over_Social_Media)

# Wykres + Test Kruskala-Wallisa
ggboxplot(baza, 
          x = "Conflicts_Over_Social_Media", 
          y = "Addicted_Score", 
          fill = "Conflicts_Over_Social_Media", 
          palette = "Reds",
) +
  stat_compare_means(method = "kruskal.test", label.y = max(baza$Addicted_Score) + 2) +
  labs(title = "Czy uzależnienie powoduje konflikty?",
       x = "Częstotliwość konfliktów (1=Rzadko, 5=Często)",
       y = "Poziom uzależnienia") +
  theme(legend.position = "none")

## tu też alternatywa z gemini żeby mieć inny typ wykresu
# --- ALTERNATYWA 3: STRUKTURA KONFLIKTÓW (Skumulowany Słupek) ---
# Pytanie: Na której platformie ludzie kłócą się najostrzej?

ggplot(baza, aes(x = Most_Used_Platform, fill = as.factor(Conflicts_Over_Social_Media))) +
  geom_bar(position = "fill") +         # "fill" zamienia liczby na 100% (proporcje)
  scale_y_continuous(labels = scales::percent) +
  
  # Kolory od zimnego (1 = spokój) do gorącego (5 = wojna)
  scale_fill_brewer(palette = "RdYlBu", direction = -1) + 
  
  theme_minimal() +
  labs(title = "Na której platformie ludzie kłócą się najczęściej?",
       x = "Platforma",
       y = "Udział użytkowników (%)",
       fill = "Poziom Konfliktu\n(1 = Rzadko, 5 = Często)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Pochylamy napisy, żeby się mieściły
#################################################################################
#--KLASTROWANIE KRAJÓW (20 krajów z których jest najwięcej studentów)-----------#
#################################################################################
# Wybieramy 20 krajów z największą liczbą studentów
top_kraje_lista <- baza %>%
  count(Country) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  pull(Country)

# Liczymy średnie dla tych krajów i standaryzujemy dane
df_kraje_top <- baza %>%
  filter(Country %in% top_kraje_lista) %>% 
  group_by(Country) %>% 
  summarise(
    Czas_Online = mean(Avg_Daily_Usage_Hours, na.rm = TRUE),
    Uzaleznienie = mean(Addicted_Score, na.rm = TRUE)
  ) %>%
  column_to_rownames("Country") %>% # Nazwy krajów jako etykiety wierszy
  scale() # Standaryzacja
#metoda sylwetki

fviz_nbclust(df_kraje_top, 
             FUN = hcut,  
             method = "silhouette", 
             k.max = 6) +
  labs(title = "Optymalna liczba klastrów ")
  
  # Obliczamy odległości i tworzymy strukturę drzewa (Metoda Warda)
  d <- dist(df_kraje_top, method = "euclidean")
  hc <- hclust(d, method = "ward.D2")
  
  # dendogram
  fviz_dend(hc, 
            k = 3,
            cex = 0.8,           
            k_colors = "jco",   
            rect = TRUE,          
            rect_border = "jco", 
            main = paste("Podobieństwo nawyków cyfrowych (czas & uzależnienie)"),
            ylab = "Skala zróżnicowania",
           )  
  
  
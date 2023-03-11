# Shiny aplikace pro zobrazování dopravních nehod a jejich shluků v ČR

### Vývoj webové aplikace a software na identifikaci klastrů nehod na silniční síti byl podpořen grantem TA ČR (CK01000049): *Tvorba pokročilých nástrojů pro analýzu dopravních nehod pro Policii ČR*

Repozitář obsahuje dvě verze/větve. Ve větvi master je verze aplikace primárně určená určená pro analytiky Policie ČR. Větev public_app obsahuje mírně zjednodušenou verzi aplikace určenou pro veřejnost. Verze pro veřejnost například neobsahuje systém tvorby statických reportů. Aplikace pro veřejnost je (dočasně) dostupná na <https://trafficacc.econ.muni.cz/>

Repozitář obsahuje Makefile, který umožňuje postavit Docker kontejner:

```{bash}
make docker
```

Připravené kontejnery jsou dostupné na Docker Hubu. Nejjednodušší způsob, jak zprovoznit aplikaci bežící v Dockeru je spuštění skriptu start_shiny (master). Jeho součástí je nastavení a) adresáře, kde aplikace hledá předpočítaná data, a b) portu, na kterém je webová aplikace vystavena. Při spuštění skript vždy z Docker Hubu stahuje (pokud je dostupná) aktuální verzi kontejneru.

Aplikace vyžaduje data, která jsou generována "výpočetní" aplikací -- a to v nezměněné adresářové struktuře.

Kontakt: Štěpán Mikula (stepan.mikula\@econ.muni.cz)

[![](tacr/logo_TACR_dopln.png)](https://www.tacr.cz/)

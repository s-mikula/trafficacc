#### Define variables ####

TITLE <- list(
  apptitle = "Analýza dopravních nehod",
  header_title = "Nehody v okrese ",
  tab1 = "Přehled za okres",
  tab2 = "Shluky nehod",
  tab3 = "Dopravní nehody",
  district = "Okres:",
  base_period_fixed = "Období (od/do):",
  base_periodprofile_fixed = "Období (od/do) a profil:",
  base_period = "Základní období (od/do):",
  comp_period = "Uživatelem nastavené srovnávací období",
  comp_period_set = "Srovnávací období: ",
  period_separator = " do ",
  help = "Nápověda",
  acc_selection = "Výběr dopravních nehod:",
  report = "Vygeneruje tisknutelný report v HTML.",
  box_accidents = "Dopravních nehod",
  box_deaths = "Usmrceno osob",
  box_swound = "Těžce zraněno osob",
  box_lwound = "Lehce zraněno osob",
  fig_casualties = "Oběti dopravních nehod",
  fig_faults = "Zavinění dopravních nehod",
  fig_causes = "Nejčastější příčiny dopravních nehod",
  fig_mosttragic = "Nejtragičtější příčiny dopravních nehod",
  fig_crashtype = "Druh srážky",
  fig_obstacle =  "Srážka s pevnou překážkou",
  fig_age = "Počet nehod podle věku řidiče (viníka nehody)",
  fig_agedead = "Počet zemřelých podle věku řidiče",
  menu_profile = "Profil:",
  menu_quantile = "Promile nejhorších úseků:",
  menu_spill = "Dodatečný rozliv klastrů:",
  note_report = "Generování HTML reportu pro velké množství nehod může trvat dlouhou dobu.",
  menu_cluster = "Výběr klastru dopravních nehod",
  mphot = "Mapa shluků dopravních nehod",
  menu_sorting = "Řazení shluků nehod",
  filter_cluster = "Omezit na nehody v oblasti poslední vybrané nehodové lokality.",
  filter_polygon = "Omezit nehody výběrem polygonu na mapě.",
  mpacc = "Mapa dopravních nehod",
  mpacc_label = "Zbarvení dopravních nehod",
  mpacc_choices = list(
    "Podle následků" = "nasledky", 
    "Podle druhu nehody" = "druh_nehody"
  ),
  profiledesc = "Nastavení profilu a výpočtu shluků nehod",
  filteracc = "Dopravní nehody ve výběru: ",
  fig_timedist = "Podíl dopravních nehod podle času (v základním období)"
)

HELP <- list(
  periodovr_text = c("Umožnuje výběr základního období. Srovnávací období je automaticky nastaveno jako předcházející období o stejné délce."),
  period_clusters_text = c("Výběr období, pro které jsou vypočítány shluky dopravních nehod. Výpočet shluků je náročný a proto je možné vybírat pouze z předem připravených možností."),
  profile_text = c("TBD"),
  severity_text = c("Parametr nastavuje, které nehody se berou v potaz pro výpočet shluků nehod. Nížší hodnoty vedou k zahrnutí jen problémovějších úseků. "),
  spill_text = c("Parametr ovlivňuje spojování shluků dopravních nehod, které leží blízko sebe. Shluky kterou jsou od sebe blíže, než je nastavená vzdálenost, se slijí do jednoho shluku. Vzdálenost je měřena v úsecící silnic - lixelech (1 lixel je v průměru cca 5 metrů). Vyšší hodnota parametru vede k nižšímu počtu rozsáhlejších shluků."),
  sorting_text = c("Shluky nehod jsou řazeny podle závažnosti - číslo shluku odpovídá jeho pořadí. Kritérium závažnosti je možné zvolit v rolovacím menu. Pro bližší analýzu dopravních nehod, které se staly na místě identifikovaného shluku, je možné použít nástroje v tabu 'Dopravní nehody'. Pro analýzu v jiném software lze stáhnout geografické vymezení shluku nehod ve formátu GeoJSON."),
  #filteraccidents_text = c(),
  filtercluster_text = c("Omezí dopravní nehody na ty, které se staly v oblasti shluku nehod aktuálně vybraného na tabu 'Nehodové lokality'. Zahrnuté nehody v tabu 'Nehodové lokality' mohou být - i při výběru stejného období - mírně odlišné. Důvodem je odlišné přiřazování nehod ke shlukům. Na tabu 'Dopravní nehody' se používá čistě geografické vymezení shluku. Konečný výběr nehod je průnikem všech filtrů (AND)."),
  filterpolygon_text = c("Na mapě je možné nakreslit obdélník nebo polygon. V případě zapnutí filtru se výběr omezí na nehody, které se staly uvnitř zvoleného polygonu. Konečný výběr nehod je průnikem všech filtrů (AND).")
)

MENU <- list(
  district = c(
    "Benešov" = "CZ0201",
    "Beroun" = "CZ0202",
    "Blansko" = "CZ0641",
    "Brno-město" = "CZ0642",
    "Brno-venkov" = "CZ0643",
    "Bruntál" = "CZ0801",
    "Břeclav" = "CZ0644",
    "Česká Lípa" = "CZ0511",
    "České Budějovice" = "CZ0311",
    "Český Krumlov" = "CZ0312",
    "Děčín" = "CZ0421",
    "Domažlice" = "CZ0321",
    "Frýdek-Místek" = "CZ0802",
    "Havlíčkův Brod" = "CZ0631",
    "Hodonín" = "CZ0645",
    "Hradec Králové" = "CZ0521",
    "Cheb" = "CZ0411",
    "Chomutov" = "CZ0422",
    "Chrudim" = "CZ0531",
    "Jablonec nad Nisou" = "CZ0512",
    "Jeseník" = "CZ0711",
    "Jičín" = "CZ0522",
    "Jihlava" = "CZ0632",
    "Jindřichův Hradec" = "CZ0313",
    "Karlovy Vary" = "CZ0412",
    "Karviná" = "CZ0803",
    "Kladno" = "CZ0203",
    "Klatovy" = "CZ0322",
    "Kolín" = "CZ0204",
    "Kroměříž" = "CZ0721",
    "Kutná Hora" = "CZ0205",
    "Liberec" = "CZ0513",
    "Litoměřice" = "CZ0423",
    "Louny" = "CZ0424",
    "Mělník" = "CZ0206",
    "Mladá Boleslav" = "CZ0207",
    "Most" = "CZ0425",
    "Náchod" = "CZ0523",
    "Nový Jičín" = "CZ0804",
    "Nymburk" = "CZ0208",
    "Olomouc" = "CZ0712",
    "Opava" = "CZ0805",
    "Ostrava-město" = "CZ0806",
    "Pardubice" = "CZ0532",
    "Pelhřimov" = "CZ0633",
    "Písek" = "CZ0314",
    "Plzeň-jih" = "CZ0324",
    "Plzeň-město" = "CZ0323",
    "Plzeň-sever" = "CZ0325",
    "Praha" = "CZ0100",
    "Praha-východ" = "CZ0209",
    "Praha-západ" = "CZ020A",
    "Prachatice" = "CZ0315",
    "Prostějov" = "CZ0713",
    "Přerov" = "CZ0714",
    "Příbram" = "CZ020B",
    "Rakovník" = "CZ020C",
    "Rokycany" = "CZ0326",
    "Rychnov nad Kněžnou" = "CZ0524",
    "Semily" = "CZ0514",
    "Sokolov" = "CZ0413",
    "Strakonice" = "CZ0316",
    "Svitavy" = "CZ0533",
    "Šumperk" = "CZ0715",
    "Tábor" = "CZ0317",
    "Tachov" = "CZ0327",
    "Teplice" = "CZ0426",
    "Trutnov" = "CZ0525",
    "Třebíč" = "CZ0634",
    "Uherské Hradiště" = "CZ0722",
    "Ústí nad Labem" = "CZ0427",
    "Ústí nad Orlicí" = "CZ0534",
    "Vsetín" = "CZ0723",
    "Vyškov" = "CZ0646",
    "Zlín" = "CZ0724",
    "Znojmo" = "CZ0647",
    "Žďár nad Sázavou" = "CZ0635"
  ),
  profile = c(
    "Rovné vážení" = "equal",
    "Výše škod" = "cost"
  ),
  filteraccidents = c("Všechny"="all",
                      "S účastí chodce"="pedestrian",
                      "S účastí cyklisty"="bike",
                      "S účastí motocyklisty"="motobike",
                      "Pod vlivem alkoholu"="alcohol"
  ),
  sorting = c(
    "Celková škoda (v milionech korun)" = "cost",
    "Škoda na metr" = "cost_per_meter"
  )
)

casualties <- c(
  "casualties_pedestrian" = "chodec",
  "casualties_car_driver" = "řidič osobního\nvozidla",
  "casualties_car_crew" = "spolujezdec v\nosobním vozidle",
  "casualties_bike_driver" = "cyklista",
  "casualties_truck_driver" = "řidič nákladního\nvozidla",
  "casualties_truck_crew" = "spolujezdec v\nnákladním vozidle",
  "casualties_motobike_driver" = "motocyklista",
  "casualties_motobike_crew" = "spolujezdec\nna motocyklu",
  "casualties_other_driver" = "řidič jiného\ndopravního prostředku",
  "casualties_other_crew" = "spolujezdec na jiném\ndopravním prostředku"
)

casualties_nolinebreaks <- remove_linebreaks(casualties)

faults <- c(
  "1"="řidičem motorového\nvozidla",
  "2"="řidičem nemotorového\nvozidla",
  "3"="chodcem",
  "4"="lesní a domácí zvěří",
  "5"="jiným účastníkem\nsilničního provozu",
  "6"="závadou komunikace",
  "7"="technickou závadou\nvozidla",
  "0"="jiné zavinení"
)

faults_nolinebreaks <- remove_linebreaks(faults)

causes <- c(
  "100" = "nezaviněná řidičem",
  "201" = "nepřizpůsobení rychlosti\nhustotě provozu",
  "202" = "nepřizpůsobení rychlosti\nviditelnosti",
  "203" = "nepřizpůsobení rychlosti\nvlastn. vozidla a nákladu",
  "204" = "nepřizpůsobení rychlosti\nstavu vozovky",
  "205" = "nepřizpůsobení rychlosti\ndopr.techn.stavu vozovky",
  "206" = "překročení předepsané rychlosti\nstanovené pravidly",
  "207" = "překročení rychlosti stanovené\ndopravní značkou",
  "208" = "nepřizpůsobení rychlosti\nbočnímu, nárazovému větru",
  "209" = "jiný druh nepřiměřené\nrychlosti",
  "301" = "předjíždění vpravo",
  "302" = "předjíždění bez dostatečného\nbočního odstupu",
  "303" = "předjíždění bez dostatečného\nrozhledu",
  "304" = "při předj. došlo k ohrož.\nprotijedoucího řidiče v.",
  "305" = "při předj. došlo k ohrož.\npředjížděného řidiče v.",
  "306" = "předjíždění vlevo vozidla\nodbočujícího vlevo",
  "307" = "předj. v místech, kde je\nto zakazáno dopr.značkou",
  "308" = "při předj. byla přejeta\npodélná čára souvislá",
  "309" = "bránění v předjíždění",
  "310" = "přehlédnutí již předjíždějícího\nsouběžně jed. voz.",
  "311" = "jiný druh nesprávného\npředjíždění",
  "401" = "jízda na červené světlo",
  "402" = "nedání předn. proti příkazu\nd.z. STŮJ DEJ PŘEDNOST",
  "403" = "nedání předn. proti příkazu\nd.z. DEJ PŘEDNOST",
  "404" = "nedání předn. vozidlu\npřijíždějícímu zprava",
  "405" = "nedání předn. při\nodbočování vlevo",
  "406" = "nedání předn. tramvaji,\nkterá odbočuje",
  "407" = "nedání předn. protijed.voz.\npři objíždění překážky",
  "408" = "nedání předn. při zařazování\ndo proudu jedouc.voz.",
  "409" = "nedání předn. při\nvjížděnína silnici",
  "410" = "nedání předn. při\notáčení nebo couvání",
  "411" = "nedání předn. při\npřejíždění z pruhu do pruhu",
  "412" = "nedání předn. chodci\nna vyznačeném přechodu",
  "413" = "nedání předn. při\nodboč.vlevo souběžně jedouc.voz.",
  "414" = "jiné nedání přednosti",
  "501" = "jízda po nespr.straně\nvozovky, vjetí do protisměru",
  "502" = "vyhýbání bez dostatečné\nboční vůle",
  "503" = "nedodržení bezpečné\nvzdálenosti za vozidlem",
  "504" = "nesprávné otáčení\nnebo couvání",
  "505" = "chyby při udání\nsměru jízdy",
  "506" = "bezohledná, agresivní,\nneohleduplná jízda",
  "507" = "náhlé bezdůvodné snížení\nrychlosti jízdy,zastavení",
  "508" = "řidič se plně nevěnoval\nřízení vozidla",
  "509" = "samovolné rozjetí\nnezajištěného vozidla",
  "510" = "vjetí na nezpevněnou\nkrajnici",
  "511" = "nezvládnutí řízení\nvozidla",
  "512" = "jízda (vjetí) jednosměrnou\nulicí, silnicí",
  "516" = "jiný druh nesprávného\nzpůsobu jízdy",
  "601" = "závada řízení",
  "602" = "závada provozní brzdy",
  "603" = "neúčinná nebo nefungující\nparkovací brzda",
  "604" = "optřebení běhounu pláště\npod stanovenou mez",
  "605" = "defekt pneumatiky - průrazem,\nnáhlým únikem vzd.",
  "606" = "závada osvětlovací soustavy\nvozidla",
  "607" = "nepřipoj./poškoz. spoj.\nhadice pro brzd.přípoj.voz.",
  "608" = "nesprávné uložení nákladu",
  "609" = "upadnutí, ztráta kola\nvozidla (i rezervního)",
  "610" = "zablokování kol v důsledku\nmech. závady vozidla",
  "611" = "lom závěsu kola, pružiny",
  "612" = "nazajištěná, poškozená\nbočnice (i u přívěsu)",
  "613" = "závada závěsu pro přívěs",
  "614" = "utržená spojovací hřídel",
  "615" = "jiná technická závada"
)

causes_nolinebreaks <- remove_linebreaks(causes)

crashtype <- c(
  "1" = "srážka s jedoucím\nnekolejovým vozidlem",
  "2" = "srážka s vozidlem\nzaparkovaným, odstav.",
  "3" = "srážka s pevnou\npřekážkou",
  "4" = "srážka s chodcem",
  "5" = "srážka s lesní zvěří",
  "6" = "srážka s domácím\nzvířetem",
  "7" = "srážka s vlakem",
  "8" = "srážka s tramvají",
  "9" = "havárie",
  "0" = "jiný druh nehody"
)

crashtype_nolinebreaks <- remove_linebreaks(crashtype)

obstacletype <- c(
  "1" = "strom",
  "2" = "sloup - telefonní, veř.\nosvětlení, el.vedení apod.",
  "3" = "odrazník, patník, sloupek\nsměrový, dopr.značky ap.",
  "4" = "svodidlo",
  "5" = "překážka vzniklá provozem\njiného vozidla",
  "6" = "zeď, pevná část mostů,\npodjezdů, tunelů apod.",
  "7" = "závory železničního\npřejezdu",
  "8" = "překážka vzniklá\nstavební činností",
  "9" = "jiná překážka - plot,\nnásep, nástupní ostrůvek",
  "0" = "nepřichází v úvahu,\nnejde o srážku s pev.přek."
)

PROFILEDESC <- list(
  PROFILE_COMMENT = "popis profilu",
  NKDE_METHOD = "způsob rozlivu nehod přes křižovatky",
  UNIT_COST_CONST = "jednotková váha připočtená ke každé škodě",
  UNIT_COST_DEAD = "koeficient hodnoty života v mil. Kč",
  UNIT_COST_SERIOUS_INJURY = "koeficient hodnoty vážného zranění v mil. Kč",
  UNIT_COST_LIGHT_INJURY = "koeficient hodnoty lehkého zranění v mil. Kč",
  UNIT_COST_MATERIAL = "koeficient hodnoty materiálních škod (násobitel)",
  ACCIDENT_TO_ROAD_MAX_DISTANCE = "",
  DISTRICT_BUFFER_SIZE = "",
  LIXEL_SIZE = "typická délka lixelů v metrech",
  LIXEL_MIN_DIST = "",
  NKDE_BW = "největší rozliv nehody v metrech",
  SUPPORTED_ROAD_CLASSES = "",
  NKDE_WEIGHTS = "způsob vážení nehod při výpočtu klastrů",
  NKDE_AGG = "",
  NKDE_ADAPTIVE = "je největší možný rozliv stanoven adaptivně?",
  NKDE_TRIM_BW = "maximální povolený rozliv vlivu nehod při adaptivním stanovení rozlivu"
)

AUTHORS <- list(
    line1 = "Vývoj webové aplikace a software na identifikaci klastrů nehod na silniční síti byl podpořen grantem TA ČR (CK01000049): ",
    line2 = "Tvorba pokročilých nástrojů pro analýzu dopravních nehod pro Policii ČR",
    line3 = "Autoři: Michal Kvasnička (michal.kvasnicka@econ.muni.cz) & Štěpán Mikula (stepan.mikula@econ.muni.cz)"
)

ALERT <- list(
  noacc = "Ve výběru nejsou žádné smrtelné nehody.",
  noacc_cause = "Ve výběru nejsou žádné nehody se známou příčinou.",
  noacc_fault = "Ve výběru nejsou žádné zaviněné nehody.",
  title1 = "Konec základního období předchází jeho začátku",
  text1 = "Specifikace období není korektní. Aplikace vyměnila začátek/konec období.",
  title2 = "Rozdílná délka základního a srovnávacího období",
  text2_1 = "Délka základního a srovnávacího období se liší. Statistiky srovnávající obě období, které mají nyní různou délku, tak nebudou vypovídající.",
  text2_2a = "Databáze obsahuje nehody, které se staly od 1.1.2011. Při vybraném základním období by srovnávací období (",
  text2_2b = ") sahalo před toto datum. Statistiky srovnávající obě období, které mají nyní různou délku, tak nebudou vypovídající.",
  title3 = "Základní a srovnávací období se překrývají",
  text3 = "Základní a srovnávací období se překrývají. Statistiky srovnávající obě období, které mají nyní různou délku, tak nebudou vypovídající.",
  title4 = "Základní období předchází srovnávacímu období",
  text4 = "Základní období předchází srovnávacímu období. Věnujte pozornost správné interpretaci statistik, které srovnávající obě období."
)

ACCCHAR <- list(
  id = "ID nehody: ",
  date = "Datum nehody: ",
  type = "Druh nehody: ",
  dead = "Počet mrtvých: ",
  swound = "Počet těžce zraněných: ",
  lwound = "Počet lehce zraněných: ",
  damage = "Škoda na majetku: "
)

BOXTITLE <- list(
  period_baseline = "Ve sledovaném období",
  period_change = "Změna proti srovnávacímu období",
  accidents = "Dopravních nehod",
  dead = "Usmrceno osob",
  swound = "Těžce zraněno osob",
  lwound = "Lehce zraněno osob"
)

FIGS <- list(
  period_baseline = "Základní období",
  period_comparison = "Srovnávací období",
  casualties = "Počet obětí",
  accidents = "Počet nehod",
  dead = "Počet zemřelých",
  age = "Věk řidiče",
  cluster_weight = "Závažnost klastru\nnehod",
  cluster_order = "Pořadí klastru nehod podle závažnosti"
)

mpacc_levels <- list(
  dead = "Nehoda s obětí na životech",
  swound = "Nehoda s těžkým zraněním",
  lwound = "Nehoda s lehkým zraněním",
  other = "Ostatní nehody"
)

mpacc_choices <- list(
  "Podle následků" = "nasledky", 
  "Podle druhu nehody" = "druh_nehody"
)

mpacc_box <- c("Zelený polygon na mapě odpovídá oblasti poslední vybrané nehodové lokality na tabu 'Nehodové lokality'. Uživatel může pomocí mapových nástrojů nakreslit vlastní polygon, který je zobrazen modrou barvou. Nehody lze podle obou polygonů filtrovat.")
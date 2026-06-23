module Language exposing (confirmRemovePlace, confirmRemoveTag, countdownDays, languageCode, tr)

{-| Translation table for the bilingual app.

English strings are used as stable keys. For English, `tr` returns the key
unchanged. For Czech, it looks up the translation and falls back to the key
when a translation is missing.

-}

import Types exposing (Language(..))


confirmRemoveTag : Language -> String -> String
confirmRemoveTag lang tag =
    case lang of
        English ->
            "Remove tag \"" ++ tag ++ "\" from all memories?"

        Czech ->
            "Odebrat tag \"" ++ tag ++ "\" ze všech vzpomínek?"


confirmRemovePlace : Language -> String -> String
confirmRemovePlace lang place =
    case lang of
        English ->
            "Remove place \"" ++ place ++ "\" from all memories?"

        Czech ->
            "Odebrat místo \"" ++ place ++ "\" ze všech vzpomínek?"


countdownDays : Language -> Int -> String
countdownDays lang days =
    case lang of
        English ->
            "in " ++ String.fromInt days ++ " days"

        Czech ->
            "za " ++ String.fromInt days ++ " dní"


tr : Language -> String -> String
tr lang key =
    case lang of
        English ->
            key

        Czech ->
            case key of
                "Home" ->
                    "Domů"

                "Timeline" ->
                    "Timeline"

                "Calendar" ->
                    "Kalendář"

                "Stats" ->
                    "Statistiky"

                "Important days" ->
                    "Důležité dny"

                "Notes" ->
                    "Poznámky"

                "Plans" ->
                    "Plány"

                "Diary" ->
                    "Deníček"

                "New entry" ->
                    "Nový záznam"

                "Save entry" ->
                    "Uložit"

                "Write your thoughts…" ->
                    "Napiš své myšlenky…"

                "Really delete this entry?" ->
                    "Opravdu smazat tento záznam?"

                "No entries yet. Start writing!" ->
                    "Zatím žádné záznamy. Začni psát!"

                "Diary entries cannot be dated in the future." ->
                    "Záznamy do deníku nemohou být datovány v budoucnosti."

                "Please fill in the date." ->
                    "Prosím vyplň datum."

                "Please write something before saving." ->
                    "Prosím napiš něco před uložením."

                "Your personal journal" ->
                    "Tvůj osobní deník"

                "Edit entry" ->
                    "Upravit záznam"

                "Entry" ->
                    "Záznam"

                "+ New entry" ->
                    "+ Nový záznam"

                "Memories" ->
                    "Vzpomínky"

                "Places, people, and photos in one diary" ->
                    "Místa, lidé a fotky v jednom deníku"

                "+ New memory" ->
                    "+ Nová vzpomínka"

                "Tags" ->
                    "Tagy"

                "Places" ->
                    "Místa"

                "New tag…" ->
                    "Nový tag…"

                "New place…" ->
                    "Nové místo…"

                "Trash" ->
                    "Koš"

                "Trash is empty." ->
                    "Koš je prázdný."

                "Restore" ->
                    "Obnovit"

                "Memory" ->
                    "Vzpomínka"

                "Important day" ->
                    "Důležitý den"

                "Plan" ->
                    "Plán"

                "Yes, delete" ->
                    "Ano, smazat"

                "Cancel" ->
                    "Zrušit"

                "Got it" ->
                    "Rozumím"

                "Edit" ->
                    "Upravit"

                "Delete" ->
                    "Smazat"

                "Delete category" ->
                    "Smazat kategorii"

                "Undo" ->
                    "Vrátit zpět"

                "Switch language" ->
                    "Přepnout jazyk"

                "Please fill in all required fields marked with the pink dot." ->
                    "Vyplň prosím všechna povinná pole označená růžovou tečkou."

                "A note with this title already exists. Please choose another title to keep the notebook tidy." ->
                    "Poznámka s tímto názvem už existuje. Zvol prosím jiný název, aby v notýsku zůstal pořádek."

                "Really delete this memory?" ->
                    "Opravdu smazat tuto vzpomínku?"

                "Really delete this day?" ->
                    "Opravdu smazat tento den?"

                "Really delete this note?" ->
                    "Opravdu smazat tuto poznámku?"

                "Really delete this plan?" ->
                    "Opravdu smazat tento plán?"

                "Really delete this category?" ->
                    "Opravdu smazat kategorii?"

                "Really delete this category? Completed plans will stay." ->
                    "Opravdu smazat kategorii? Splněné plány zůstanou."

                "Loading…" ->
                    "Načítání…"

                "Error: " ->
                    "Chyba: "

                "No memories yet." ->
                    "Zatím žádné vzpomínky."

                "No memories match the filter." ->
                    "Žádné vzpomínky neodpovídají filtru."

                "On this day in history" ->
                    "V tento den v minulosti"

                "birthday" ->
                    "narozeniny"

                "Search…" ->
                    "Hledat…"

                "Place" ->
                    "Místo"

                "From" ->
                    "Od"

                "To" ->
                    "Do"

                "Clear filters" ->
                    "Smazat filtry"

                "No memories on this day." ->
                    "V tento den žádné vzpomínky."

                "Nothing on this day." ->
                    "V tento den nic není."

                "New memory" ->
                    "Nová vzpomínka"

                "Edit memory" ->
                    "Upravit vzpomínku"

                "Title" ->
                    "Název"

                "Memory title" ->
                    "Název vzpomínky"

                "Description" ->
                    "Popis"

                "What happened…" ->
                    "Co se stalo…"

                "Place name or exact address…" ->
                    "Název místa nebo přesná adresa…"

                "Open map search. The map is only for finding the place; fill the address into the field manually." ->
                    "Otevřít hledání v mapě. Mapa slouží jen k nalezení místa; adresu je potřeba vyplnit ručně."

                "The icon only opens map search. Fill the selected address into Place manually." ->
                    "Ikonka otevře jen hledání v mapě. Vybranou adresu je potřeba vyplnit ručně do pole Místo."

                "Photos" ->
                    "Fotky"

                "Add photos" ->
                    "Přidat fotky"

                "They are saved into the shared photos/ folder." ->
                    "Ukládají se do společné složky photos/"

                "Attach" ->
                    "Připojit"

                "Add tag…" ->
                    "Přidat tag…"

                "Save" ->
                    "Uložit"

                "Update" ->
                    "Aktualizovat"

                "What our shared archive looks like" ->
                    "Jak vypadá náš společný archiv"

                "Recorded time" ->
                    "Zaznamenaný čas"

                "Memory count" ->
                    "Vzpomínek"

                "Visited places" ->
                    "Navštívených míst"

                "Photo count" ->
                    "Fotografií"

                "Average length" ->
                    "Průměrná délka"

                "Longest memory" ->
                    "Nejdelší vzpomínka"

                "Top tag" ->
                    "Nejčastější tag"

                "Top place" ->
                    "Nejčastější místo"

                "Activity by month" ->
                    "Aktivita po měsících"

                "Activity heatmap" ->
                    "Heatmapa aktivity"

                "sum of all memories" ->
                    "součet všech vzpomínek"

                "saved in the timeline" ->
                    "uložených v timeline"

                "unique places" ->
                    "unikátních míst"

                "attached to memories" ->
                    "připojených ke vzpomínkám"

                "typical length of one saved memory" ->
                    "typická délka jedné uložené vzpomínky"

                "largest time span in the timeline" ->
                    "největší časový úsek v timeline"

                "your most common shared theme" ->
                    "nejčastější společné téma"

                "where you return most often" ->
                    "kam se nejvíc vracíte"

                "no tag yet" ->
                    "zatím žádný tag"

                "no place yet" ->
                    "zatím žádné místo"

                "Loading stats…" ->
                    "Načítání statistik…"

                "Once you add your first memory, a monthly overview will appear here." ->
                    "Až přidáte první vzpomínku, objeví se tady měsíční přehled."

                "Notebook" ->
                    "Poznámkovník"

                "Things worth remembering" ->
                    "Věci, které se hodí vědět"

                "+ Add note" ->
                    "+ Přidat poznámku"

                "Search notes…" ->
                    "Hledat v poznámkách…"

                "Shared notes" ->
                    "Společné poznámky"

                "Shared" ->
                    "Společné"

                "Nothing here yet" ->
                    "Zatím prázdno"

                "Loading notes…" ->
                    "Načítání poznámek…"

                "New note" ->
                    "Nová poznámka"

                "Edit note" ->
                    "Upravit poznámku"

                "Belongs to" ->
                    "Komu patří"

                "Note" ->
                    "Poznámka"

                "Favorite color, allergies, address…" ->
                    "Oblíbená barva, alergie, adresa…"

                "What do you want to remember…" ->
                    "Co si chcete pamatovat…"

                "what applies to both of us" ->
                    "co platí pro nás dva"

                "A good place for shared things: traditions, gift ideas, home routines, or anything that belongs to both of us." ->
                    "Sem se hodí společné věci: tradice, nápady na dárky, domácí pravidla nebo cokoliv, co patří nám dvěma."

                "This can hold allergies, favorite colors, sizes, comfort movies, or small tips for making them happy." ->
                    "Tady můžou být alergie, oblíbená barva, velikosti, filmy na špatný den nebo malé tipy pro radost."

                "Shared plans" ->
                    "Společné plány"

                "What we'll make happen" ->
                    "Co spolu uskutečníme"

                "Create plan" ->
                    "Vytvořit plán"

                "Categories" ->
                    "Kategorie"

                "New category…" ->
                    "Nová kategorie…"

                "Add" ->
                    "Přidat"

                "Rename" ->
                    "Přejmenovat"

                "Delete category (completed plans stay)" ->
                    "Smazat kategorii (splněné plány zůstanou)"

                "Pending" ->
                    "Čekající"

                "Done" ->
                    "Splněné"

                "Loading plans…" ->
                    "Načítání plánů…"

                "No pending plans." ->
                    "Žádné čekající plány."

                "No completed plans." ->
                    "Žádné splněné plány."

                "Still empty. Add a plan with the Create plan button." ->
                    "Zatím prázdné. Přidej plán tlačítkem Vytvořit plán."

                "All plans in this category are done." ->
                    "Všechny plány v této kategorii jsou splněné."

                "done" ->
                    "splněno"

                "pending" ->
                    "čeká"

                "New plan" ->
                    "Nový plán"

                "Edit plan" ->
                    "Upravit plán"

                "Category" ->
                    "Kategorie"

                "Bucket list, Food, Movies…" ->
                    "Bucket list, Jídlo, Filmy…"

                "Sushi night, cinema, trip…" ->
                    "Sushi večer, kino, výlet…"

                "Detail" ->
                    "Detail"

                "Address, note, link, why we want it…" ->
                    "Adresa, poznámka, odkaz, proč to chceme…"

                "Important dates" ->
                    "Důležitá data"

                "No birthdays or other important days yet." ->
                    "Zatím tu nejsou žádné narozeniny ani další důležité dny."

                "+ Add day" ->
                    "+ Přidat den"

                "Relationship counter" ->
                    "Počítadlo vztahu"

                "Enter the day you got together. The app will track each monthiversary and anniversary." ->
                    "Zadej den, kdy jste se dali dohromady. Aplikace pak hlídá výměsíčí každý měsíc a výročí každý rok."

                "Relationship start" ->
                    "Začátek vztahu"

                "Set start date" ->
                    "Nastavit datum začátku"

                "The request URL is invalid." ->
                    "Adresa požadavku není platná."

                "The request timed out. Please try again." ->
                    "Požadavek vypršel. Zkus to prosím znovu."

                "The server is unreachable. Check your connection and try again." ->
                    "Server není dostupný. Zkontroluj připojení a zkus to znovu."

                "The server rejected the request." ->
                    "Server požadavek odmítl."

                "The server returned an invalid response." ->
                    "Server vrátil neplatnou odpověď."

                "No uploaded file was returned." ->
                    "Server nevrátil žádný nahraný soubor."

                "Save date" ->
                    "Uložit datum"

                "After saving, your shared counter will appear here." ->
                    "Po uložení se tady objeví vaše společné počítadlo."

                "monthiversary" ->
                    "výměsíčí"

                "anniversary" ->
                    "výročí"

                "Today!" ->
                    "Dnes!"

                "Tomorrow" ->
                    "Zítra"

                "New important day" ->
                    "Nový důležitý den"

                "Child's birthday…" ->
                    "Narozeniny potomka…"

                "Date" ->
                    "Datum"

                "Reminder…" ->
                    "Připomínka…"

                "Us two" ->
                    "My dva"

                "days together" ->
                    "dnů spolu"

                "Edit heart" ->
                    "Upravit srdce"

                "Heart color" ->
                    "Barva srdce"

                "We got together" ->
                    "Dali jsme se dohromady"

                "Save heart" ->
                    "Uložit srdce"

                "Edit avatar" ->
                    "Úprava avatara"

                "Name" ->
                    "Jméno"

                "Display" ->
                    "Zobrazení"

                "Figure" ->
                    "Panáček"

                "Photo" ->
                    "Fotka"

                "Upload" ->
                    "Nahrát"

                "Figure color" ->
                    "Barva panáčka"

                "Accessory" ->
                    "Doplněk"

                "Expression" ->
                    "Výraz"

                "Birthday" ->
                    "Narození"

                "Red" ->
                    "Červená"

                "Pink" ->
                    "Růžová"

                "Purple" ->
                    "Fialová"

                "Blue" ->
                    "Modrá"

                "Green" ->
                    "Zelená"

                "Yellow" ->
                    "Žlutá"

                "Orange" ->
                    "Oranžová"

                "Gray" ->
                    "Šedá"

                "Black" ->
                    "Černá"

                "No accessory" ->
                    "Bez doplňku"

                "Flower" ->
                    "Kytička"

                "Hat" ->
                    "Klobouček"

                "Crown" ->
                    "Korunka"

                "Bow" ->
                    "Mašle"

                "Halo" ->
                    "Svatozář"

                "Party hat" ->
                    "Party čepička"

                "Heart" ->
                    "Srdíčko"

                "Smile" ->
                    "Úsměv"

                "Laughing" ->
                    "Vysmátý"

                "Neutral" ->
                    "Neutrální"

                "In love" ->
                    "Zamilovaný"

                "Sad" ->
                    "Smutný"

                "Angry" ->
                    "Naštvaný"

                "Surprised" ->
                    "Překvapený"

                "Latest memories" ->
                    "Poslední vzpomínky"

                "Our plans" ->
                    "Naše plány"

                "On this day" ->
                    "Dnes ve vzpomínkách"

                "next date" ->
                    "nejbližší datum"

                "Once you add your first memory, a small selection will appear here." ->
                    "Až přidáte první vzpomínku, objeví se tady malý výběr."

                "Upcoming ideas from Plans will appear here." ->
                    "Sem se budou propisovat nejbližší nápady z Plánů."

                "When a memory matches today's day and month, it will be remembered here." ->
                    "Až se nějaká vzpomínka potká se stejným dnem v roce, připomene se tady."

                "The end time must be the same as or later than the start time." ->
                    "Čas konce musí být stejný nebo pozdější než čas začátku."

                "Edit important day" ->
                    "Upravit důležitý den"

                "today" ->
                    "dnes"

                _ ->
                    key


languageCode : Language -> String
languageCode lang =
    case lang of
        English ->
            "EN"

        Czech ->
            "CZ"

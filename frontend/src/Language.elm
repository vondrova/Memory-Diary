{- Bilingual translation table. Czech is explicit; English keys pass through unchanged. -}
module Language exposing (tr)

import Types exposing (Lang(..))


tr : Lang -> String -> String
tr lang key =
    case lang of
        EN ->
            key

        CZ ->
            case key of
                "nav_memories"   -> "Vzpomínky"
                "nav_timeline"   -> "Timeline"
                "nav_important"  -> "Důležité dny"
                "nav_notes"      -> "Poznámky"
                "nav_plans"      -> "Plány"
                "nav_diary"      -> "Deník"
                "nav_stats"      -> "Statistiky"
                "nav_trash"      -> "Koš"
                "add_memory"     -> "+ Přidat vzpomínku"
                "save"           -> "Uložit"
                "cancel"         -> "Zrušit"
                "search"         -> "Hledat…"
                "empty"          -> "Nic tu není."
                "new_memory"     -> "Nová vzpomínka"
                "name"           -> "Název"
                "from"           -> "Od"
                "to"             -> "Do"
                "place"          -> "Místo"
                "tags"           -> "Tagy (oddělené čárkou)"
                "description"    -> "Popis"
                "pick_photo"     -> "📷 Vybrat foto"
                "map_search"     -> "Hledat na mapě…"
                "add_day"        -> "Přidat důležitý den"
                "date"           -> "Datum"
                "kind"           -> "Typ"
                "anniversary"    -> "💍 Výročí"
                "birthday"       -> "🎂 Narozeniny"
                "other_kind"     -> "📅 Jiné"
                "new_note"       -> "Nová poznámka"
                "from_who"       -> "Od koho"
                "title"          -> "Nadpis"
                "body"           -> "Text"
                "new_plan"       -> "Nový plán"
                "category"       -> "Kategorie"
                "detail"         -> "Detail"
                "done"           -> "Hotovo"
                "new_entry"      -> "Nový zápis"
                "mood"           -> "Nálada"
                "statistics"     -> "Statistiky"
                "memory_count"   -> "Vzpomínek"
                "total_minutes"  -> "Celkem minut"
                "avg_minutes"    -> "Průměr minut"
                "visited_places" -> "Navštívená místa"
                "top_tags"       -> "Nejčastější tagy"
                "trash_title"    -> "Koš"
                "trash_empty"    -> "Koš je prázdný."
                "restore"        -> "Obnovit"
                _                -> key

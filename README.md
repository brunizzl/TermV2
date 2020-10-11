# TermV2
Classes meant to simplify mathematical terms

## Planung Patterns

Zeile hat Aufbau: "*deklarationen* | *muster* = *muster*"

                
*muster* kann enthalten: 
- alle Elemente eines normalen Terms
    - Summe
    - Produkt
    - Bekannte Funktion
    - Generische Funktion
    - Variable
    - Wert
- Globale Mustervariablen (müssen in *deklarationen* deklariert werden, mehrfach pro Seite nutzbar)
- lokale Mustervariablen (nur innerhalb von Summen Muster oder Produkt Muster erlaubt, nur jeweils einfach)
- Summen Muster: "sum{ *Summand Muster* | *deklarationen von lokalen Mustervariablen* \<\- *auflistung Globale Mustervariablen* }"
- Produkt Muster: "product{ *Faktor Muster* | *deklarationen von lokalen Mustervariablen* \<\- *auflistung Globale Mustervariablen* }"
    
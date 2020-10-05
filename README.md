# TermV2
Classes meant to simplify mathematical terms

## Planung Patterns

Zeile hat Aufbau: "*lhs* = *rhs*"

### Matching Seite
Pattern Aufbau (lhs): "*Muster Term*" oder "*Muster Term* | *Bedingungen*"
                
*Muster Term* kann enthalten: 
- alle Elemente eines normalen Terms
    - Summe
    - Produkt
    - Bekannte Funktion
    - Generische Funktion
    - Variable
    - Wert
- Muster Variablen 
- *vielleicht* eingeschränkte Mustervariablen (sonst nur in *Bedingungen* erlauben?)
- *vielleicht* Summen Muster: "*Muster Variable*:sum{ *Summand Muster* }"
- *vielleicht* Produkt Muster: "*Muster Variable*:product{ *Faktor Muster* }"
- *vielleicht* *vielleicht* erweitertes Summen Muster: "*Muster Variable*:sum{ *Summand Muster* | *Bedingungen* }" 
- *vielleicht* *vielleicht* erweitertes Produkt Muster: "*Muster Variable*:product{ *Faktor Muster* | *Bedingungen* }" 
    
*Bedingungen* ist komma separierte Aufzählung aus Logikausdrücken. Ein Logikausdruck kann enthalten:
- Negation: "!*Logik Operand*"
- Und: "*Logik Operand1* && *Logik Operand2*"
- Oder: "*Logik Operand1* || *Logik Operand2*"
- Prädikat: "*Name*(*Muster Term*)" (prädikat aus vorher bekannter menge mit enum?)
- Vergleich: "*Muster Term* *op* *Muster Term*" mit op aus "==", "!=", "\<=", "\>=" "\<" "\>"



### Ergebnis Seite   
Ergebnis Aufbau (rhs): ???
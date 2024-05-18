# Opis języka
Język imperatywny na podstawie języka `Latte`. Ma trzy typy: `int`, `bool` oraz `string`. Większość konstrukcji jest identyczna jak w `Latte`. Z tą różnicą, że wykonywane operacje nie muszą być w ciele jakiejś funkcji (podobnie jak w `Python`). Dodatkowo można przekazywać zmienne poprzez wartość oraz zmienną.

Jeśli funkcja nie zakończy się `return _;` przyjmujemy domyślne zwracane wartości: `int` -> `0`, `bool` -> `false`, `string` -> `""`.
# Tabelka cech
```
Na 15 punktów
  01 (trzy typy) +
  02 (literały, arytmetyka, porównania) +
  03 (zmienne, przypisanie) +
  04 (print) +
  05 (while, if) +
  06 (funkcje lub procedury, rekurencja) +
  07 (przez zmienną / przez wartość / in/out) +
  08 (zmienne read-only i pętla for) -
  Na 20 punktów
  09 (przesłanianie i statyczne wiązanie) +
  10 (obsługa błędów wykonania) +
  11 (funkcje zwracające wartość) +
  Na 30 punktów
  12 (4) (statyczne typowanie) +
  13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem) +
  14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe) -
  15 (2) (krotki z przypisaniem) -
  16 (1) (break, continue) -
  17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia) -
  18 (3) (generatory) -
```
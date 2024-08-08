# Opis języka
Język imperatywny na podstawie języka `Latte`. Ma trzy typy: `int`, `bool` oraz `string`. Większość konstrukcji jest identyczna jak w `Latte`. Z tą różnicą, że wykonywane operacje nie muszą być w ciele jakiejś funkcji (podobnie jak w `Python`). Dodatkowo można przekazywać zmienne poprzez wartość oraz zmienną.

Jeśli funkcja nie zakończy się `return _;` przyjmujemy domyślne zwracane wartości: `int` -> `0`, `bool` -> `false`, `string` -> `""`.
# Uruchomienie
Do uruchomienia wymagane jest `GHC` w wersji `9.0.2`.
## Kompilacja
Do stworzenia pliku wykonywalnego wystarczy uruchomienie w katalogu głównym polecenia:
```
make
```
## Wykonanie programu
Są dwie możliwości dostarczenia programu dla interpretera, przez plik lub standardowe wejście.

Przez plik:
```
./interpreter <path-to-file>
```

Aby wczytywał ze standardowego wejścia wystarczy uruchomić bez żadnych argumentów:
```
./interpreter
```
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
# Przykładowe programy
Więcej w katalogu `good`.
## Hello world
```
print "Hello world!";
```
## Rekurencyjna silnia
```
int factr (int n) {
  if (n < 2)
    return 1 ;
  else
    return (n * factr(n-1)) ;
}

print factr(3);
```
## Przekazywanie przez wartość i zmienną
```
int valref (int x, int & y) {
   x = x + 1;
   y = y + 1;

   return x + y;
}

int x = 1;
int y = 1;

print valref(x,y);
print x;
print y;
```
## Zagnieżdżona definicja funkcji
```
string level1() {
  string level2() {
    
  return "level 2";
  }
  print level2();
  return "level 1";
}

print level1();
```
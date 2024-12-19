# Interpreter języka imperatywnego
## Opis języka
1. Język ma trzy typy wartości: int, boolean, string.
2. Obsługuje literały, operatory: +, -, *, /, %, <, <=, >=, >, ==, =, !=, ||, && (jak w Latte).
3. Umożliwia deklarację zmiennych oraz przypisanie im wartości.
4. Ma wbudowaną funkcję println, przyjmującą argument dowolnego typu i wypisującego go w na wyjście ze znakiem końca linii.
5. Obsługuje while, if, else. Blok, następujący po każdej z tych instrukcji, musi być w nawiasach klamrowych.
6. Nie ma procedur, a wyłącznie funkcje, zwracające wartość dowolnego typu.
7. Umożliwia przekazywanie parametrów przez wartość i przez zmienną. Aby przekazać funkcji referencję do zmiennej należy użyć w jej deklaracji var np.

   int f(var int x) { return x; }.

9. Obsługuje zgnieżdżone funkcje.
10. W przypadku błędu wypisuje na wyjściu komunikat i zatrzymuje działanie programu.
11. Jak w punkcie 6.
12. Ma zaimplementowane statyczne typowanie.
13. Jak w punkcie 9.

## Opis implementacji
-  Do analizy składni użyłam BNFC.
- W [TypeChecker.hs](TypeChecker.hs) zostało zrealizowane statyczne typowanie, które odbywa się przed wykonaniem programu przez interpreter.
- W [Interpreter.hs](Interpreter.hs) znajduje się kod interpretatora.
- Funkcja main jest w [IcedAmericano.hs](IcedAmericano.hs).
- Kody błędów są w oddzielnym pliku [ErrorMessage.hs](ErrorMessage.hs).
## Sposób uruchomienia
Interpreter uruchamiamy poleceniami:
``` 
make
./interpreter <ścieżka_do_pliku>
``` 
Alternatywnie można podać program na wejściu intepretera: `cat <ścieżka_do_pliku> | ./interpreter`.

## Tabelka cech
Tabelka cech znajduje się w [tabelka_cech.txt](tabelka_cech.txt).
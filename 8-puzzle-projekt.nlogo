extensions [matrix table]

breed [tiles tile]
tiles-own [
  udaljenost-od-cilja
  ]


globals [osnovna-matrica
  m              ;matrica
  lista          ;potrebno za stvoriti matricu
  stanja
  prazno
  oko-nule ;brojevi koji su oko praznog mjesta - koje mozemo pomaknit
  broj-koraka

  pokreti
  pronađen-put
  granica

  ;varijable potrebne za A*
  neighbour
  open
  closed
  ]

to setup
  ca
  generiraj-pocetno-stanje
  stvori-stanja
  set stanja (list (list lista  [0]) )
  find-moves lista
  set broj-koraka 0
  reset-ticks
  set granica racunaj-udaljenost first stanja + racunaj-heuristiku first first stanja ; granica potrebna za IDA*
  set closed []
  set open []
end



to generiraj-pocetno-stanje ;postavljanje početnog stanja
  if vrsta-slagalice = "1" [set lista [1 2 3 4 0 5 7 8 6]]
  if vrsta-slagalice = "2" [set lista [1 2 3 7 4 5 0 8 6]]
  if vrsta-slagalice = "3" [set lista [1 2 3 4 8 0 7 6 5]]
  if vrsta-slagalice = "4" [set lista [4 1 3 7 2 6 5 8 0]]
  if vrsta-slagalice = "5" [set lista [1 6 2 5 3 0 4 7 8]]
  if vrsta-slagalice = "6" [set lista [5 1 2 6 3 0 4 7 8]]
  if vrsta-slagalice = "7" [set lista [1 2 6 3 5 0 4 7 8]]
  if vrsta-slagalice = "8" [set lista [3 5 6 1 4 8 0 7 2]]
  if vrsta-slagalice = "9" [set lista [4 3 6 8 7 1 0 5 2]]
  if vrsta-slagalice = "10" [set lista [3 0 2 6 5 1 4 7 8]]

  foreach lista [if ? = 0 [set prazno position ? lista]]

  let l1 (list item 0 lista item 1 lista item 2 lista)
  let l2 (list item 3 lista item 4 lista item 5 lista)
  let l3 (list item 6 lista item 7 lista item 8 lista)
  set m matrix:from-row-list (list l1 l2 l3)
end

to stvori-stanja ;prikaz stanja na simulaciji preko tiles - pločica
  foreach [0 1 2] [
    let j ?1

   foreach [0 1 2] [
     let i ?1
    let broj matrix:get m i j
    create-tiles 1 [
      if broj = 0 [set shape "square" set color black]
      if broj = 1 [set shape "jedan" ]
      if broj = 2 [set shape "dva" ]
      if broj = 3 [set shape "tri" ]
      if broj = 4 [set shape "cetiri" ]
      if broj = 5 [set shape "pet" ]
      if broj = 6 [set shape "sest"]
      if broj = 7 [set shape "sedam"]
      if broj = 8 [set shape "osam" ]
      move-to patch (j - 3) (3 - i)
      set size 1.2
      set heading 0

    ]
  ]
  ]
end


to find-moves [trenutna-lista] ;procedura za pronalaženje dozvoljenih akcija tj. elemenata koji se mogu pomaknuti
  set oko-nule []
  set pokreti []
  foreach trenutna-lista [if ? = 0 [set prazno position ? trenutna-lista]]

  if not((prazno - 1) mod 3 = 2) [set oko-nule lput (item (prazno - 1) trenutna-lista) oko-nule set pokreti lput "L" pokreti]
  if not((prazno + 1) mod 3 = 0) [set oko-nule lput (item (prazno + 1) trenutna-lista) oko-nule set pokreti lput "R" pokreti]
  if not((prazno - 3) < 0) [set oko-nule lput (item (prazno - 3) trenutna-lista) oko-nule set pokreti lput "U" pokreti]
  if not ((prazno + 3) > 8) [set oko-nule lput (item (prazno + 3) trenutna-lista) oko-nule set pokreti lput "D" pokreti]
end




to korak

  if test-cilja = 1 [stop]

  if algoritam = "pretraga-po-sirini" [ pretraga-po-sirini]
  if algoritam = "pretraga-po-dubini" [pretraga-po-dubini if provjera-sve = 0 [stop] ]
  if algoritam = "pohlepna-pretraga" [pohlepna-pretraga if provjera-sve = 0 [stop]]
  if algoritam = "astar-pretraga" [ astar-pretraga if provjera-sve = 0 [stop] ]
  if algoritam = "ida-algoritam" [ ida-pretraga]
  ifelse pronađi-put = 0 [set pronađen-put false] [set pronađen-put true]

  tick
end
























to sirina-korak [element]
  find-moves (first element)

  while [length oko-nule > 0] [
    let trenutna-lista first element ;lista u kojoj je prikazano stanje
    let moves last element ; lista u kojoj su prikazani dosadasnji potezi
    let broj first oko-nule ;uzimamo prvi broj za zamjenu
    let potez first pokreti ;uzimamo i prvi potez, koji odgovara prvom broju

    let trenutna-poz 0
    foreach trenutna-lista [if ? = broj [set trenutna-poz position ? trenutna-lista]] ; trazimo trenutnu poziciju broja koji gledamo
    set trenutna-lista replace-item prazno trenutna-lista (broj) ; na mjesto 0 stavljamo broj
    set trenutna-lista replace-item trenutna-poz trenutna-lista 0 ; na mjesto broja stavljamo 0
    set moves lput potez moves ;na listu poteza stavljamo potez koji smo napravili da dobijemo novo stanje
    set oko-nule remove (first oko-nule) oko-nule ; mičemo broj s liste
    set pokreti remove (first pokreti) pokreti ; mičemo pokret s liste

    if (provjera trenutna-lista) = 0 [ set stanja lput (list trenutna-lista moves) stanja] ;dodajemo stanje (raspored brojeva + potezi do tu) u listu stanja
  ] ; radimo sve dok ima brojeva oko praznog mjesta

end



to pretraga-po-sirini
  if test-cilja = 1 [stop]
  set broj-koraka broj-koraka + 1 ;trenutna razina kroz koju prolazimo
  foreach stanja [
    if length(last ?) = broj-koraka [
      sirina-korak (?)
      ]
    ]

end




to-report provjera [trenutna-lista] ;provjera nalazi li se trenutna-lista već u stanjima
  let f 0

  foreach stanja [
    if first ? = trenutna-lista [set f 1]
    ]
  report f

end

to-report test-cilja ; procedura koja provjerava je li se došlo do cilja
  let cilj 0
  foreach stanja
  [ if first ? = [1 2 3 4 5 6 7 8 0] [set cilj 1]
    ]
  report cilj
end



to-report pronađi-put ;ako se došlo do cilja, vraća se put - potezi koji su napravljeni da se dođe do cilja
  let put 0
  foreach stanja
  [ if first ? = [1 2 3 4 5 6 7 8 0] [set put last ?]
    ]
  report put
end


to ocitaj-matricu ;procedura za očitavanje stanja na simulaciji u matricu
   set m matrix:from-row-list [[0 0 0] [0 0 0] [0 0 0]]

  ask tiles [
    if shape = "jedan" [matrix:set m (- ycor) xcor 1]
    if shape = "dva" [matrix:set m  (- ycor) xcor 2]
    if shape = "tri" [matrix:set m (- ycor) xcor 3]
    if shape = "cetiri" [matrix:set m (- ycor) xcor 4]
    if shape = "pet" [matrix:set m (- ycor) xcor 5]
    if shape = "sest" [matrix:set m (- ycor) xcor 6]
    if shape = "sedam" [matrix:set m (- ycor) xcor 7]
    if shape = "osam" [matrix:set m (- ycor) xcor 8]
    ]
end

to pomakni ;procedura koja služi za vizualnu prezentaciju puta
  let put pronađi-put
  if put = 0 [user-message "Algoritam nije pronašao put!" stop]
  let trenutno []
  foreach put [
    set trenutno lput ? trenutno
    foreach stanja [
      if ((last ?) = trenutno) [stvori-matricu (first ?) stvori-stanja]
    ]
    wait 0.75
]

end


to stvori-matricu [tr-lista] ;procedura za stvaranje matrice iz liste

  let l1 (list item 0 tr-lista item 1 tr-lista item 2 tr-lista)
  let l2 (list item 3 tr-lista item 4 tr-lista item 5 tr-lista)
  let l3 (list item 6 tr-lista item 7 tr-lista item 8 tr-lista)

  set m matrix:from-row-list (list l1 l2 l3)


end


to dubina-korak [element]
  find-moves (first element)

  while [length oko-nule > 0] [
    let trenutna-lista first element ;lista u kojoj je prikazano stanje
    let moves last element ; lista u kojoj su prikazani dosadasnji potezi
    let broj first oko-nule ;uzimamo prvi broj za zamjenu
    let potez first pokreti ;uzimamo i prvi potez, koji odgovara prvom broju

    let trenutna-poz 0
    foreach trenutna-lista [if ? = broj [set trenutna-poz position ? trenutna-lista]] ; trazimo trenutnu poziciju broja koji gledamo
    set trenutna-lista replace-item prazno trenutna-lista (broj) ; na mjesto 0 stavljamo broj
    set trenutna-lista replace-item trenutna-poz trenutna-lista 0 ; na mjesto broja stavljamo 0
    set moves lput potez moves ;na listu poteza stavljamo potez koji smo napravili da dobijemo novo stanje
    set oko-nule remove (first oko-nule) oko-nule ; mičemo broj s liste
    set pokreti remove (first pokreti) pokreti ; mičemo pokret s liste
  if (provjera trenutna-lista) = 0 [ set stanja fput (list trenutna-lista 0 moves)  stanja] ;dodajemo stanje (raspored brojeva + potezi do tu) u listu stanja
  ] ; radimo sve dok ima brojeva oko praznog mjesta

  let y position element stanja
  set stanja remove-item y stanja
  set stanja lput (list first element 1 last element) stanja ;stavljamo element koji smo upravo posjetili na kraj liste stanja
end


to pretraga-po-dubini

 ; prvi element na listi stanja nema oznaku posjećenosti, pa mu stavljamo da
 ; nije još posjećen
 let y position (list lista [0]) stanja
 let a (list lista 0 [0])
 if not(y = false) [set stanja replace-item y stanja a]

 ; korak
 let x first stanja
 ifelse not (length(last x) = dubina) [ dubina-korak x] ;ako nismo dosli do granicne dubine, izvrsavamo korak
      [
        let i position x stanja
        set stanja remove-item i stanja
        set stanja lput (list first x 1 last x) stanja] ;ako smo dosli do granicne dubine, stanje označimo kao posjećeno i stavimo na kraj

 ;provjeravamo ima li neposjećenih čvorova, ako nema, završavamo program
 let broj 0
 foreach stanja [
     if item 1 ? = 0 [set broj broj + 1]
     ]
   if broj = 0 [stop]



end



to-report provjera-sve ;provjerava jesu li svi elementi posjećeni
  let broj 0
   foreach stanja
   [
     if length ? = 4[if item 2 ? = 0 [set broj broj + 1]]
     if length ? = 3 [if item 1 ? = 0 [set broj broj + 1]]

   ]
   report broj
end




to-report racunaj-heuristiku [trenutna-lista] ;racuna manhattan udaljenost cijelog stanja
  let udaljenost 0

  let l1 (list item 0 trenutna-lista item 1 trenutna-lista item 2 trenutna-lista)
  let l2 (list item 3 trenutna-lista item 4 trenutna-lista item 5 trenutna-lista)
  let l3 (list item 6 trenutna-lista item 7 trenutna-lista item 8 trenutna-lista)

  let matrica matrix:from-row-list (list l1 l2 l3)
  set osnovna-matrica matrix:from-row-list [[0 1 2] [3 4 5] [6 7 8]]

  foreach [0 1 2] ;retci
  [
    let i ?
    foreach [0 1 2] ;stupci
    [
      let j ?
      if (matrix:get matrica i j) = 1 [set udaljenost udaljenost + abs(i) + abs(j)]
      if (matrix:get matrica i j) = 2 [set udaljenost udaljenost + abs(i) + abs(j - 1)]
      if (matrix:get matrica i j) = 3 [set udaljenost udaljenost + abs(i) + abs(j - 2)]
      if (matrix:get matrica i j) = 4 [set udaljenost udaljenost + abs(i - 1) + abs(j)]
      if (matrix:get matrica i j) = 5 [set udaljenost udaljenost + abs(i - 1) + abs(j - 1)]
      if (matrix:get matrica i j) = 6 [set udaljenost udaljenost + abs(i - 1) + abs(j - 2)]
      if (matrix:get matrica i j) = 7 [set udaljenost udaljenost + abs(i - 2) + abs(j)]
      if (matrix:get matrica i j) = 8 [set udaljenost udaljenost + abs(i - 2) + abs(j - 1)]
      ]
    ]
  report udaljenost

end

to-report racunaj-udaljenost [element] ;racuna koliko smo poteza napravili do trenutnog stanja
  report length (last element) - 1
end


to pohlepna-korak [element]
  find-moves (first element)

  while [length oko-nule > 0] [
    let trenutna-lista first element ;lista u kojoj je prikazano stanje
    let moves last element ; lista u kojoj su prikazani dosadasnji potezi
    let broj first oko-nule ;uzimamo prvi broj za zamjenu
    let potez first pokreti ;uzimamo i prvi potez, koji odgovara prvom broju

    let trenutna-poz 0
    foreach trenutna-lista [if ? = broj [set trenutna-poz position ? trenutna-lista]] ; trazimo trenutnu poziciju broja koji gledamo
    set trenutna-lista replace-item prazno trenutna-lista (broj) ; na mjesto 0 stavljamo broj
    set trenutna-lista replace-item trenutna-poz trenutna-lista 0 ; na mjesto broja stavljamo 0
    set moves lput potez moves ;na listu poteza stavljamo potez koji smo napravili da dobijemo novo stanje
    set oko-nule remove (first oko-nule) oko-nule ; mičemo broj s liste
    set pokreti remove (first pokreti) pokreti ; mičemo pokret s liste
    if (provjera trenutna-lista) = 0 [ set stanja fput (list trenutna-lista (racunaj-heuristiku trenutna-lista) 0 moves)  stanja]
  ;dodajemo stanje (raspored brojeva + potezi do tu) u listu stanja
  ] ; radimo sve dok ima brojeva oko praznog mjesta

  ;stavljamo element koji smo upravo posjetili na kraj liste stanja
  let y position element stanja
  set stanja remove-item y stanja
  set stanja lput (list first element (racunaj-heuristiku first element) 1 last element) stanja
end


to pohlepna-pretraga
 let y position (list lista [0]) stanja
 let a (list lista (racunaj-heuristiku lista) 0 [0])
 if not(y = false) [set stanja replace-item y stanja a] ;početnom stanju dodajemo vrijednost heuristike i oznaku posjećenosti
 let trenutna-stanja []
 let minimum 100

 let x first stanja

 foreach stanja [
   if length (last ?) = length (last x) and item 2 ? = 0 [if item 1 ? < minimum [set minimum (item 1 ?)]]
   ] ;traži neposjećeno stanje s najmanjom vrijednosti

 foreach stanja [
   if length (last ?) = length (last x) and item 2 ? = 0 [if item 1 ? = minimum [set trenutna-stanja lput ? trenutna-stanja]]
   ] ;one s minimalnom vrijednosti stavljamo na listu trenutna-stanja


 let broj 0
   foreach stanja
   [
     if item 2 ? = 0 [set broj broj + 1]

     ]
   if broj = 0 [stop]

 set x first trenutna-stanja

 ifelse not (length(last x) = dubina) [pohlepna-korak x] ;ako nismo dosli do granice dubine, izvrsavamo korak, u suprotnom, označavamo da smo posjetili i stavljamo na kraj
      [
        let i position x stanja
        set stanja remove-item i stanja
        set stanja lput (list first x  (item 1 x) 1 last x) stanja
        ]


end








to astar-korak [element]
  find-moves (first element)

  while [length oko-nule > 0] [
   let trenutna-lista first element ;lista u kojoj je prikazano stanje
  let moves last element ; lista u kojoj su prikazani dosadasnji potezi
  let broj first oko-nule ;uzimamo prvi broj za zamjenu
  let potez first pokreti ;uzimamo i prvi potez, koji odgovara prvom broju

  let trenutna-poz 0
  foreach trenutna-lista [if ? = broj [set trenutna-poz position ? trenutna-lista]] ; trazimo trenutnu poziciju broja koji gledamo
  set trenutna-lista replace-item prazno trenutna-lista (broj) ; na mjesto 0 stavljamo broj
  set trenutna-lista replace-item trenutna-poz trenutna-lista 0 ; na mjesto broja stavljamo 0
  set moves lput potez moves ;na listu poteza stavljamo potez koji smo napravili da dobijemo novo stanje
  set oko-nule remove (first oko-nule) oko-nule ; mičemo broj s liste
  set pokreti remove (first pokreti) pokreti ; mičemo pokret s liste
  let udaljenost racunaj-heuristiku trenutna-lista + racunaj-udaljenost (list trenutna-lista moves)
  let x (list trenutna-lista udaljenost moves)
  if provjera trenutna-lista = 0 [set neighbour lput x neighbour]

  ] ; radimo sve dok ima brojeva oko praznog mjesta


end

to astar-pretraga
  set neighbour [] ;lista u koju ćemo spremati čvorove koji se dobiju kada proširimo jedan čvor


if length(stanja) = 1 [
let element (list lista [0])
set stanja remove first stanja stanja
set open lput (list lista (racunaj-heuristiku lista + racunaj-udaljenost element) [0] ) open ] ; izvrsava se samo u prvoj iteraciji, stavlja prvi element na open listu


while [not (open = []) ] [
  set open sort-by [item 1 ?1 < item 1 ?2] open ; sortira elemente u open listi tako da na pocetku budu s manjom vrijednosti

  let current first open ;promatramo onoga s najmanjom vrijednosti
  if first current = [1 2 3 4 5 6 7 8 0] [
    set stanja lput current stanja
    stop
    ]; ako je on rješenje, stavimo ga na stanja i zaustavimo pretragu

  set open remove current open ;maknemo trenutni s open liste i stavimo ga na closed listu
  set closed lput current closed

  astar-korak current ; obavimo korak za trenutni
  tick

  foreach neighbour [
    if not member? ?  closed and not member? ? open[
      set open fput ? open]
  ] ; za svaki cvor iz sljedeceg koraka gledamo je li na open ili closed, ako nije stavljamo ga na open
  set stanja lput current stanja ;stavljamo trenutno promatrano stanje na kraj liste stanja

  ]


end




to ida-korak [element]
  find-moves (first element)

  while [length oko-nule > 0] [
    let trenutna-lista first element ;lista u kojoj je prikazano stanje
    let moves last element ; lista u kojoj su prikazani dosadasnji potezi
    let broj first oko-nule ;uzimamo prvi broj za zamjenu
    let potez first pokreti ;uzimamo i prvi potez, koji odgovara prvom broju

    let trenutna-poz 0
    foreach trenutna-lista [if ? = broj [set trenutna-poz position ? trenutna-lista]] ; trazimo trenutnu poziciju broja koji gledamo
    set trenutna-lista replace-item prazno trenutna-lista (broj) ; na mjesto 0 stavljamo broj
    set trenutna-lista replace-item trenutna-poz trenutna-lista 0 ; na mjesto broja stavljamo 0
    set moves lput potez moves ;na listu poteza stavljamo potez koji smo napravili da dobijemo novo stanje
    set oko-nule remove (first oko-nule) oko-nule ; mičemo broj s liste
    set pokreti remove (first pokreti) pokreti ; mičemo pokret s liste

    let udaljenost racunaj-heuristiku trenutna-lista
    let x (list trenutna-lista udaljenost moves)
    if provjera trenutna-lista = 0 [set neighbour lput x neighbour]]

end


to ida-algoritam
  if test-cilja = 1 [stop]
  let x granica
  let udaljenosti []

  if length (stanja) = 1 [
   let u racunaj-heuristiku first first stanja
   set stanja replace-item 0 stanja (list lista u [0])
    ] ;računamo heuristiku za početno stanje


    foreach stanja [
      set x item 1 ?
      if (x <= granica) [ida-korak ?
      ]
    ]
    foreach stanja [if (item 1 ? > granica) [set udaljenosti fput item 1 ? udaljenosti]]
    set granica min udaljenosti
end












to ida-dubina-korak [element]
  find-moves (first element)

  while [length oko-nule > 0] [
    let trenutna-lista first element ;lista u kojoj je prikazano stanje
    let moves last element ; lista u kojoj su prikazani dosadasnji potezi

  ;let lista1 trenutna-lista
    let broj first oko-nule ;uzimamo prvi broj za zamjenu
    let potez first pokreti ;uzimamo i prvi potez, koji odgovara prvom broju

    let trenutna-poz 0
    foreach trenutna-lista [if ? = broj [set trenutna-poz position ? trenutna-lista]] ; trazimo trenutnu poziciju broja koji gledamo
    set trenutna-lista replace-item prazno trenutna-lista (broj) ; na mjesto 0 stavljamo broj
    set trenutna-lista replace-item trenutna-poz trenutna-lista 0 ; na mjesto broja stavljamo 0
    set moves lput potez moves ;na listu poteza stavljamo potez koji smo napravili da dobijemo novo stanje
    set oko-nule remove (first oko-nule) oko-nule ; mičemo broj s liste
    set pokreti remove (first pokreti) pokreti ; mičemo pokret s liste
  ;set stanja lput (list lista1 broj-koraka) stanja
  if (provjera trenutna-lista) = 0 [ set stanja fput (list trenutna-lista (racunaj-heuristiku trenutna-lista + racunaj-udaljenost (list trenutna-lista moves)) 0 moves)  stanja]
  ;dodajemo stanje (raspored brojeva + potezi do tu) u listu stanja
  ] ; radimo sve dok ima brojeva oko praznog mjesta

  let y position element stanja
  if not (y = false) [set stanja remove-item y stanja
  set stanja lput (list first element (racunaj-heuristiku (first element)+ racunaj-udaljenost element) 1 last element) stanja]
end


to ida-pretraga
  if test-cilja = 1 [stop]
  let udaljenosti []

  if length(stanja) = 1 [
let element (list lista [0])
set stanja remove first stanja stanja
set stanja fput (list lista (racunaj-heuristiku lista + racunaj-udaljenost element) 0 [0] ) stanja
] ; početnom stanju dodaje heurističku vrijednost te postavlja početnu granicu na tu vrijednost

  let x first stanja ; uzimamo prvi element sa stanja jer je to zadnji otvoreni

  while [item 2 x = 0][ ;dok god prvi od stanja nije posjećen

    set x first stanja ; uzmemo prvi iz stanja
  ifelse not ((item 1 x) > granica) [ida-dubina-korak x tick] ;ako vrijednost funkcije tog stanja nije presla granicu, otvaramo njemu sljedeće čvorove i (u koraku) stavljamo ih na početak liste stanja
  [
    set stanja remove x stanja
    set udaljenosti lput item 1 x udaljenosti
    ] ;ako je vrijednost prešla granicu to stanje mičemo s liste stanja, a vrijednost dodajemo u listu gdje pratimo vrijednosti za sljedeću granicu
  show stanja
  ]

  ifelse udaljenosti = [] [set stanja (list (list lista (racunaj-heuristiku lista + racunaj-udaljenost (list lista [0])) 0 [0] )) ][set granica min udaljenosti] ; nakon što smo prošli kroz sve otvorene čvorove, stavljamo granicu na najmanju od njih


end
@#$#@#$#@
GRAPHICS-WINDOW
739
10
1049
341
-1
-1
100.0
1
10
1
1
1
0
1
1
1
0
2
-2
0
0
0
1
ticks
30.0

BUTTON
96
88
245
121
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1300
165
1428
198
NIL
pretraga-po-sirini
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1300
213
1430
246
dubina
pretraga-po-dubini
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
11
234
275
279
put
pronađi-put
17
1
11

CHOOSER
20
29
158
74
vrsta-slagalice
vrsta-slagalice
"1" "2" "3" "4" "5" "6" "7" "8" "9" "10"
3

MONITOR
284
234
353
279
broj poteza
length (pronađi-put) - 1
17
1
11

BUTTON
93
298
251
338
prikaži rješenje
pomakni
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
17
463
1078
508
NIL
stanja
17
1
11

TEXTBOX
408
45
682
213
Upute:\n1. odaberite slagalicu (broj 1 - 10)\n2. odaberite dubinu (pretraga po dubini i pohlepni algoritam su ograničeni po dubini)\n3. postavite svijet (setup)\n4. odaberite algoritam i pritisnite \"korak\"\n5. odaberite \"prikaži rješenje\" za vizualnu reprezentaciju rješenja (ako postoji, u protivnom se javlja poruka)\n6. za pokrenuti novi algoritam ponoviti korake 1 - 5\n
11
0.0
1

BUTTON
1301
255
1432
288
pohlepna
pohlepna-pretraga
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
191
32
363
65
dubina
dubina
1
25
16
1
1
NIL
HORIZONTAL

BUTTON
1303
305
1435
338
astar
astar-pretraga
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
95
134
247
179
algoritam
algoritam
"pretraga-po-sirini" "pretraga-po-dubini" "pohlepna-pretraga" "astar-pretraga" "ida-algoritam"
2

BUTTON
97
191
249
224
korak
ifelse test-cilja = 1 [stop][\nkorak]\nifelse pronađi-put = 0 [set pronađen-put false] [set pronađen-put true]
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1305
353
1439
386
NIL
ida-pretraga
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1332
437
1418
482
NIL
granica
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

cetiri
true
0
Rectangle -1 true false 45 45 255 255
Rectangle -16777216 true false 90 60 105 240
Polygon -16777216 true false 135 60 165 240 195 240 225 60 210 60 180 240 150 60 135 60
Rectangle -16777216 true false 60 240 75 240
Rectangle -16777216 true false 165 225 195 240

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

dva
true
0
Rectangle -1 true false 45 45 255 255
Rectangle -16777216 true false 120 60 135 240
Rectangle -16777216 true false 165 60 180 240

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

jedan
true
0
Rectangle -1 true false 45 45 255 255
Rectangle -16777216 true false 135 60 150 240

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

one
true
0
Rectangle -1 true false 45 45 255 255
Rectangle -16777216 true false 165 90 180 225
Polygon -16777216 true false 165 90 120 120 120 135 165 105

osam
true
0
Rectangle -1 true false 45 45 255 255
Polygon -16777216 true false 60 60 90 240 120 240 150 60 135 60 105 240 75 60
Rectangle -16777216 true false 165 60 180 240
Rectangle -16777216 true false 195 60 210 240
Rectangle -16777216 true false 225 60 240 240
Rectangle -16777216 true false 90 225 120 240

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

pet
true
0
Rectangle -1 true false 45 45 255 255
Polygon -16777216 true false 105 60 135 240 165 240 195 60 180 60 150 240 120 60 105 60
Rectangle -16777216 true false 60 240 75 240
Rectangle -16777216 true false 135 225 165 240

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sedam
true
0
Rectangle -1 true false 45 45 255 255
Polygon -16777216 true false 60 60 90 240 120 240 150 60 135 60 105 240 75 60
Rectangle -16777216 true false 165 60 180 240
Rectangle -16777216 true false 195 60 210 240
Rectangle -16777216 true false 90 225 120 240

sest
true
0
Rectangle -1 true false 45 45 255 255
Polygon -16777216 true false 75 60 105 240 135 240 165 60 150 60 120 240 90 60
Rectangle -16777216 true false 195 60 210 240
Rectangle -16777216 true false 105 225 135 240

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

tri
true
0
Rectangle -1 true false 45 45 255 255
Rectangle -16777216 true false 105 60 120 240
Rectangle -16777216 true false 135 60 150 240
Rectangle -16777216 true false 165 60 180 240

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Experiment 1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>korak</go>
    <exitCondition>test-cilja = 1</exitCondition>
    <metric>length (stanja)</metric>
    <metric>pronađen-put</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="algoritam">
      <value value="&quot;pretraga-po-sirini&quot;"/>
      <value value="&quot;pretraga-po-dubini&quot;"/>
      <value value="&quot;pohlepna-pretraga&quot;"/>
      <value value="&quot;astar-pretraga&quot;"/>
      <value value="&quot;ida-algoritam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vrsta-slagalice">
      <value value="&quot;1&quot;"/>
      <value value="&quot;2&quot;"/>
      <value value="&quot;3&quot;"/>
      <value value="&quot;4&quot;"/>
      <value value="&quot;5&quot;"/>
      <value value="&quot;6&quot;"/>
      <value value="&quot;7&quot;"/>
      <value value="&quot;8&quot;"/>
      <value value="&quot;9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dubina">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Experiment 2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>korak</go>
    <exitCondition>test-cilja = 1</exitCondition>
    <metric>length (stanja)</metric>
    <metric>pronađen-put</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="algoritam">
      <value value="&quot;pretraga-po-dubini&quot;"/>
      <value value="&quot;pohlepna-pretraga&quot;"/>
      <value value="&quot;astar-pretraga&quot;"/>
      <value value="&quot;ida-algoritam&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vrsta-slagalice">
      <value value="&quot;1&quot;"/>
      <value value="&quot;2&quot;"/>
      <value value="&quot;3&quot;"/>
      <value value="&quot;4&quot;"/>
      <value value="&quot;5&quot;"/>
      <value value="&quot;6&quot;"/>
      <value value="&quot;7&quot;"/>
      <value value="&quot;8&quot;"/>
      <value value="&quot;9&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dubina">
      <value value="11"/>
      <value value="12"/>
      <value value="13"/>
      <value value="14"/>
      <value value="15"/>
      <value value="16"/>
      <value value="17"/>
      <value value="18"/>
      <value value="19"/>
      <value value="20"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@

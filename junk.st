img:= GdiplusImage fromFile: 'C:\users\pletts\Pictures\testspot.bmp'
img flipY
img flipX
img rotateLeft

clock:= GdiplusImage fromFile: 'C:\users\pletts\Pictures\clock.bmp'
clock graphics
tmp:= GdiplusGraphics fromImage: clock
tmp drawImage: clock

Canvas forDesktop

clock:= SpotSymbol for: 'C:\users\pletts\Pictures\clock.bmp'
test:= SpotSymbol for: 'C:\users\pletts\Pictures\spottest.bmp'
clock image pageScale

more:= SpotDeck with: (Array with: clock with: test) named: 'First'
more drawRandom
answer:= nil
1 to: 8 do: [:each | Transcript show: (((Random new peek * 1000 asInteger at: 1) rem: 15) printString); cr]
1 to: 55 do: [:each | Transcript show: ((Time microsecondClockValue rem: 15 )printString); cr]

SpotDeck testDeck symbols anyOne path
SpotCard drawFrom: (SpotDeck testDeck) limit: 8

1 to: 100 do: [:each |
	pair:= SpotCardPair drawFrom: (SpotDeck testDeck) limit: 8.
	Transcript show: (pair left symbols symmetricDifference: pair right symbols) size printString; cr
]
#(1 2 3 4 5 6 7 8) randomizeUsing: Random new

(#(1 2 3 4) symmetricDifference: #(1 2 3 4 5 6 7 8)) anyOne
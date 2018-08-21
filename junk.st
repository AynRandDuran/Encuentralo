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

SpotDeck testDeck drawRandom path
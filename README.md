# Encuentralo
An adaptation of the card game Spot It for use in classrooms with custom symbols, providing a fun and competetive method of practicing vocabulary.

## Use
A Windows executable is available [over in releases](https://github.com/AynRandDuran/Encuentralo/releases). For full functionality, ensure that the file DolphinDR7.dll is in the same directory as your Encuentralo executable.

Simply start the game with the provided executable and use the deck picker to browser for folders containing the images you'd like to play with. Once you've selected your decks and are ready, click play. After this, you can begin playing in the new window that appears. Click 'Cycle' to repeatedly flip new pairs of cards, from left to right, and try to be the first to identify which symbol both cards have in common. The game will cycle between no cards, left, and both being revealed with each button press. Each card pair will only **ever** have a single symbol in common.

When creating decks, here are some tips:
* A minimum image resolution of 100x100 is recommended. There is no particular limit on resolution but below this, quality may suffer notably.
* Relatively square aspect ratios. All images are forced to 1:1 when play begins, they may look bad otherwise.
* Supported image types are BMP, JPG, and PNG.
* Provide at least 15 total images when playing. The game will not begin if your selected decks have less, though there is no upper limit.

There is no limit on how long you can play. Given enough images are provided, there can be *many* unique cards.
## Development
PRs are welcome; active development has fallen to the wayside but I'd like to keep working on this. Simply install the provided package and you should be good to go to mess with it.
Developed with Dolphin v7.0.57.2
### Load order
If you'd really rather load the classes yourself, here's the load order to file in:

1. DirectoryCollectionPresenter.cls
2. State.cls
3. SpotSymbol.cls
4. SpotCard.cls
5. SpotDeck.cls
6. SpotCardPair.cls
7. SpotDeckGenerator.cls
8. SpotDeckGame.cls
9. EncuentraloState.cls
10. EncuentraloStateClear.cls
11. EncuentraloStateLeft.cls
12. EncuentraloStateBoth.cls
13. EncuentraloPlayer.cls
14. Shell.cls
15. EncuentraloAbout.cls (Not 100% on this one but should be good)
16. Encuentralo.cls
17. EncuentraloSessionManager.cls

The game can be started with the message `Encuentralo new`

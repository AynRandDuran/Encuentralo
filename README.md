# Encuentralo
An attempt at a computerized version of the card game Spot It for use in class rooms with custom symbols.
Please don't sue me. 

## Use
A Windows executable is available [over in releases](https://github.com/AynRandDuran/Encuentralo/releases), but I guess if you really want just throw every class in a package and deploy it yourself, or just start it in a workspace with `Encuentralo new`.

Just give it a good ol' double-click and you're good to go. Use the folder selector to add folders of images with which to play. BMP, PNG, and JPG images are supported.

Any resolution can be used but I would recommend no less than 100x100 for the sake of it not looking terrible. Images are forced to be square to make arranging them easier, so avoid particularly rectangular images or they'll look weird. Use at least 15 total images for proper play, though there is no upper limit.

After that, just click play to start. Click the cycle button to "draw" cards until you're done playing.

## Development
PRs are welcome; active development has fallen to the wayside but I'd like to keep working on this.

Current version of Dolphin is v7.1.3-61
### Load order
To develop yourself, here's the load order to file in:

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
15. Encuentralo.cls
16. EncuentraloSessionManager.cls

# RhChess
A small haskell tool to list the possible moves of a chess variant.
The charecter w reprents a white piece and the charecter b represents a black piece.
## Piece charecters
- **r**: rook
- **n**: knight
- **b**: bishop
- **q**: queen
- **k**: king
- **e**: empty
## Board format
The board is divided into rows an columns. Rows are spereated by newlines, columns are seperated by spaces.
Each column contains 2 charecters (cp with c being the color and p being the piece type),
each row contains 8 columns and the board contains 8 rows. Empty columns are represented by ee.
## Example
this variation
```
ee ee ee ee ee ee ee ee
ee ee ee ee ee ee ee ee
ee ee ee ee ee ee ee ee
ee ee ee bq ee ee ee ee
ee ee ee ee ee ee ee ee
ee ee ee ee ee ee ee ee
ee ee ee ee bn ee ee ee
ee ee ee ee ee ee ee ee
```
gives out this image

![Alt text](https://raw.githubusercontent.com/RHL120/RhChess/master/test.svg "example")
##Side note
For now I have not written a proper user interface so the board should be in ./test
and the output will always be test.svg

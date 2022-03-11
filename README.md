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
this is the board format for the initial point of a chess game:
```
br bn bb bq bk bb bn br
bp bp bp bp bp bp bp bp
ee ee ee ee ee ee ee ee
ee ee ee ee ee ee ee ee
ee ee ee ee ee ee ee ee
ee ee ee ee ee ee ee ee
wp wp wp wp wp wp wp wp
wr wn wb wq wk wb wn wr
```

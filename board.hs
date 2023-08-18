type Square = Maybe Piece
type Board = [[Square]]

startBoard = unlines ["rnbqkbnr",
                      "pppppppp",
                      "        ",
                      "        ",
                      "        ",
                      "        ",
                      "PPPPPPPP",
                      "RNBQKBNR"]

readBoard :: String -> Board
readBoard = map readRow . lines
  where readRow = map readSquare

showBoard :: Board -> String
showBoard = unlines . map showRow
  where showRow = map showSquare

{-
    Apresentação do tabuleiro, mostrando uma peça em posições ocupadas ou " " em
  espaços vazios
}-

--           ' '       showPiece         (Square -> Char)
-- maybe :: Char -> (Piece -> Char) -> (Maybe Piece -> Char)
showSquare :: Square -> Char
showSquare = maybe ' ' showPiece

readSquare :: Char -> Square
readSquare ' ' = Nothing
readSquare c = Just (readPiece c)

data Piece = Piece PColor PType deriving (Show)
data PColor = White | Black deriving (Show)
data PType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show)

{-
    Implementação das peças no tabuleiro utilizando a notação de Forsyth-Edward
      *Peças brancas em MAIUSCULAS "PNBRQK"
      *Peças pretas  em minusculas "pnbrqk"
}-

showPiece :: Piece -> Char
showPiece (Piece White Pawn)   = 'P'
showPiece (Piece White Knight) = 'N'
showPiece (Piece White Bishop) = 'B'
showPiece (Piece White Rook)   = 'R'
showPiece (Piece White Queen)  = 'Q'
showPiece (Piece White King)   = 'K'
showPiece (Piece Black Pawn)   = 'p'
showPiece (Piece Black Knight) = 'n'
showPiece (Piece Black Bishop) = 'b'
showPiece (Piece Black Rook)   = 'r'
showPiece (Piece Black Queen)  = 'q'
showPiece (Piece Black King)   = 'k'

readPiece :: Char -> Piece
readPiece 'P' = (Piece White Pawn)
readPiece 'N' = (Piece White Knight)
readPiece 'B' = (Piece White Bishop)
readPiece 'R' = (Piece White Rook)
readPiece 'Q' = (Piece White Queen)
readPiece 'K' = (Piece White King)
readPiece 'p' = (Piece Black Pawn)
readPiece 'n' = (Piece Black Knight)
readPiece 'b' = (Piece Black Bishop)
readPiece 'r' = (Piece Black Rook)
readPiece 'q' = (Piece Black Queen)
readPiece 'k' = (Piece Black King)

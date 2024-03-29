import Data.Char (ord)
import Data.Char (chr)
type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])




setBoard::Board
setBoard = (White, [
    R ('h',1), N ('g',1), B ('f',1), Q ('d',1), K ('e',1), B ('c',1), N ('b',1), R ('a',1),
    P ('g',2), P ('h',2), P ('f',2), P ('e',2), P ('d',2), P ('c',2), P ('b',2), P ('a',2)],
    [R ('h',8), N ('g',8), B ('f',8), Q ('d',8), K ('e',8), B ('c',8), N ('b',8), R ('a',8),
    P ('h',7), P ('g',7), P ('f',7), P ('e',7), P ('d',7), P ('c',7), P ('b',7), P ('a',7)])


isLegal:: Piece -> Board -> Location -> Bool
isLegal (P oldLoc) board loc = isValidMovePawn board oldLoc loc
isLegal (N oldLoc) board loc = isValidMoveKnight board oldLoc loc
isLegal (K oldLoc) board loc = isValidMoveKing board oldLoc loc
isLegal (R oldLoc) board loc = isValidMoveRook board oldLoc loc
isLegal (B oldLoc) board loc = isValidMoveBishop board oldLoc loc
isLegal (Q oldLoc) board loc = isValidMoveQueen board oldLoc loc


isValidMovePawn :: Board -> Location -> Location -> Bool
isValidMovePawn (player, whitePiecesLocations , blackPiecesLocations) (cOld, iOld) (cNew, iNew) =  
    if player == White
        then
            isWithinBoard iOld iNew cOld cNew
            && isLocationTrue whitePiecesLocations (cOld, iOld)
            && (iNew > iOld)
            && ((abs (iNew - iOld) == 1) || ( abs (iNew - iOld) == 2 && (iOld == 2 || iOld == 7) 
                            && areColCellsEmpty (player, whitePiecesLocations , blackPiecesLocations) (cOld, iOld) (cOld, iOld) (cNew, iNew) ))
            && (((getDifference cNew cOld == 0) && (isLocationEmpty whitePiecesLocations (cNew, iNew)) && (isLocationEmpty blackPiecesLocations (cNew, iNew)))
                || ((getDifference cNew cOld == 1) && ((not (isLocationEmpty blackPiecesLocations (cNew, iNew))))))
    else
        isWithinBoard iOld iNew cOld cNew
        && isLocationTrue blackPiecesLocations (cOld, iOld)
        && (iNew < iOld)
         && ((abs (iNew - iOld) == 1) || ( abs (iNew - iOld) == 2 && (iOld == 2 || iOld == 7) 
                            && areColCellsEmpty (player, whitePiecesLocations , blackPiecesLocations) (cOld, iOld) (cOld, iOld) (cNew, iNew) ))
        && (((getDifference cNew cOld == 0) && (isLocationEmpty whitePiecesLocations (cNew, iNew)) && (isLocationEmpty blackPiecesLocations (cNew, iNew)))
        || (getDifference cNew cOld == 1) && (not(isLocationEmpty whitePiecesLocations (cNew, iNew))))

isValidMoveKnight :: Board -> Location -> Location -> Bool
isValidMoveKnight (player, whitePiecesLocations , blackPiecesLocations) (cOld, iOld) (cNew, iNew) = 
    if player == White
        then
            isWithinBoard iOld iNew cOld cNew
            && isLocationTrue whitePiecesLocations (cOld, iOld)
            && ((getDifference cNew cOld * abs(iNew-iOld)) == 2)
            && isLocationEmpty whitePiecesLocations (cNew, iNew)
    else if player == Black
        then
            isWithinBoard iOld iNew cOld cNew
            && isLocationTrue blackPiecesLocations (cOld, iOld)
            && isLocationTrue blackPiecesLocations(cOld, iOld)
            && ((getDifference cNew cOld * abs(iNew-iOld)) == 2)
            && isLocationEmpty blackPiecesLocations (cNew, iNew)
    else
        False

isValidMoveKing :: Board -> Location -> Location -> Bool
isValidMoveKing (player, whitePiecesLocations , blackPiecesLocations) (cOld,iOld) (cNew, iNew) =
    if player == White
        then
            isWithinBoard iOld iNew cOld cNew
            && isLocationTrue whitePiecesLocations (cOld, iOld)
            && (abs (iNew - iOld) <= 1 &&  abs (getDifference cNew cOld) <= 1)
            && isLocationEmpty whitePiecesLocations (cNew,iNew)
    else
        if player == Black
            then
                isWithinBoard iOld iNew cOld cNew
                && isLocationTrue blackPiecesLocations (cOld, iOld)
                && (abs (iNew - iOld) <= 1 &&  abs (getDifference cNew cOld) <= 1)
                && isLocationEmpty blackPiecesLocations (cNew,iNew)
    else
        False

--same row or same column
isValidMoveRook :: Board -> Location -> Location -> Bool
isValidMoveRook (player, whitePiecesLocations , blackPiecesLocations) (cOld,iOld) (cNew, iNew) =
    if player == White
        then
            isWithinBoard iOld iNew cOld cNew 
            && isLocationTrue whitePiecesLocations (cOld, iOld)
            && (cNew == cOld && not(iNew == iOld) && areColCellsEmpty (player, whitePiecesLocations , blackPiecesLocations) (cOld,iOld) (cOld, iOld) (cNew, iNew))
            || (not(cNew == cOld) && (iNew == iOld) && areRowCellsEmpty (player, whitePiecesLocations , blackPiecesLocations) (cOld, iOld) (cOld,iOld) (cNew, iNew))

            && isLocationEmpty whitePiecesLocations (cNew, iNew)
    else
            isWithinBoard iOld iNew cOld cNew 
            && isLocationTrue blackPiecesLocations (cOld, iOld)
            && (cNew == cOld && not(iNew == iOld) && areColCellsEmpty (player, whitePiecesLocations , blackPiecesLocations) (cOld,iOld) (cOld, iOld) (cNew, iNew)) 
                    || (not(cNew == cOld) && (iNew == iOld) 
                    && areRowCellsEmpty (player, whitePiecesLocations , blackPiecesLocations) (cOld, iOld) (cOld,iOld) (cNew, iNew) )
            && isLocationEmpty blackPiecesLocations (cNew, iNew)

isValidMoveBishop :: Board -> Location -> Location -> Bool
isValidMoveBishop (player, whitePiecesLocations , blackPiecesLocations) (cOld,iOld) (cNew, iNew) =
    if player == White
        then
            isWithinBoard iOld iNew cOld cNew 
            && isLocationTrue whitePiecesLocations (cOld, iOld)
            && abs (iNew - iOld) == abs(getDifference cNew cOld)
            && areDiagonalCellsEmpty (player, whitePiecesLocations , blackPiecesLocations) (cOld, iOld) (cOld, iOld) (cNew, iNew)
            && isLocationEmpty whitePiecesLocations (cNew, iNew)
    else
            isWithinBoard iOld iNew cOld cNew 
            && isLocationTrue blackPiecesLocations (cOld, iOld)
            && abs (iNew - iOld) == abs(getDifference cNew cOld)
            && areDiagonalCellsEmpty (player, whitePiecesLocations , blackPiecesLocations) (cOld, iOld) (cOld, iOld) (cNew, iNew)
            && isLocationEmpty blackPiecesLocations (cNew, iNew)


isValidMoveQueen :: Board -> Location -> Location -> Bool
isValidMoveQueen board oldLoc newLoc = 
    isValidMoveBishop board oldLoc newLoc || isValidMoveRook board oldLoc newLoc




getDifference :: Char -> Char -> Int
getDifference c1 c2 = abs (ord c1 - ord c2)

isValidChar :: Char -> Bool
isValidChar c = (ord c < ord 'i') && (ord c >= ord 'a')

isValidInt :: Int -> Bool
isValidInt c = (c >= 0 && c <= 8)

isWithinBoard :: Int -> Int -> Char -> Char -> Bool
isWithinBoard iOld iNew cOld cNew = (isValidInt iOld) && (isValidInt iNew) && (isValidChar cNew) && (isValidChar cOld)

isLocationEmpty :: [Piece] -> Location -> Bool
isLocationEmpty [] _ = True
isLocationEmpty (piece : xs) loc =
  case piece of
    P pLoc -> if loc == pLoc then False else isLocationEmpty xs loc
    N pLoc -> if loc == pLoc then False else isLocationEmpty xs loc
    K pLoc -> if loc == pLoc then False else isLocationEmpty xs loc
    Q pLoc -> if loc == pLoc then False else isLocationEmpty xs loc
    R pLoc -> if loc == pLoc then False else isLocationEmpty xs loc
    B pLoc -> if loc == pLoc then False else isLocationEmpty xs loc

-- check if location is in player's pieces
isLocationTrue :: [Piece] -> Location -> Bool
isLocationTrue [] loc = False
isLocationTrue (piece : xs) loc =
  case piece of
    P pLoc -> if loc == pLoc then True else isLocationTrue xs loc
    N pLoc -> if loc == pLoc then True else isLocationTrue xs loc
    K pLoc -> if loc == pLoc then True else isLocationTrue xs loc
    Q pLoc -> if loc == pLoc then True else isLocationTrue xs loc
    R pLoc -> if loc == pLoc then True else isLocationTrue xs loc
    B pLoc -> if loc == pLoc then True else isLocationTrue xs loc




areColCellsEmpty :: Board -> Location -> Location -> Location -> Bool
areColCellsEmpty (piece, whitePiecesLocations, blackPiecesLocations) (cOld, iOld) (cOldStore, iOldStore) (cNew, iNew)
  | cOld == cNew && iOld == iNew = True
  | (cOldStore, iOldStore) /= (cOld, iOld) && (not (isLocationEmpty whitePiecesLocations (cOld, iOld)) || not (isLocationEmpty blackPiecesLocations (cOld, iOld))) = False
  | cOld /= cNew = False
  | iNew > iOld = areColCellsEmpty (piece, whitePiecesLocations, blackPiecesLocations) (cOld, iOld + 1) (cOldStore, iOldStore) (cNew, iNew)
  | iOld > iNew = areColCellsEmpty (piece, whitePiecesLocations, blackPiecesLocations) (cOld, iOld - 1) (cOldStore, iOldStore) (cNew, iNew)
  | otherwise = error "Invalid locations"
  
areRowCellsEmpty :: Board -> Location -> Location -> Location -> Bool
areRowCellsEmpty (piece, whitePiecesLocations, blackPiecesLocations) (cOldStore, iOldStore) (cOld, iOld) (cNew, iNew)
  | cOld == cNew && iOld == iNew = True
  | (cOldStore, iOldStore) /= (cOld, iOld) && (not (isLocationEmpty whitePiecesLocations (cOld, iOld)) || not (isLocationEmpty blackPiecesLocations (cOld, iOld))) = False
  | iOld /= iNew =False
  | cNew > cOld = areRowCellsEmpty (piece, whitePiecesLocations, blackPiecesLocations) (cOldStore, iOldStore) (chr ((ord cOld) +1), iOld) (cNew, iNew)
  | cOld > cNew = areRowCellsEmpty (piece, whitePiecesLocations, blackPiecesLocations) (cOldStore, iOldStore) (chr ((ord cOld) -1), iOld) (cNew, iNew)
  | otherwise = error "Invalid locations"  
  
areDiagonalCellsEmpty :: Board -> Location -> Location -> Location -> Bool
areDiagonalCellsEmpty (piece, whitePiecesLocations, blackPiecesLocations) (cOldStore, iOldStore) (cOld, iOld) (cNew, iNew)
  | cOld == cNew && iOld == iNew = True
  | (cOldStore, iOldStore) /= (cOld, iOld) && (not (isLocationEmpty whitePiecesLocations (cOld, iOld)) || not (isLocationEmpty blackPiecesLocations (cOld, iOld))) = False
  | iNew > iOld && cNew > cOld = areDiagonalCellsEmpty (piece, whitePiecesLocations, blackPiecesLocations) (cOldStore, iOldStore) (chr ((ord cOld) +1), iOld + 1) (cNew, iNew)
  | iNew > iOld && cNew < cOld = areDiagonalCellsEmpty (piece, whitePiecesLocations, blackPiecesLocations) (cOldStore, iOldStore) (chr ((ord cOld) -1), iOld + 1) (cNew, iNew)
  | iNew < iOld && cNew > cOld = areDiagonalCellsEmpty (piece, whitePiecesLocations, blackPiecesLocations) (cOldStore, iOldStore) (chr ((ord cOld) +1), iOld - 1) (cNew, iNew)
  | iNew < iOld && cNew < cOld = areDiagonalCellsEmpty (piece, whitePiecesLocations, blackPiecesLocations) (cOldStore, iOldStore) (chr ((ord cOld) -1), iOld - 1) (cNew, iNew)
  | otherwise = error "Invalid locations" 
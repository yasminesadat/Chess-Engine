import Data.Char
import Data.List
type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location| B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])

setBoard :: Board
setBoard=(White,[R('h',1),N('g',1),B('f',1),K('e',1),Q('d',1),B('c',1),N('b',1),R('a',1),P('h',2),P ('g',2),P ('f',2),P ('e',2),P ('d',2),P ('c',2),P ('b',2),P ('a',2)],[R ('h',8),N ('g',8),B('f',8),K ('e',8),Q ('d',8),B ('c',8),N ('b',8),R ('a',8),P ('h',7),P ('g',7),P ('f',7),P ('e',7),P ('d',7),P ('c',7),P ('b',7),P('a',7)])

isLegal:: Piece-> Board-> Location-> Bool
isLegal p b (cE,iE) = abs(ord cE-ord cS)+abs(iE-iS)/=0&&withinBoard (cE,iE) && checkCases (getType p) b (cS,iS)(cE,iE) (abs(ord cS-ord cE)) (abs(iS-iE)) where (cS,iS)=(getLocation p)

checkCases::String->Board->Location->Location->Int->Int->Bool
checkCases "K" b(cS,iS)(cE,iE) dc di= dc<=1&&di<=1 &&not((returnPieceTeam(cS,iS) b)==(returnPieceTeam(cE,iE) b))

checkCases "B" b (cS,iS)(cE,iE) dc di=dc==di&&(if (iE>iS&&cE>cS) then
        (check b(returnPieceTeam (cS,iS) b) (zip [chr(ord cS+1)..cE][iS+1..iE]))else  --up&right
		if (iE>iS&&cE<cS)then (check b(returnPieceTeam (cS,iS) b) (zip [chr (ord cS-1), chr (ord cS-2)..cE][iS+1..iE])) else --up&left
        if(iE<iS&&cE>cS) then  (check b(returnPieceTeam (cS,iS) b) (zip [chr(ord cS+1)..cE][iS-1,iS-2..iE])) else --down&right
        (check b(returnPieceTeam (cS,iS) b) (zip [chr (ord cS-1), chr (ord cS-2)..cE][iS-1,iS-2..iE]))) --down&left
		  
checkCases "R" b (cS,iS)(cE,iE) dc di=(dc==0&&(if (iE-iS)>0 then 
         (check b(returnPieceTeam (cS,iS) b) (zip (replicate (iE-iS) cS)[iS+1..iE]))else  --move up
         (check b(returnPieceTeam (cS,iS) b) (zip (replicate (iS-iE) cS)[iS-1,iS-2..iE])))) --move down
		 ||((di==0) && (if (ord cE - ord cS)>0 then
         (check b (returnPieceTeam (cS,iS) b) (zip  [chr(ord cS+1)..cE] (replicate (ord cE-ord cS)iS)))  --move right
         else(check b (returnPieceTeam (cS,iS) b) (zip  [chr (ord cS-1), chr (ord cS-2)..cE] (replicate (ord cS-ord cE) iS))))) --move left
								 
checkCases "Q" b (cS,iS) (cE,iE) dc di=(checkCases "R" b (cS,iS)(cE,iE) dc di)||(checkCases "B" b (cS,iS)(cE,iE) dc di)	

checkCases "N" b (cS,iS)(cE,iE) dc di=((dc==1&&di==2)||(dc==2&&di==1))&&(not((returnPieceTeam(cS,iS) b)==(returnPieceTeam(cE,iE) b)))

checkCases "P" b (cS,iS)(cE,iE) dc di= (((returnPieceTeam (cS,iS) b)=="White")&&((returnPieceTeam (cE,iE) b)=="Black")&&(iE-iS==1)&&dc==1)
                                 || (((returnPieceTeam (cS,iS) b)=="Black")&&((returnPieceTeam (cE,iE) b)=="White")&&(iS-iE==1)&&dc==1)
                                 || ((dc==0&&(returnPieceTeam (cE,iE) b)=="Empty")&&(if (returnPieceTeam (cS,iS) b)=="White" then
                                 ((check b "White" (zip (replicate (iE-iS)cS)[iS+1..iE]))&&(if iS==2 then (iE==3||iE==4) else iE==iS+1)) else
                                 ((check b "Black" (zip (replicate (iS-iE) cS)[iS-1,iS-2..iE]))&&(if iS==7 then (iE==6||iE==5) else iE==iS-1))))
			
check:: Board->String->[Location]->Bool
check _ _[]=False
check (_,pw,pb) p [h]=if(p=="White") then not(elem h (extractLocations pw)) else not(elem h (extractLocations pb))
check (pl,pw,pb) p (h:t)|(not(elem h (extractLocations pw)))&&(not((elem h (extractLocations pb))))=check(pl,pw,pb) p t
                        |otherwise=False
					   
returnPieceTeam ::Location->Board->String
returnPieceTeam loc (_,p1,p2)=if elem loc(extractLocations p1) then "White" else if elem loc(extractLocations p2) then "Black" else "Empty"
	
suggestMove:: Piece -> Board -> [Location]
suggestMove p b=filter (isLegal p b)[(x,y)|x<-['a'..'h'],y<-[1..8]]

move:: Piece -> Location -> Board -> Board
move p loc (White, boardW, boardB) | (returnPieceTeam (getLocation p) (White, boardW, boardB)) /= "White"=error "This is White player's turn, Black can't move."
                                   | not(isLegal p (White, boardW, boardB) loc)= error("Illegal move for piece "++show p)
								   |otherwise=(Black,[changeLocation p loc]++(delete p boardW),updateBoard boardB loc)

move p loc (Black, boardW, boardB) |(returnPieceTeam (getLocation p) (Black, boardW, boardB)) /= "Black"=error  "This is Black player's turn, White can't move."
                                   | not(isLegal p (Black, boardW, boardB) loc)= error("Illegal move for piece "++show p)
								   |otherwise=(White,updateBoard boardW loc,[changeLocation p loc]++(delete p boardB))
								   
getLocation :: Piece-> Location
getLocation (P loc)= loc 
getLocation (N loc)= loc
getLocation (K loc)= loc
getLocation (Q loc)= loc
getLocation (R loc)= loc
getLocation (B loc)= loc

getType::Piece->String
getType (P loc)= "P"
getType (N loc)= "N"
getType (K loc)= "K"
getType (Q loc)= "Q"
getType (R loc)= "R"
getType (B loc)= "B"

pieceWithSuffix :: [Piece] -> [Piece] -> [Piece] -> String
pieceWithSuffix pieceWhite pieceBlack [p] = if elem p pieceWhite then getType p ++ "W " else getType p ++ "B "

withinBoard (c,i)=elem c ['a'..'h']&& elem i [1..8]

extractLocations :: [Piece]-> [Location]
extractLocations []=[]
extractLocations (P loc:t)= [loc] ++extractLocations t
extractLocations (N loc:t)= [loc] ++extractLocations t
extractLocations (K loc:t)= [loc] ++extractLocations t
extractLocations (Q loc:t)= [loc] ++extractLocations t
extractLocations (R loc:t)= [loc] ++extractLocations t
extractLocations (B loc:t)= [loc] ++extractLocations t

changeLocation::Piece->Location->Piece
changeLocation (P l) loc = P loc
changeLocation (N l) loc = N loc
changeLocation (K l) loc= K loc
changeLocation (Q l) loc= Q loc
changeLocation (R l) loc= R loc
changeLocation (B l) loc= B loc

updateBoard::[Piece]-> Location->[Piece]
updateBoard p loc=filter (\x -> getLocation x/=loc) p

findPiece :: Board -> Location -> String
findPiece (_, pW, pB) loc = if (elem loc (extractLocations pW) || elem loc (extractLocations pB)) then pieceWithSuffix pW pB (filter (\x -> getLocation x==loc) (pW ++ pB)) else "   " 
  
visualizeBoard :: Board -> String
visualizeBoard (p, pW, pB) = unlines (header: rows ++ [turn]) 
  where 
    allPieces = pB ++ pW 
    header = concat (["    "] ++ (intersperse "    " ["a","b","c","d","e","f","g","h"]))
    rows =[show row ++ " | " ++ concat (intersperse "| " [findPiece (p, pW, pB)(col, row) | col <- ['a'..'h']]) ++  "|" | row <- [8,7..1]]
    turn = "\nTurn: " ++ show p


  
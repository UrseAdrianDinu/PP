{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}


import ProblemState
import Data.List

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

instance Show Target where
  show (Target (x, y) _) = "("++show x++","++show y++")"

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.

    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}

type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***

    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Cell = Hunter | TTarget | Obstacle | Gateway | Blank
    deriving (Eq, Ord)

instance Show Cell
    where
      show Hunter = "!"
      show TTarget = "*"
      show Obstacle = "@"
      show Gateway = "#"
      show Blank = " "

data Game = Game
  {
    board :: [Cell],      -- lista de celule pentru a reprezenta tabla de joc
    n :: Int,             -- nr. de linii
    m :: Int,             -- nr. de coloane
    hunter :: Position,   -- pozitia hunter-ului
    targetlist :: [Target], --lista de target - uri
    gatewaylist :: [(Position,Position)]  -- lista de perechi de gateway-uri
  } deriving (Eq, Ord)

{-
    *** Optional ***

    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.

    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
gameAsString :: Game -> String
gameAsString (Game b x y (_,_) _ _) =  intercalate "\n" [concatMap show $ take y $ drop (y * (i - 1) ) b | i <- [1..x]]
instance Show Game where
    show = gameAsString

{-
    *** TODO ***

    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame a b = Game (firstline ++ secondline ++ mid ++ lastline) a b (1, 1) [] [] where
                        firstline = replicate b Obstacle
                        secondline = [Obstacle] ++ [Hunter] ++ replicate ( b - 3 ) Blank ++ [Obstacle]
                        mid = foldl (++) [] (replicate (a - 3) ([Obstacle] ++ replicate ( b - 2) Blank ++ [Obstacle]))
                        lastline = replicate b Obstacle

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}

checkPosition :: Position -> Game -> Int
checkPosition (x, y) (Game b l c _ _ _) =
  let cell = last (take (x * c + y + 1) b) in
   if (x >= 0 && x <= l && y >= 0 && y <= c) then
     if cell == Blank then 1
       else
         if cell == Gateway then 2
           else
             if cell == TTarget then 3
               else -1
    else -1

checkPositioninGateway ::Position -> Game -> Bool
checkPositioninGateway (x, y) (Game _ _ _ (_, _) _ glist) =
  if length (filter (\((x1,y1),(x2,y2)) -> if (x1 == x && y1 == y || x2 == x && y2 == y) then True else False) glist) > 0 then True
  else False

addHunter :: Position -> Game -> Game
addHunter (x, y) (Game b l c (xh, yh) tlist glist)  =
  let dest = checkPosition (x, y) (Game b l c (xh, yh) tlist glist)
      aux = (take (xh * c + yh) b ++ [Blank] ++ drop (xh * c + yh + 1) b)
      gate = (take (xh * c + yh) b ++ [Gateway] ++ drop (xh * c + yh + 1) b)
      src = checkPositioninGateway (xh, yh) (Game b l c (xh,yh) tlist glist) in
      if dest /= -1 then if src then (Game (take (x * c + y) gate ++ [Hunter] ++ drop (x * c + y + 1) gate) l c (x, y) tlist glist)
        else (Game (take (x * c + y) aux ++ [Hunter] ++ drop (x * c + y + 1) aux) l c (x, y) tlist glist)
        else (Game b l c (xh, yh) tlist glist)

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget behav (x,y) (Game b l c (xh,yh) tlist glist) =
  (Game (take (x * c + y) b ++ [TTarget] ++ drop (x * c + y + 1) b) l c (xh, yh) (tlist ++ [Target (x, y) behav]) glist)

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway ((x1 , y1), (x2, y2)) (Game b l c (xh,yh) tlist glist) =
  let aux = (take (x1 * c + y1) b ++ [Gateway] ++ drop (x1 * c + y1 + 1) b)
  in (Game (take (x2 * c + y2) aux ++ [Gateway] ++ drop (x2 * c + y2 + 1) aux ) l c (xh, yh) tlist (glist ++ [((x1,y1),(x2,y2))]))

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}

addObstacle :: Position -> Game -> Game
addObstacle (x, y) (Game b l c (xh, yh) tlist glist)  =
  (Game (take (x * c + y) b ++ [Obstacle] ++ drop (x * c + y + 1) b) l c (xh, yh) tlist glist)

{-
    *** TODO ***

    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove (x,y) (Game b l c (xh,yh) tlist glist) =
  let cell = last (take (x * c + y + 1) b)
      gateways = filter (\((x1,y1),(x2,y2)) -> if (x1 == x && y1 == y || x2 == x && y2 == y) then True else False) glist in
      if cell == Blank then Just (x, y)
        else if cell == Gateway then if ( fst (fst (head gateways)) == x && snd (fst (head gateways)) == y) then Just (fst (snd (head gateways)), snd (snd (head gateways)))
          else Just (fst (fst (head gateways)), snd (fst (head gateways)))
          else if cell == Obstacle then Nothing
            else if checkPositioninGateway (x, y) (Game b l c (xh,yh) tlist glist) then
              if ( fst (fst (head gateways)) == x && snd (fst (head gateways)) == y) then Just (fst (snd (head gateways)), snd (snd (head gateways)))
                else Just (fst (fst (head gateways)), snd (fst (head gateways)))
                else Just (x, y)




{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.

    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.

    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

transformMaybe :: Maybe Position -> Position
transformMaybe (Just (a,b)) = (a,b)
transformMaybe Nothing = (0, 0)

goHelper :: Position -> Position -> Behavior -> Game -> Target
goHelper (x1, y1) (x2, y2) behav (Game b l c (xh, yh) tlist glist) =
  let coord = attemptMove (x1 + x2, y1 + y2) (Game b l c (xh, yh) tlist glist)
      currentpos = attemptMove (x1 ,y1) (Game b l c (xh, yh) tlist glist) in
      if coord == Nothing then
        (Target (transformMaybe currentpos) behav)
        else (Target (transformMaybe coord) behav)



goEast :: Behavior
goEast (x, y) (Game b l c (xh, yh) tlist glist) = goHelper (x, y) (0,1) goEast (Game b l c (xh, yh) tlist glist)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.
-}
goWest :: Behavior
goWest (x, y) (Game b l c (xh, yh) tlist glist) = goHelper (x, y) (0, -1) goWest (Game b l c (xh, yh) tlist glist)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.
-}
goNorth :: Behavior
goNorth (x, y) (Game b l c (xh, yh) tlist glist) = goHelper (x, y) (-1, 0) goNorth (Game b l c (xh, yh) tlist glist)

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud.
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne
    pe loc.
-}
goSouth :: Behavior
goSouth (x, y) (Game b l c (xh, yh) tlist glist) = goHelper (x, y) (1, 0) goSouth (Game b l c (xh, yh) tlist glist)
{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud.
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
goSouthBounce :: Behavior
goSouthBounce (x, y) (Game b l c (xh, yh) tlist glist) =
  let coord = attemptMove (x + 1 , y) (Game b l c (xh, yh) tlist glist)
      temp = attemptMove (x - 1 , y) (Game b l c (xh, yh) tlist glist) in
  if coord == Nothing then if temp == Nothing then (Target (x,y) goSouthBounce)
    else (Target (transformMaybe temp) goNorthBounce)
    else (Target (transformMaybe coord) goSouthBounce)

goNorthBounce :: Behavior
goNorthBounce (x, y) (Game b l c (xh, yh) tlist glist) =
  let coord = attemptMove (x - 1 , y) (Game b l c (xh, yh) tlist glist)
      temp = attemptMove (x + 1 , y) (Game b l c (xh, yh) tlist glist) in
      if coord == Nothing then if temp==Nothing then (Target (x, y) goNorthBounce)
        else (Target (transformMaybe temp) goSouthBounce)
        else (Target (transformMaybe coord) goNorthBounce)


bounce :: Int -> Behavior
bounce direction = if ( direction == 1) then goSouthBounce else goNorthBounce
{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.

-}

moveTargets :: Game -> Game
moveTargets (Game b l c (xh, yh) tlist glist) =
  let targets = map (\(Target (x,y) behav) -> behav (x,y) (Game b l c (xh, yh) tlist glist)) tlist
      aux = foldl (\acc (Target (x,y) _) -> if checkPositioninGateway (x,y) (Game b l c (xh, yh) tlist glist) then
        (take (x * c + y) acc ++ [Gateway] ++ drop (x * c + y + 1) acc)
        else (take (x * c + y) acc ++ [Blank] ++ drop (x * c + y + 1) acc)) b tlist
          in (Game (foldl (\acc (Target (x,y) _) -> (take (x * c + y) acc ++ [TTarget] ++ drop (x * c + y + 1) acc)) aux targets) l c (xh,yh) targets glist)


{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (x,y) (Target (xt,yt) _) =
  if xt + 1 == x && yt == y then True else
    if xt - 1 == x && yt == y then True else
      if yt + 1 == y && xt == x then True else
        if yt - 1 == y && xt == x then True else
          False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.

    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
removeKilledTargets :: Game -> Game
removeKilledTargets (Game b l c (xh, yh) tlist glist) =
  let notKilled = filter (\t -> not (isTargetKilled (xh,yh) t)) tlist
      cleared = foldl (\acc (Target (x,y) _) -> if checkPositioninGateway (x,y) (Game b l c (xh, yh) tlist glist) then
        (take (x * c + y) acc ++ [Gateway] ++ drop (x * c + y + 1) acc)
        else (take (x * c + y) acc ++ [Blank] ++ drop (x * c + y + 1) acc)) b tlist in
        (Game (foldl (\acc (Target (x,y) _) -> (take (x * c + y) acc ++ [TTarget] ++ drop (x * c + y + 1) acc)) cleared notKilled) l c (xh, yh) notKilled glist)

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir val g@(Game b l c (xh, yh) tlist glist)
  | dir == North =
    let coord = attemptMove (xh - 1, yh) (Game b l c (xh, yh) tlist glist) in
    if coord /= Nothing then
      let movedHunter = addHunter (transformMaybe coord) (Game b l c (xh, yh) tlist glist)
          removedTargets1 = removeKilledTargets movedHunter
          movedTargets = moveTargets removedTargets1
          removedTargets2 = removeKilledTargets movedTargets in
      if val == True then removedTargets2 else
        movedHunter
      else
        if val == True then removeKilledTargets $ moveTargets $ removeKilledTargets g
          else g
  | dir == South =
    let coord = attemptMove (xh + 1, yh) (Game b l c (xh, yh) tlist glist) in
    if coord /= Nothing then
      let movedHunter = addHunter (transformMaybe coord) (Game b l c (xh, yh) tlist glist)
          removedTargets1 = removeKilledTargets movedHunter
          movedTargets = moveTargets removedTargets1
          removedTargets2 = removeKilledTargets movedTargets in
      if val == True then removedTargets2 else
        movedHunter
      else
        if val == True then removeKilledTargets $ moveTargets $ removeKilledTargets g
          else g
  | dir == East =
    let coord = attemptMove (xh, yh + 1) (Game b l c (xh, yh) tlist glist) in
    if coord /= Nothing then
      let movedHunter = addHunter (transformMaybe coord) (Game b l c (xh, yh) tlist glist)
          removedTargets1 = removeKilledTargets movedHunter
          movedTargets = moveTargets removedTargets1
          removedTargets2 = removeKilledTargets movedTargets in
      if val == True then removedTargets2 else
        movedHunter
      else
        if val == True then removeKilledTargets $ moveTargets $ removeKilledTargets g
          else g
  | dir == West =
    let coord = attemptMove (xh, yh - 1 ) (Game b l c (xh, yh) tlist glist) in
    if coord /= Nothing then
      let movedHunter = addHunter (transformMaybe coord) (Game b l c (xh, yh) tlist glist)
          removedTargets1 = removeKilledTargets movedHunter
          movedTargets = moveTargets removedTargets1
          removedTargets2 = removeKilledTargets movedTargets in
      if val == True then removedTargets2 else
        movedHunter
      else
        if val == True then removeKilledTargets $ moveTargets $ removeKilledTargets g
          else g
  | otherwise = (Game b l c (xh, yh) tlist glist)


{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft (Game _ _ _ (_,_) tlist _ ) = if tlist == [] then False
  else True

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***

        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = [(North ,advanceGameState North False game)] ++
                      [(South ,advanceGameState South False game)] ++
                      [(East ,advanceGameState East False game)] ++
                      [(West ,advanceGameState West False game)]

    {-
        *** TODO ***

        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal (Game _ _ _ (xh, yh) tlist _) =
      if length ( filter (\t -> (isTargetKilled (xh,yh) t)) tlist) > 0 then True
        else False

    {-
        *** TODO ***

        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h (Game _ _ _ (xh, yh) tlist _) =
      let targets = (filter (\t -> (isTargetKilled (xh,yh) t)) tlist) in
      if length targets > 0 then
        let (Target (x,y) _ ) = head targets in hEuclidean (x,y) (xh, yh)
        else
          let (Target (x,y) _) = head tlist in
          hEuclidean (x,y) (xh, yh)

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică.
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined

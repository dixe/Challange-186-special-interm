import System.Environment
import Debug.Trace
import System.Random
import Data.Array.IO
import Control.Monad
import Data.List
import Data.STRef

data Stats = Stats {sKill :: Int
                   ,dKill :: Int
                   ,vBitten :: Int
                   ,hBitten :: Int
                   ,hMove :: Int
                   ,zMove :: Int
                   ,vMove :: Int
              } deriving (Show)

data Map = Map {zombies :: [(Int,Int)]
               ,hunters :: [(Int,Int)]
               ,victums :: [(Int,Int)]
               } deriving (Show)


data TS = TS {tiles :: Map
             ,stats :: Stats
             ,rand :: StdGen
             } deriving (Show)


defaultStats :: Stats
defaultStats = Stats {sKill =0, dKill =0, vBitten =0, hBitten =0, hMove = 0, zMove = 0, vMove = 0}

-- new empty game
newGame ::[(Int,Int)] -> Int -> Int -> Int -> TS
newGame map x y z = TS {tiles = Map {zombies = take x map
                                ,hunters = take y (drop x map)
                                ,victums = take z (drop (x+y) map)}
                       ,stats = defaultStats 
                       ,rand = mkStdGen 200
                   }

--getCreature :: Ts ->(Map -> []  -> [(Int,Int)]
getCList ts creature= creature (tiles ts)

-- list of every position as in pair   
mapList :: [(Int,Int)]
mapList = foldl(++) [(x,y) | x <- take 20 [0..], y <- take 20 [0..]] []


-- movement step for every creature entry
moveMap :: TS -> TS 
moveMap ts = let zombiemoveTs = foldl(\acc (x,y) -> zombieMove acc x y) ts (zombies (tiles ts))
                 huntermoveTs = foldl(\acc (x,y) -> hunterMove acc x y) zombiemoveTs (hunters (tiles ts))
                 victummoveTs = foldl(\acc (x,y) -> victumMove acc x y) huntermoveTs (victums (tiles ts))
             in victummoveTs


-- move single zombie
zombieMove :: TS -> Int -> Int -> TS
zombieMove ts x y = let targetList = [(i,j) | (i,j) <- hunters (tiles ts) ++ victums (tiles ts), -- potential victums
                                      i>=x-1, i<=x+1, j<=y+1, j>=y-1, x==i || y == j] 
                        moveTS = if targetList /= [] then ts 
                                 else -- move
                                     let moveSpotList = [(i,j) | (i,j) <- mapList, 
                                                         not $ elem (i,j) (hunters (tiles ts) ++ victums (tiles ts) ++ zombies (tiles ts)),
                                                         i>=x-1, i<=x+1, j<=y+1, j>=y-1, x==i || y == j, not (x==i && y==j)] 
                                         (index,gen) = next (rand ts)
                                         moveTs = if moveSpotList == [] then ts
                                                  else let move = moveSpotList!!(mod index (length moveSpotList))
                                                           nZombieList = map (\(a,b) -> if a==x && b==y then move else (a,b)) (zombies (tiles ts))
                                                       -- update zombie move stats
                                                       in ts {tiles = (tiles ts) {zombies = nZombieList} , stats = (stats ts) {zMove = ((zMove (stats ts)) + 1)}, rand = gen}
                                     in moveTs
                    in moveTS


-- move single hunter
hunterMove :: TS -> Int -> Int-> TS
hunterMove ts x y = let targetList = [(i,j) | (i,j) <- zombies (tiles ts), -- potential kills
                                      i>=x-1, i<=x+1, j<=y+1, j>=y-1] 
                        moveTS = if targetList /= [] then ts 
                                 else -- move
                                     let moveSpotList = [(i,j) | (i,j) <- mapList, 
                                                         not $ elem (i,j) (hunters (tiles ts) ++ victums (tiles ts) ++ zombies (tiles ts)),
                                                         i>=x-1, i<=x+1, j<=y+1, j>=y-1, not (x==i && y==j)] 
                                         (index,gen) = next (rand ts)
                                         moveTs = if moveSpotList == [] then ts
                                                  else let move = moveSpotList!!(mod index (length moveSpotList))
                                                           nHunterList = map (\(a,b) -> if a==x && b==y then move else (a,b)) (hunters (tiles ts))
                                                       in ts {tiles = (tiles ts) {hunters = nHunterList} , stats = (stats ts) {hMove = ((hMove (stats ts)) + 1)}, rand = gen}
                                     in moveTs
                    in moveTS

-- move single victum
victumMove :: TS -> Int -> Int -> TS
victumMove ts x y = let dangerList = [(i,j) | (i,j) <- zombies (tiles ts), -- potential danger
                                      i>=x-1, i<=x+1, j<=y+1, j>=y-1, x==i || y== j] 
                        moveTS = if dangerList == [] then ts 
                                 else -- move
                                     let moveSpotList = [(i,j) | (i,j) <- mapList, 
                                                         not $ elem (i,j) (hunters (tiles ts) ++ victums (tiles ts) ++ zombies (tiles ts)),
                                                         i>=x-1, i<=x+1, j<=y+1, j>=y-1, not (x==i && y==j)] 
                                         (index,gen) = next (rand ts)
                                         moveTs = if moveSpotList == [] then ts
                                                  else let move = moveSpotList!!(mod index (length moveSpotList))
                                                           nVictumList = map (\(a,b) -> if a==x && b==y then move else (a,b)) (victums (tiles ts))
                                                       in ts {tiles = (tiles ts) {victums = nVictumList} , stats = (stats ts) {hMove = ((vMove (stats ts)) + 1)}, rand = gen}
                                     in moveTs
                    in moveTS


-- hunters turn
killMap :: TS -> TS 
killMap ts = foldl (\acc (x,y) -> hunterKill acc x y) ts (hunters (tiles ts)) 

-- single hunter kill turn
hunterKill :: TS -> Int -> Int -> TS
hunterKill ts x y = let closeZ = [(i,j) | (i,j) <- zombies (tiles ts), i>=x-1, i<=x+1, j<=y+1,  j>=y-1] -- zombies in range
                        (index1,gen) = next(rand ts)
                        (index2,gen2) = next gen
                        --get the target list
                        firstTarget = if closeZ == [] then (-1,-1) else closeZ!!(mod index1 (length closeZ))
                        restList =  [(i,j) | (i,j) <- closeZ , (i,j) /= firstTarget]
                        targetsList = if restList == [] then [] else (restList!!(mod index2 (length restList))) : [firstTarget]
                        -- remove them from zombie list
                        restZ = [(i,j) | (i,j) <- zombies (tiles ts), not $ elem (i,j) $ targetsList]
                        statTs = if length closeZ >0
                                 then if length closeZ == 1 
                                      then ts {stats = (stats ts) {sKill = ((sKill (stats ts)) + 1)}} -- on killed
                                      else ts {stats = (stats ts) {dKill = ((dKill (stats ts)) + 1)}}   -- two killed
                                 else ts
                    in statTs { tiles = (tiles ts) {zombies = restZ}, rand = gen2} 

-- zombies turn
biteMap ::TS -> TS 
biteMap ts = foldl (\acc (x,y) -> zombieBite acc x y) ts (zombies (tiles ts)) 

-- single zombie bite turn
zombieBite :: TS -> Int -> Int -> TS
zombieBite ts x y = let closeV = [(i,j) | (i,j) <- hunters (tiles ts) ++ victums (tiles ts), -- potential victums
                                                   i>=x-1, i<=x+1, j<=y+1, j>=y-1, x==i || y == j] 
                        -- choose biten
                        (index,gen) = next(rand ts)
                        -- find the on to be bitten
                        bitten = if closeV == [] then (-1,-1) else closeV!!(mod index (length closeV))
                        -- remove bitten
                        survivers =  [(i,j) | (i,j) <- hunters (tiles ts) ++ victums (tiles ts), (i,j) /= bitten]
                        -- make bitten zombies
                        zombiesL =(take 1 closeV) ++ (getCList ts zombies)
                        --Update stats
                        statTs = if length closeV >0
                                 then if elem (head closeV) (getCList ts hunters)
                                      then ts {stats = (stats ts) {hBitten = ((hBitten (stats ts)) + 1)}} -- on killed
                                      else ts {stats = (stats ts) {vBitten = ((vBitten (stats ts)) + 1)}}   -- two killed
                                 else ts
                        -- update biten hunters
                        updatedHTS = statTs {tiles = (tiles statTs) {hunters = [(i,j) | (i,j) <- hunters (tiles ts),  elem (i,j) survivers]}}
                        -- update biten victum
                        updatedVTS = updatedHTS {tiles = (tiles updatedHTS) {victums = [(i,j) | (i,j) <- victums (tiles ts),  elem (i,j) survivers]}}
                       -- update zombies list and random generator 
                    in updatedVTS { tiles = (tiles updatedVTS) {zombies = zombiesL} , rand = gen} 




-- IO FUNCTIONS --
run :: TS -> Int -> IO()
run ts t = if t<1
           then do
             prettyMap ts
             print $ stats ts
           else do
             let nTurn =  biteMap $ killMap $ moveMap ts
             run nTurn (t-1)
                        
                    

data Tile = H | Z | V | E

data Pos = Pos Int Int Tile

instance Eq Pos where
    Pos a b _ == Pos c d _ = a==b && c==d

instance Ord Pos where
    Pos a b _ <= Pos c d _= if a /= c then a<=c
                     else b<d

instance Show Tile where
    show H = "h"
    show Z = "z"
    show V = "v"
    show E = " "

prettyMap :: TS -> IO ()
prettyMap ts = let emptyMap = zip mapList (cycle [E])
                   withZ = map (\(xy,t) -> if elem xy (zombies (tiles ts)) then (xy,Z) else (xy,t)) emptyMap
                   withH = map (\(xy,t) -> if elem xy (hunters (tiles ts)) then (xy,H) else (xy,t)) withZ
                   withV = map (\(xy,t) -> if elem xy (victums (tiles ts)) then (xy,V) else (xy,t)) withH
                   sortedMap = [((a,b),t) |(Pos a b t) <- (sort[(Pos a b t) | ((a,b),t) <- withV])]
                   stringMap =  foldl (\acc ((x,y),t) -> let nAcc = acc ++ (show t)
                                                        in if y == 19 then nAcc ++ "\n" else nAcc) "" sortedMap
               in do
                 putStr stringMap

  
  
  
-- shuffle list with IO
shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1..n] $ \i -> do
               j <- randomRIO (i,n)
               vi <- readArray ar i
               vj <- readArray ar j
               writeArray ar j vi
               return vj
      where
        n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n xs = newListArray (1,n) xs


main = do
  args <- getArgs
  let x = read (args!!0) ::Int
  let y = read (args!!1) ::Int
  let z = read (args!!2) ::Int
  let t = read (args!!3) ::Int
  if  x+y+z > 400
  then print "To many dude x+y+z > 400"
  else do

    --create mape with randomly placed zombies, hunters and victums
    emptyMap <- shuffle mapList    
    let startTurn = newGame emptyMap x y z
    -- get random stdGen
    stdgen <- getStdGen
    --add rand to startTurn
    let sTurn = startTurn {rand = stdgen}
    prettyMap sTurn
    putStrLn "-------------------"
    run sTurn t  
 
import DataFile
import Data.List
----------
remove1 _ [] = []
remove1 x (y:ys) | x == y    = remove1 x ys
                 | otherwise = y : remove1 x ys

find1 x [] = False
find1 x (y:ys) = if x == y then True
                 else find1 x ys
------------
traverse [] =[]
traverse (' ':xs) = ' ':traverse(xs)
traverse (x:xs) = if(find1 x punct==True) then ' ':x:traverse(xs)
                  else x:traverse xs
 
wordToken [] = []
wordToken ((x:xs)) = words(traverse(x:xs))
------------------------------------------------------------		 
wordTokenList [] = []
wordTokenList (x:xs) = wordToken x ++ wordTokenList xs
------------------------------------------------------------
removeDup [] = []
removeDup (x:xs) = x:(removeDup (remove1 x xs))

uniqueBigrams (x:[]) = []
uniqueBigrams (x:y:xs) = removeDup((x,y):uniqueBigrams (y:xs))
  
uniqueTrigrams (x:y:[]) = []
uniqueTrigrams (x:y:z:xs) = removeDup((x,y,z):uniqueTrigrams (y:z:xs))
-------------------------------------------------------------
uniqueBigrams1 (x:[]) = []
uniqueBigrams1 (x:y:xs) = (x,y):uniqueBigrams1 (y:xs)

uniqueTrigrams1 (x:y:[]) = []
uniqueTrigrams1 (x:y:z:xs) = (x,y,z):uniqueTrigrams1 (y:z:xs)

freqGetter []= []

freqGetter (x:xs) = (x,n):freqGetter (remove1 x xs)  where n = ((genericLength (x:xs) - genericLength(remove1 x xs)))


bigramsFreq (x:xs) = (freqGetter (uniqueBigrams1 (x:xs)))

trigramsFreq (x:xs) = (freqGetter (uniqueTrigrams1 (x:xs)))
---------------------------------------------------------------

getFreq x ((a,b):[]) = if(x==a) then b
                           else error "Sorry, it is not possible to infer from current database"
getFreq x ((a,b):ys) | x == a = b
                     | otherwise  = getFreq x ys
--------------------------------------------------------------
generateOneProb ((a,b,c),d) (((x,y),z):[]) =  (d/z)
generateOneProb ((a,b,c),d) (((x,y),z):xs) = if (a,b)==(x,y) then (d/z)
                                                             else generateOneProb ((a,b,c),d) (xs)
---------------------------------------------------------------

genProbPairs  [] (((x,y),z):ys) = []
genProbPairs (((a,b,c),d):xs) (((x,y),z):ys) = ((a,b,c),n): genProbPairs xs (((x,y),z):ys) where n = generateOneProb ((a,b,c),d) (((x,y),z):ys)
  
-------------------------------------------------------------------

counter x [] = []
counter x (((a,b,c),d):xs) = if(x == [a,b]) then ((a,b,c),d):counter (x) (xs)
                           else counter (x) (xs)

checker [] =  error "Sorry, it is not possible to infer from current database"
	
filter2 [] = []	
filter2 (((a,b,c),d):xs) =  if(d>=0.03) then ((a,b,c),d):filter2 xs 
                            else filter2 xs
					
modifiedRandom z (x:xs) = if(n/=length (filter2((counter z (x:xs))))) then n
                          else modifiedRandom z (x:xs) 
						  where n =randomZeroToX(length (filter2((counter z (x:xs)))))

get((a,b,c),d) = c

generateNextWord x (((a,b,c),d):xs) = if((filter2(counter x (((a,b,c),d):xs)))==[]) then checker (filter2(counter x (((a,b,c),d):xs)))

                                      else get((filter2(counter x (((a,b,c),d):xs))) !! (modifiedRandom x (((a,b,c),d):xs)))

---------------------------------------------------------------------
checkIfThere _ _ 0 = True
checkIfThere [x1,x2] y z = if(genericLength(filter2(counter [x1,x2] (y)))==0) then False
																					else checkIfThere [x2,n] y (z-1)
																					where  n = (generateNextWord [x1,x2] y)


generateTextHelper [x1,x2] y 0 = ""
generateTextHelper [x1,x2] y z = if(checkIfThere [x1,x2] y z == False) then error "Sorry, it is not possible to infer from current database"
                                 else (n ++ " ") ++ generateTextHelper [x2,n]  y (z-1) 
                                 where n = (generateNextWord [x1,x2] y)
                           

generateText x y = if (checkIfThere (wordToken x) (trigramsFreq (wordTokenList docs)) y  == False) then error "Sorry, it is not possible to infer from current database"
																							else  removeExtra((x ++ " ") ++ l )
																							where l = generateTextHelper (wordToken x) (trigramsFreq (wordTokenList docs)) y 

removeExtra [] = ""
removeExtra [' '] = ""
removeExtra (x:y:z:xs) = if(x ==' '&& elem y punct) then removeExtra (y:xs)
                       else if(y==' ' && elem z punct) then x:z:removeExtra(xs)
					        else (x:y:z: removeExtra xs)
---------------------------------------------------------------------

sentToken []= [""]
sentToken (x:xs)= if(x == ',') then ".":rest
                 else if(x =='!') then "!":rest
				 else if(x =='?') then "?":rest
                 else (x:head rest):tail rest
				   where rest = sentToken xs








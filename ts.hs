import Data.List
import Data.Char
import System.Environment

type Symbol = String
type Rule = (Symbol, [Symbol])
type NSet = (Symbol, [Symbol])

data Grammar = Grammar {
    nterms :: [Symbol],
    terms :: [Symbol],
    start :: Symbol,
    rules :: [Rule]
} deriving (Show)

newGrammar :: [Symbol] -> [Symbol] -> Symbol -> [Rule] -> Grammar
newGrammar nts ts start rs
    | not (all isUpper $ concat nts) = error "bad non-term"
    | not (all isLower $ concat ts) = error "bad term"
    | not (start `elem` nts) = error "s not in nterms"
    | not (validateRules rs nts ts) = error "bad rules"
    | otherwise = Grammar nts ts start rs

validateRules :: [Rule] -> [Symbol] -> [Symbol] -> Bool
validateRules [] _ _ = True
validateRules (x:xs) nts ts =
    fst x `elem` nts
    && intersect (snd x) (nts ++ ts) == snd x
    && validateRules xs nts ts

---------------------------------------------------------

--removeSimpleRules :: Grammar -> Grammar
--removeSimpleRules g = newGrammar (nterms g) (terms g) (start g) (transformRules (rules g)  (getReachableNonTerms (nterms g) (rules g)))

findSetNStep :: [Symbol] -> [Rule] -> [Symbol]
findSetNStep symbs [] = symbs
findSetNStep symbs (r:rs) =
    (if fst r `elem` symbs && isRuleSimple r && not ((head $ snd r) `elem` symbs)
    then snd r
    else [])
    ++ findSetNStep symbs rs

findSetN :: [Symbol] -> [Rule] -> [Symbol]
findSetN symbs rs =
    if symbs == findSetNStep symbs rs
    then symbs
    else findSetN (findSetNStep symbs rs) rs

getReachableNonTerms :: [Symbol] -> [Rule] -> [NSet]
getReachableNonTerms [] _ = []
getReachableNonTerms (nt:nts) rs = [(nt, findSetN [nt] rs)] ++ getReachableNonTerms nts rs

-- params: 1. original rule list  2. list of Nas
transformRules :: [Rule] -> [NSet] -> [Rule]
transformRules [] _ = []
transformRules (r:rs) sets =
    (if isRuleSimple r
    then []
    else generateNewRules r (map fst (filter (\x -> (fst r) `elem` (snd x)) sets)))
    ++ transformRules rs sets

generateNewRules :: Rule -> [Symbol] -> [Rule]
generateNewRules r [] = []
generateNewRules r (x:xs) = [(x, snd r)] ++ generateNewRules r xs

isRuleSimple :: Rule -> Bool
isRuleSimple r = length (snd r) == 1 && all isUpper (head $ snd r)

---------------------------------------------------------

--transformToCNF :: Grammar -> Grammar
--transformToCNF g = newGrammar (getNonTerminals  (transformRulesToCNF (rules g)) (nterms g)) (terms g) (start g) (transformRulesToCNF (rules g))

getNonTerminals :: [Rule] -> [Symbol] -> [Symbol]
getNonTerminals rs symbs = union (map fst rs) symbs

transformRulesToCNF :: [Rule] -> [Rule]
transformRulesToCNF [] = []
transformRulesToCNF (r:rs) = parseRule r ++ transformRulesToCNF rs

parseRule :: Rule -> [Rule]
parseRule r
    | (length $ snd r) == 1 && all isLower (head $ snd r) = [r]  -- r is like A -> a
    | (length $ snd r) == 2 && (all isUpper $ concat (snd r)) = [r] -- r is like A -> BC
    | (length $ snd r) == 2 = [(fst r, map commify (snd r))]
        ++ generateFinal (snd r !! 0)
        ++ generateFinal (snd r !! 1)-- r is like A -> alpha where len(alpha) == 2
    | (length $ snd r) > 2 = [(fst r, [commify (head $ snd r)] ++ ["<" ++ (concat $ tail $ snd r) ++ ">"])]
        ++ generateFinal (head $ snd r)
        ++ generateCNFRules (concat $ tail $ snd r) -- r is like A -> alpha where len(alpha) > 2
    | otherwise = error "" -- there should be no other rule

generateCNFRules :: String -> [Rule]
generateCNFRules rs
    | length rs > 2 = [("<" ++ rs ++ ">", [commify [head rs], "<" ++ tail rs ++ ">"])]
        ++ generateFinal ([head rs])
        ++ generateCNFRules (tail rs)
    | otherwise = [("<" ++ rs ++ ">", [commify [head rs] ++ commify (tail rs)])]
        ++ generateFinal ([rs !! 0])
        ++ generateFinal ([rs !! 1])

generateFinal :: Symbol -> [Rule]
generateFinal s = if all isLower s then [(s ++ "'", [s])] else []

commify :: Symbol -> Symbol
commify s = if all isLower s then s ++ "'" else s


------------------------------

readAndPrint :: String -> String
readAndPrint s = "readAndPrint"

removeSimpleRules :: String -> String
removeSimpleRules s = "removeSimpleRules"

transformToCNF :: String -> String
transformToCNF s = "transformToCNF"

getInput :: [String] -> String
getInput s = "input"

dispatch :: [(String, [String] -> IO ())]
dispatch =  [("-i", readAndPrint), ("-1", removeSimpleRules), ("-2", transformToCNF)]


main = do
    args <- getArgs
    let inputStr = getInput args
    

    let action = lookup command dispatch
    if action == Nothing
    then error "wrong action"
    else putStrLn $ action inputStr

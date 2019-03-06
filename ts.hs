import Data.List
import Data.List.Split
import Data.Char
import System.Environment
import System.Directory
import System.IO

type Symbol = String
type Rule = (Symbol, [Symbol])
type NSet = (Symbol, [Symbol])

data Grammar = Grammar {
    nterms :: [Symbol],
    terms :: [Symbol],
    start :: Symbol,
    rules :: [Rule]
}

showGrammar :: Grammar -> String
showGrammar g = intercalate "\n" [
    (intercalate "," (nterms g)), 
    (intercalate "," (terms g)),
    (start g),
    intercalate "\n" (map (\x -> fst x ++ "->" ++ (concat $ snd x)) (rules g))
    ]

instance Show Grammar where show = showGrammar

-- the grammar on the input needs to meet the following rules:
-- 1. non-terminals are upper case symbols
-- 2. terminals are lower case symbols
-- 3. starting symbol is a non-terminal
-- 4. rules are in the valid form: the left side of the rule is a known non-terminals
--    and the right side contains known terminals/non-terminals
newGrammar :: [Symbol] -> [Symbol] -> Symbol -> [Rule] -> Grammar
newGrammar nts ts start rs
    | not (all isUpper $ concat nts) = error "bad non-term"
    | not (all isLower $ concat ts) = error "bad term"
    | not (start `elem` nts) = error "s not in nterms"
    | not (validateRules rs nts ts) = error "bad rules"
    | otherwise = Grammar nts ts start rs

newGrammarNoChecks :: [Symbol] -> [Symbol] -> Symbol -> [Rule] -> Grammar
newGrammarNoChecks nts ts start rs = Grammar nts ts start rs

validateRules :: [Rule] -> [Symbol] -> [Symbol] -> Bool
validateRules [] _ _ = True
validateRules (x:xs) nts ts =
    fst x `elem` nts
    && intersect (snd x) (nts ++ ts) == snd x
    && validateRules xs nts ts

---------------------------------------------------------

removeSimpleRules :: Grammar -> Grammar
removeSimpleRules g = newGrammar
    (nterms g)
    (terms g)
    (start g)
    (transformRules (rules g) (getReachableNonTerms (nterms g) (rules g)))

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

getReachableNonTerms :: [Symbol] -> [Rule] -> [NSet]
getReachableNonTerms [] _ = []
getReachableNonTerms (nt:nts) rs = [(nt, findSetN [nt] rs)] ++ getReachableNonTerms nts rs

findSetN :: [Symbol] -> [Rule] -> [Symbol]
findSetN symbs rs =
    if symbs == findSetNStep symbs rs
    then symbs
    else findSetN (findSetNStep symbs rs) rs

findSetNStep :: [Symbol] -> [Rule] -> [Symbol]
findSetNStep symbs [] = symbs
findSetNStep symbs (r:rs) =
    (if fst r `elem` symbs && isRuleSimple r && not ((head $ snd r) `elem` symbs)
    then snd r
    else [])
    ++ findSetNStep symbs rs

isRuleSimple :: Rule -> Bool
isRuleSimple r = length (snd r) == 1 && all isUpper (head $ snd r)

---------------------------------------------------------

transformToCNF :: Grammar -> Grammar
transformToCNF g = newGrammarNoChecks
    (getNonTerminals  (transformRulesToCNF (rules g)) (nterms g))
    (terms g)
    (start g)
    (transformRulesToCNF (rules g))

getNonTerminals :: [Rule] -> [Symbol] -> [Symbol]
getNonTerminals rs symbs = map head $ group $ sort (map fst rs ++ symbs)

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

readGrammar :: String -> Grammar
readGrammar s = newGrammar
    (splitOn "," (lines s !! 0))
    (splitOn "," (lines s !! 1))
    (lines s !! 2)
    (map (\x -> (head x, map (:[]) $ concat (tail x))) (map (splitOn "->") (drop 3 $ lines s)))

readAndPrint :: String -> String
readAndPrint s = show $ readGrammar s

removeSimpleRulesStr :: String -> String
removeSimpleRulesStr s = show $ removeSimpleRules $ readGrammar s

transformToCNFStr :: String -> String
transformToCNFStr s = show $ transformToCNF $ removeSimpleRules $ readGrammar s

dispatch :: [(String, String -> String)]
dispatch =  [("-i", readAndPrint), ("-1", removeSimpleRulesStr), ("-2", transformToCNFStr)]


main = do
    args <- getArgs
    if length args < 1 || length args > 2
    then putStrLn "BAD ARGS"
    else do
        let command = head args
            
        contents <- (if length args == 2
                     then readFile $ last args
                     else getContents)

        case lookup command dispatch of
            Nothing -> putStrLn "ERROR"
            Just a -> putStrLn $ a contents


import Data.List
import Data.List.Split
import Data.Char
import System.Environment
import System.Directory
import System.IO

-- symbol of the grammar
type Symbol = String

-- non-terminal and list of symbols on the right side
type Rule = (Symbol, [Symbol])

-- tuple of non-terminal and it's set of reachable symbols by single rules
type NSet = (Symbol, [Symbol])

-- grammar is: (1.) set of non-terminals, (2.) set of terminals
-- (3.) starting non-terminal, (4.) set of rules
data Grammar = Grammar {
    nterms :: [Symbol],
    terms :: [Symbol],
    start :: Symbol,
    rules :: [Rule]
}

-- define how to stringify grammar
instance Show Grammar where show = showGrammar

-- print the rule
showRule :: Rule -> String
showRule r = fst r ++ "->" ++ concat (snd r)

-- stringify the grammar
showGrammar :: Grammar -> String
showGrammar g = intercalate "\n" [
    intercalate "," (nterms g),
    intercalate "," (terms g),
    start g,
    intercalate "\n" (map (\x -> fst x ++ "->" ++ concat  (snd x)) (rules g))
    ]

-- creates grammar and checks that its input format
-- is used when reading input data
newGrammar :: [Symbol] -> [Symbol] -> Symbol -> [Rule] -> Grammar
newGrammar nts ts start rs
    | not (all isUpper $ concat nts) = error "wrong input format of the non-terminals"
    | not (all isLower $ concat ts) = error "wrong input format of the terminals"
    | start `notElem` nts = error "wrong input format, unknown starting symbol"
    | not (all validateRule rs) = error "wrong input format of the rules"
    | otherwise = Grammar nts ts start rs
    where validateRule r = fst r `elem` nts && intersect (snd r) (nts ++ ts) == snd r

-- generates new grammar with transformed rules so that simple rules are eliminated
removeSimpleRules :: Grammar -> Grammar
removeSimpleRules g = Grammar
    (nterms g)
    (terms g)
    (start g)
    (transformRules (rules g) (getReachableNonTerms (nterms g) (rules g)))
    where getReachableNonTerms [] _ = []
          getReachableNonTerms (nt:nts) rs = (nt, findSetN [nt] rs) : getReachableNonTerms nts rs

-- applies NSet - the set of rules reachable by simple rules
transformRules :: [Rule] -> [NSet] -> [Rule]
transformRules [] _ = []
transformRules (r:rs) sets =
    (if isRuleSimple r
    then []
    else generateNewRules r (map fst (filter (\x -> fst r `elem` snd x) sets)))
    ++ transformRules rs sets
    where generateNewRules r = foldr (\ x -> (++) [(x, snd r)]) []

-- for every non-terminal symbol find all non-terminal symbols,
-- that are reachable by simple rules
findSetN :: [Symbol] -> [Rule] -> [Symbol]
findSetN symbs rs =
    if symbs == nextIteration
    then symbs
    else findSetN nextIteration rs
        where nextIteration = findSetNStep symbs rs

-- one iteration of findSetN
findSetNStep :: [Symbol] -> [Rule] -> [Symbol]
findSetNStep symbs [] = symbs
findSetNStep symbs (r:rs) =
    foldr (\ r -> (++) 
        (if fst r `elem` symbs && isRuleSimple r && (head (snd r) `notElem` symbs)
         then snd r
         else [])
         ) symbs rs

-- a simple rule is in form A->B where B is a non-terminal
isRuleSimple :: Rule -> Bool
isRuleSimple r = length (snd r) == 1 && all isUpper (head $ snd r)

-- removes duplicate values from a list
removeDuplicates :: Ord b => [b] -> [b]
removeDuplicates = map head . group . sort

-- takes a grammar and performs transformation to CNF
transformToCNF :: Grammar -> Grammar
transformToCNF g = Grammar
    (getNonTerminals (transformRulesToCNF g) (nterms g))
    (terms g)
    (start g)
    (transformRulesToCNF g)
        where transformRulesToCNF g = removeDuplicates $ concatMap parseRule (rules g)
              getNonTerminals rs symbs = removeDuplicates (map fst rs ++ symbs)

-- takes one rule and generates corresponding rules based on the CNF algorithm
parseRule :: Rule -> [Rule]
parseRule r
    -- r is like A -> a, just keep this rule
    | length (snd r) == 1 && all isLower (head $ snd r) = [r]
    
    -- r is like A -> BC, just keep this rule
    | length (snd r) == 2 && all isUpper (concat (snd r)) = [r] 
    
    -- r is like A -> bc, we need to generate (1) A -> b'c' and (2) terminal rules (a' -> a)
    | length (snd r) == 2 = [(fst r, map commify (snd r))]
        ++ generateTerminalRule (head (snd r))
        ++ generateTerminalRule (snd r !! 1)
        
    -- r is like A -> abcd, we need to generate (1) A -> a'<bcd> and (2) terminal rule (a' -> a)
    -- and (3) rules which decompose the <bcd> non-terminal
    | length (snd r) > 2 =
        [(fst r, commify (head $ snd r) : ["<" ++ concat (tail $ snd r) ++ ">"])]
        ++ generateTerminalRule (head $ snd r)
        ++ parseComposedNonTerm (concat $ tail $ snd r) 
        
    -- there should be no other type of rule
    | otherwise = error ("bad rule: " ++ showRule r)

-- parse the non-terminal in form <abcd> and return list of rules
parseComposedNonTerm :: String -> [Rule]
parseComposedNonTerm rs
    -- if non-terminal len is > 2, we need (1) <ABCD> -> a'<BCD> and (2) terminal rule a' -> a
    -- and (3) further decompose remaining non-terminal <BCD>
    | length rs > 2 = [("<" ++ rs ++ ">", [commify [head rs], "<" ++ tail rs ++ ">"])]
        ++ generateTerminalRule [head rs]
        ++ parseComposedNonTerm (tail rs)
        
    -- if non-terminal len is 2, (A -> <BC>) we need to generate (1) <BC> -> BC and
    -- (2) terminal rules (b' -> b)
    | length rs == 2 = [("<" ++ rs ++ ">", [commify [head rs] ++ commify (tail rs)])]
        ++ generateTerminalRule [head rs]
        ++ generateTerminalRule [rs !! 1]
        
    -- no other rule is possible here
    | otherwise = error ("bad rule: " ++ show rs)

-- generates rule for terminal in form: a' -> a
-- if the input is non-terminal, no rule is needed
generateTerminalRule :: Symbol -> [Rule]
generateTerminalRule s = [(s ++ "'", [s]) | all isLower s]

-- adds comma to terminals only, ignores non-terminals
commify :: Symbol -> Symbol
commify s = if all isLower s then s ++ "'" else s

-- load the grammar from a string
readGrammarFromStr :: [String] -> Grammar
readGrammarFromStr (l1:l2:l3:ls) =
    newGrammar (splitOn "," l1) (splitOn "," l2) l3
        (map ((\ x -> (head x, map (: []) $ concat (tail x))) . splitOn "->") ls)
readGrammarFromStr _ = error "bad input format"

-- command -i: load and print the grammar
readAndPrintStr :: String -> String
readAndPrintStr s = show $ readGrammarFromStr $ lines s

-- command -1: load the grammar, remove simple rules and print
removeSimpleRulesStr :: String -> String
removeSimpleRulesStr s = show $ removeSimpleRules $ readGrammarFromStr $ lines s

-- command -2: load the grammar and transform to CNF
transformToCNFStr :: String -> String
transformToCNFStr s = show $ transformToCNF $ removeSimpleRules $ readGrammarFromStr $ lines s

-- map of known commands and assigned functions
dispatch :: [(String, String -> String)]
dispatch =  [("-i", readAndPrintStr), ("-1", removeSimpleRulesStr), ("-2", transformToCNFStr)]

main = do
    args <- getArgs

    -- expecting a command and possibly input file
    if length args < 1 || length args > 2
    then putStrLn "bad arguments"
    else do
        -- read the command
        let command = head args

        -- read the input from file or stdin
        contents <- if length args == 2
                    then readFile $ last args
                    else getContents

        -- execute the command
        case lookup command dispatch of
            Nothing -> putStrLn "bad arguments"
            Just a -> putStrLn $ a contents

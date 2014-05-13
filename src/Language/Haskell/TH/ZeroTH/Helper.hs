{-# LANGUAGE PatternGuards #-}
module Language.Haskell.TH.ZeroTH.Helper ( helper, idPrefix ) where

import Data.Generics
import Data.Maybe                 ( fromJust, isJust )
import System.IO                  ( hFlush, stdout )

import Language.Haskell.TH.ZeroTH.Comments ( Location )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Set as S
import Data.List ( isInfixOf, isPrefixOf, intersperse )

idPrefix :: String
idPrefix = "ZEROTH OUTPUT: "

helper :: Q [Dec] -> Location -> Q [Dec]
helper splice loc = do
    decls <- splice
    runIO $ do putStrLn $ idPrefix ++ show ((loc, unlines $ fmap (braceLet . pprint) $ map unNameU decls),
                                map (fromJust . nameModule) $ listify (isJust . nameModule) decls)
               hFlush stdout
    return decls

-- | if a Dec binds a name that is a NameU, ghc generates a NameS
unNameU :: Dec -> Dec
unNameU x = everywhere (mkT $ \n -> if S.member n u
                    then renameU n
                    else n)
                x
    where u = S.fromList (topLevelBindrs x)

topLevelBindrs :: Dec -> [Name]
topLevelBindrs x = case x of
    FunD n _            -> [n]
    ValD p _ _          -> allVarPNameU p 
    DataD _ n _ _ _     -> [n]
    NewtypeD _ n _ _ _  -> [n]
    TySynD n _ _        -> [n]
    ClassD _ n _ _ _    -> [n] -- needed?
    SigD n _            -> [n]
    InfixD _ n          -> [n]
    FamilyD _ n _ _     -> [n]
    DataInstD _ n _ _ _ -> [n]
    NewtypeInstD _ n _ _ _ -> [n]
    TySynInstD n _    -> [n]
    PragmaD (InlineP n _ _ _)       -> [n]
    PragmaD (SpecialiseP n _ _ _)   -> [n]
    InstanceD {}    -> []
    ForeignD {}     -> []
    PragmaD {}      -> []
    ClosedTypeFamilyD n _ _ _ -> [n]
    RoleAnnotD n _ -> [n]

renameU :: Name -> Name
renameU (Name occ (NameU _)) = Name occ NameS
renameU x = x

allVarPNameU :: Data a => a -> [Name]
allVarPNameU = everything (++) (mkQ [] $ \e -> case e of
    VarP n | Name _ (NameU _) <- n -> [n]
    _ -> [])

braceLet :: String -> String
braceLet string = if "let " `isInfixOf` string then replace "let " "let {" . replace ";" "};" $ string else string

-- From MissingH
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (startsWith delim) str
        in 
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim 
                                                 (drop (length delim) x)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = joinWith new . split old $ l

joinWith :: [a] -> [[a]] -> [a]
joinWith delim l = concat (intersperse delim l)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith = isPrefixOf

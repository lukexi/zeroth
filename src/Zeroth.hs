module Zeroth
    ( prettyPrintAll, zeroth, zerothInternal
    ) where

import Language.Haskell.Exts hiding ( comments )
import System.Process        ( runInteractiveProcess, waitForProcess )
import System.IO             ( hPutStr, hClose, hGetContents, openTempFile, stdin )
import System.Directory      ( removeFile, getTemporaryDirectory )
import System.Exit           ( ExitCode (..) )
import Control.Monad         ( when )
import Data.List             ( (\\), intersperse, isPrefixOf, nub, stripPrefix )
import Data.Maybe            ( catMaybes, mapMaybe )

import Comments              ( Location, parseComments, mixComments )
import ListUtils             ( replaceAll )

readFromFile :: FilePath -> IO String
readFromFile "-"  = hGetContents stdin
readFromFile path = readFile path

zeroth :: FilePath -- ^ Path to GHC
       -> FilePath -- ^ Path to cpphs
       -> [String] -- ^ GHC options
       -> [String] -- ^ cpphs options
       -> String   -- ^ Input filename, or "-" for stdin
       -> [String] -- ^ Import prefixes to drop
       -> IO String
zeroth ghcPath cpphsPath ghcOpts cpphsOpts inputFile dropImports
    = fmap prettyPrintAll $ zerothInternal ghcPath cpphsPath ghcOpts cpphsOpts inputFile dropImports

data ZeroTHOutput
    = ZeroTHOutput { originalSource :: String
                   , minusTH :: Module
                   , thOutput :: [(Location, String)]
                   }

zerothInternal :: FilePath -- ^ Path to GHC
               -> FilePath -- ^ Path to cpphs
               -> [String] -- ^ GHC options
               -> [String] -- ^ cpphs options
               -> String   -- ^ Input filename, or "-" for stdin
               -> [String] -- ^ Import prefixes to drop
               -> IO ZeroTHOutput
zerothInternal ghcPath cpphsPath ghcOpts cpphsOpts inputFile dropImports
    = do input       <- readFromFile inputFile
         tmpDir      <- getTemporaryDirectory
         (inputFile2, tmpHandle) <- case inputFile of
                                       "-" -> openTempFile tmpDir "TH.cpphs.zeroth"
                                       _   -> return (inputFile, undefined)
         when (inputFile == "-") $ hPutStr tmpHandle input >> hClose tmpHandle
         thInput     <- preprocessCpphs cpphsPath (["--noline","-DHASTH"]++cpphsOpts) inputFile2
         zerothInput <- preprocessCpphs cpphsPath (["--noline"]++cpphsOpts) inputFile2
         thData <- case parseModule thInput of
                     ParseOk m@(Module _ _ _ _ _ _ decls) -> runTH ghcPath m ghcOpts (mapMaybe getTH decls)
                     e -> error (show e)
         zerothData <- case parseModule zerothInput of
                         ParseOk (Module loc m pragmas mWarn exports im decls)
                           -> return (Module loc m pragmas mWarn exports (postProcessImports dropImports im $ snd thData) (filter delTH decls))
                         e -> error (show e)
         when (inputFile == "-") $ removeFile inputFile2
         return $ ZeroTHOutput { originalSource = input
                               , minusTH = zerothData
                               , thOutput = fst thData
                               }
    where getTH (SpliceDecl l s) = Just (l,s)
          getTH _ = Nothing
          delTH (SpliceDecl _ _) = False
          delTH _ = True

prettyPrintAll :: ZeroTHOutput -> String
prettyPrintAll out = unlines . mixComments (parseComments $ originalSource out) $ numberAndPrettyPrint (minusTH out) ++ ((-1, 1), "") : thOutput out

location :: SrcLoc -> Location
location sLoc = (srcLine sLoc, srcColumn sLoc)

numberAndPrettyPrint :: Module -> [(Location, String)]
numberAndPrettyPrint (Module mLoc m prags mbWarn exports imp decls)
    = (nAndPPrag =<< prags)
      ++ (location mLoc, concat $ "module "
                                 : prettyPrint m
                                 : catMaybes [ fmap ppWarnText mbWarn
                                                , fmap (\es -> " (" ++ concat (intersperse ", " $ map prettyPrint es) ++ ")") exports
                                                ]
                                   ++ [" where"])
         : (map (\i -> (location (importLoc i), prettyPrint i)) imp ++ (nAndPDec =<< decls))
    where nAndPDec d@(TypeDecl loc _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(DataDecl loc _ _ _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(GDataDecl loc _ _ _ _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(InfixDecl loc _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(ClassDecl loc _ _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(InstDecl loc _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(DefaultDecl loc _) = [(location loc, prettyPrint d)]
          nAndPDec d@(SpliceDecl loc _) = [(location loc, prettyPrint d)]
          nAndPDec d@(TypeSig loc _ _) = [(location loc, prettyPrint d)]
          nAndPDec (FunBind matches) = map (\match@(Match loc _ _ _ _ _) -> (location loc, prettyPrint match)) matches
          nAndPDec d@(PatBind loc _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(ForImp loc _ _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(ForExp loc _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(DataFamDecl loc _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(DataInsDecl loc _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(DeprPragmaDecl loc _) = [(location loc, prettyPrint d)]
          nAndPDec d@(DerivDecl loc _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(GDataInsDecl loc _ _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(InlineSig loc _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(InstSig loc _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(RulePragmaDecl loc _) = [(location loc, prettyPrint d)]
          nAndPDec d@(SpecInlineSig loc _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(SpecSig loc _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(TypeFamDecl loc _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(TypeInsDecl loc _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(UnknownDeclPragma loc _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(WarnPragmaDecl loc _) = [(location loc, prettyPrint d)]
          nAndPPrag (LanguagePragma loc names)
              | null filteredNames = []
              | otherwise          = [(location loc, prettyPrint $ LanguagePragma loc filteredNames)]
              where
                  filteredNames = names \\ map Ident unwantedLanguageOptions
          nAndPPrag p@(IncludePragma loc _) = [(location loc, prettyPrint p)]
          nAndPPrag p@(CFilesPragma loc _) = [(location loc, prettyPrint p)]
          nAndPPrag (OptionsPragma loc mt s) = [(location loc, prettyPrint . OptionsPragma loc mt $ filterOptions s)]
          nAndPPrag p@(UnknownTopPragma loc _ _) = [(location loc, prettyPrint p)]
          filterOptions optStr = foldr (\opt -> replaceAll (" -" ++ opt ++ " ") " ") optStr $ "cpp" : "fth" : map ('X' :) unwantedLanguageOptions
          unwantedLanguageOptions = ["CPP", "TemplateHaskell"]

ppWarnText :: WarningText -> String
ppWarnText (DeprText s) = "{-# DEPRECATED" ++ s ++ "#-}"
ppWarnText (WarnText s) = "{-# WARNING" ++ s ++ "#-}"

-- Removes TH imports, and adds any qualified imports needed by generated TH code
postProcessImports :: [String] -> [ImportDecl] -> [String] -> [ImportDecl]
postProcessImports dropPrefixes oldImports qNames
   = nub $  removeTH
        ++ ( map (\q -> ImportDecl { importLoc = emptySrcLoc
                                   , importModule = ModuleName q
                                   , importQualified = True
                                   , importSrc = False
                                   , importAs = Nothing
                                   , importSpecs = Nothing })
            $ filter (\q -> not $ any (maybe False (\(ModuleName m) -> m == q) . importAs) removeTH) qNames )
  where
    removeTH = filter (not . (\(ModuleName m) -> any (`isPrefixOf` m) dropPrefixes) . importModule) oldImports

preprocessCpphs :: FilePath -- ^ Path to cpphs
                -> [String]
                -> String
                -> IO String
preprocessCpphs cpphs args inputFile
    = do (inH,outH,_,pid) <- runInteractiveProcess cpphs (inputFile:args) Nothing Nothing
         hClose inH
         output <- hGetContents outH
         length output `seq` hClose outH
         eCode <- waitForProcess pid
         case eCode of
           ExitFailure err -> error $ "Failed to run cpphs: " ++ show err
           ExitSuccess -> return output

runTH :: FilePath -- ^ Path to GHC
      -> Module 
      -> [String]
      -> [(SrcLoc,Splice)]
      -> IO ([(Location,String)], [String])
runTH ghcPath (Module _ _ pragmas _ _ imports _) ghcOpts th
    = do tmpDir <- getTemporaryDirectory
         (tmpInPath,tmpInHandle) <- openTempFile tmpDir "TH.source.zeroth.hs"
         hPutStr tmpInHandle realM
         hClose tmpInHandle
         let args = [tmpInPath,"-fno-code"]++ghcOpts
         --putStrLn $ "Module:\n" ++ realM
         --putStrLn $ "Running: " ++ unwords (ghcPath:args)
         (inH,outH,errH,pid) <- runInteractiveProcess ghcPath args Nothing Nothing
         hClose inH
         output <- hGetContents outH
         --putStrLn $ "TH Data:\n" ++ output
         length output `seq` hClose outH
         errMsg <- hGetContents errH
         length errMsg `seq` hClose errH
         eCode <- waitForProcess pid
         -- removeFile tmpInPath
         case eCode of
           ExitFailure err -> error (unwords (ghcPath:args) ++ ": failure: " ++ show err ++ ":\n" ++ errMsg)
           ExitSuccess | not (null errMsg) -> error (unwords (ghcPath:args) ++ ": failure:\n" ++ errMsg)
                       | otherwise -> case mapMaybe (stripPrefix idPrefix) $ lines output of
                                        [h] -> case reads h of
                                            [(ret,_)] -> return ret
                                            _         -> error $ "Failed to parse result:\n"++output
                                        _   -> error $ "Failed to parse result:\n"++output
    where thImport = ImportDecl emptySrcLoc (ModuleName "Language.Haskell.TH") False False Nothing Nothing
          pp :: (Pretty a) => a -> String
          pp = prettyPrintWithMode (defaultMode{layout = PPInLine})
          realM = unlines $ map (pp . disableWarnings) pragmas
                            ++ ["module Main ( main ) where"]
                            ++ map pp (thImport:imports)
                            ++ ["import qualified Data.Generics.Schemes"]
                            ++ ["import qualified Data.Maybe"]
                            ++ ["import qualified System.IO"]
                            ++ ["import qualified Prelude"]
                            ++ ["main = Prelude.undefined"]
                            ++ ["$( do decls <- Prelude.sequence " ++ pp (List splices)]
                            ++ ["      runIO $ do Prelude.putStrLn $ " ++ show idPrefix
                                ++ " ++ Prelude.show ( Prelude.map (\\(l,d) -> (l,pprint d)) (Prelude.zip "
                                ++ pp (List locations) ++ " decls)"]
                            ++ ["                         , Prelude.map (Data.Maybe.fromJust . nameModule) $ Data.Generics.Schemes.listify (Data.Maybe.isJust . nameModule) decls)"]
                            ++ ["                 System.IO.hFlush System.IO.stdout"]
                            ++ ["      Prelude.return []"]
                            ++ [" )"]
          splices = flip map th $ \(_src,splice) -> spliceToExp splice
          locations = flip map th $ \(loc,_splice) -> Tuple [ Lit (Int (fromIntegral (srcLine loc)))
                                                            , Lit (Int (fromIntegral (srcColumn loc)))
                                                            ]
          spliceToExp (ParenSplice e) = e
          spliceToExp _ = error "TH: FIXME!"
          idPrefix = "ZEROTH OUTPUT: "
          disableWarnings (OptionsPragma loc Nothing    s) = OptionsPragma loc Nothing $ s ++ " -w" -- Turn off all warnings (works for GHC)
          disableWarnings (OptionsPragma loc (Just GHC) s) = OptionsPragma loc (Just GHC) $ s ++ " -w"
          disableWarnings x = x

emptySrcLoc :: SrcLoc
emptySrcLoc = SrcLoc "" 0 0

{-

module Test where
$(test) -- line 2
$(jalla) -- line 3
svend = svend


-------------------------------

module Main where
import Language.Haskell.TH
main = do decs <- sequence [runQ test
                           ,runQ jalla]
          mapM_ (putStrLn.pprint) (zip decs [2,3])


-}

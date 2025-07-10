module Hmusic where 

import Test.QuickCheck
import Test.QuickCheck.Function

data Piece m = Piece [Phrase m]  deriving (Show, Eq)

-- A functional category classifies chords as being part of a tonic (TonM), dominant    
-- (DomM), or subdominant (SubM) structure, where a subdominant structure must     
-- always precede a dominant structure.     

data Phrase m = Pt (Ton m) | Pd (Sub m) (Dom m) deriving (Show, Eq)    
    
data Ton m = Ton m [ Chord ] deriving (Show, Eq)    
data Dom m = Dom m [ Chord ] deriving (Show, Eq)
data Sub m = Sub m [ Chord ] deriving (Show, Eq)    

-- mode of the key
data Mode = Major | Minor deriving (Show, Eq)

-- chord class
data Class = Maj | Min | Dom7 | Dim | Aug deriving (Show, Eq)

data Degree = I | II | III | IV | V | VI | VII deriving (Show, Eq, Ord , Enum)

data Chord = Chord Degree Class deriving( Show, Eq)

genTonic :: Mode -> Gen (Ton Mode)
genTonic Major = frequency [ (3, return $ Ton Major [ Chord I Maj] ),
                             (1, return $ Ton Major [ Chord I Maj, Chord IV Maj, Chord I Maj])]
genTonic Minor = frequency [ (3, return $ Ton Minor [ Chord I Min] ),
                             (1, return $ Ton Minor [ Chord I Min, Chord IV Min, Chord I Min])]


genSub Major =  frequency [ (1, return $ Sub Major [ Chord IV Maj] ),
                            (1, return $ Sub Major [ Chord II Min] )]
genSub Minor =  frequency [ (1, return $ Sub Minor [ Chord IV Min] ),
                            (1, return $ Sub Minor [ Chord II Min] )] -- minor?

genDom m = frequency [ (1, return $ Dom m [ Chord V Dom7] ),
                       (3, return $ Dom m [ Chord V Maj ] )] -- minor?


genPd :: Gen (Mode) ->  Gen (Phrase Mode)
genPd genM = do
    m <- genM
    s <- genSub m
    d <- genDom m
    return $ Pd s d

--genPhrase genM = do 
--    m <- genM
--    case m of
--        Major -> do
--            frequency []
--                       --(1, return $ Pt (Ton m ) ), 
--                       --(2, return $ Pd (Sub m) (Dom m) ),
--                       --(1, return subDomintant  )]
--        Minor -> do
--            frequency []
--






--instance Arbitrary m => Arbitrary (Piece m) where
--    arbitrary = Piece <$> listOf arbitrary
--
instance Arbitrary Mode where
    arbitrary = elements [Major, Minor]
--
--instance Arbitrary m => Arbitrary (Phrase m) where
--    arbitrary = genPhrase arbitrary
--
--
--samplePiece :: Gen (Piece Mode)
--samplePiece = arbitrary
--
--samplePhrase :: Gen (Phrase Mode)
--samplePhrase = arbitrary
--
sampleMode :: Gen Mode
sampleMode =  arbitrary

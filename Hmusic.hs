module Hmusic where 

import Test.QuickCheck
import Test.QuickCheck.Function

data Piece m = Piece [Phrase m]  deriving (Show, Eq)

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

genTonic :: Mode -> Gen (Phrase Mode)
genTonic Major = frequency [ (4, return $ Pt $ Ton Major [ Chord I Maj] ),
                             (1, return $ Pt $ Ton Major [ Chord I Maj, Chord IV Maj, Chord I Maj])]

genTonic Minor = frequency [ (4, return $ Pt $ Ton Minor [ Chord I Min] ),
                             (1, return $ Pt $ Ton Minor [ Chord I Min, Chord IV Min, Chord I Min])]

genSub Major =  frequency [ (1, return $ Sub Major [ Chord IV Maj] ),
                            (1, return $ Sub Major [ Chord II Min] )]
genSub Minor =  frequency [ (1, return $ Sub Minor [ Chord IV Min] ),
                            (1, return $ Sub Minor [ Chord II Min] )] -- minor?

genDom m = frequency [ (1, return $ Dom m [ Chord V Dom7] ),
                       (3, return $ Dom m [ Chord V Maj ] )] -- minor?

genPd :: Mode ->  Gen (Phrase Mode)
genPd m = do
    s <- genSub m
    d <- genDom m
    return $ Pd s d

genPhrase genM = do
    m <- genM 
    frequency [ ( 2, genTonic m), 
                ( 1, genPd    m)]


genPiece = Piece <$> listOf arbitrary

instance Arbitrary (Piece Mode) where
    arbitrary = genPiece

instance Arbitrary Mode where
    arbitrary = elements [Major, Minor]

instance Arbitrary (Phrase Mode) where
    arbitrary = genPhrase arbitrary

samplePiece :: Gen (Piece Mode)
samplePiece = arbitrary

sampleMode :: Gen Mode
sampleMode =  arbitrary

samplePhrase :: Gen (Phrase Mode)
samplePhrase = arbitrary

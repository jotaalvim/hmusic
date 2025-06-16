module Hmusic where 


import Test.QuickCheck
import Test.QuickCheck.Function

data Piece m = Piece [Phrase m]  deriving (Show, Eq)

-- A functional category classifies chords as being part of a tonic (TonM), dominant    
-- (DomM), or subdominant (SubM) structure, where a subdominant structure must     
-- always precede a dominant structure.     

data Phrase m = Pt (Ton m) | Pd (Sub m) (Dom m) deriving (Show, Eq)    
    
-- não sei se devo passar o modo como parametro
data Ton m = Ton m deriving (Show, Eq)    
data Dom m = Dom m deriving (Show, Eq)
data Sub m = Sub m deriving (Show, Eq)    

-- mode of the key
data Mode = Major | Minor deriving (Show, Eq)


-- chord class
data Class = Maj | Min | Dom7 | Dim | Aug deriving (Show, Eq)

instance Arbitrary Mode where
    arbitrary = elements [Major, Minor]

instance Arbitrary m => Arbitrary (Phrase m) where
    arbitrary = genPhrase arbitrary


genPhrase genM = do 
    mode <- genM
    frequency [(1, return $  Pt (Ton mode)  ), 
               (2, return $  Pd (Sub mode) (Dom mode) )]



samplePhrase :: Gen (Phrase Mode)
samplePhrase = arbitrary

sampleMode :: Gen Mode
sampleMode =  arbitrary
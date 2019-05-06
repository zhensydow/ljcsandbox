data Career = Actor | Programmer | TaxiDriver | Writer
            deriving Show

data Person = Person { name :: String
                     , career :: Maybe Career
                     , contact :: String }
            deriving Show

me = Person "Luis Jose Cabellos Gomez" 
     (Just Programmer)
     "zhen.sydow at gmail.com"
     
main = print me

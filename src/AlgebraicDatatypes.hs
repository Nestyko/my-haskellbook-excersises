data PugType = PugData

data HuskyType = HyskyData

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

data Price = Price Integer deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar x = case x of
  Car _ _ -> True
  _ -> False

isPlane :: Vehicle -> Bool
isPlane x = case x of
  Plane _ _ -> True
  _ -> False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu x = case x of
  Car manu _ -> manu

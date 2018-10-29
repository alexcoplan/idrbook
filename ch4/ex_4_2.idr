import Data.Vect

data PowerSource = Petrol | Pedal | Juice

data Vehicle : PowerSource -> Type where
  Unicycle : Vehicle Pedal
  Bicycle : Vehicle Pedal
  Tram : Vehicle Juice
  Motorcycle : ( fuel : Nat ) -> Vehicle Petrol
  Car : ( fuel : Nat ) -> Vehicle Petrol
  Bus : ( fuel : Nat ) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel Bicycle impossible
refuel Unicycle impossible

vtake : (k : Nat) -> Vect (k+n) el -> Vect k el
vtake Z xs = []
vtake (S k) (x :: xs) = x :: vtake k xs

sumAt : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumAt {n} pos xs ys =
  case integerToFin pos n of
       Nothing => Nothing
       (Just idx) => Just ((index idx xs) + (index idx ys))


vzip : Vect n a -> Vect n b -> Vect n (a,b)
vzip [] [] = []
vzip (x :: xs) (y :: ys) = (x, y) :: vzip xs ys

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys =
  case integerToFin pos n of
       Nothing => Nothing
       (Just iFin) => Just ((Vect.index iFin xs) + (Vect.index iFin ys))



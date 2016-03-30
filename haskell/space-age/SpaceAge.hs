module SpaceAge
  (Planet(Earth , Mercury , Venus , Mars , Jupiter , Saturn , Uranus , Neptune), ageOn)
       where


data Planet =  Earth
  | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune
    

ageOn :: Planet -> Integer -> Double
ageOn Earth z =  (fromInteger z ) / 31557600.0
ageOn Mercury z = ( / 0.2408467)  $ ageOn Earth  z
ageOn Venus   z = ( / 0.61519726) $ ageOn Earth  z
ageOn Mars    z = ( /  1.8808158) $ ageOn Earth  z
ageOn Jupiter z = ( / 11.862615)  $ ageOn Earth  z
ageOn Saturn  z = ( / 29.447498)  $ ageOn Earth  z
ageOn Uranus  z = ( / 84.016846)  $ ageOn Earth  z
ageOn Neptune z = ( / 164.79132)  $ ageOn Earth  z

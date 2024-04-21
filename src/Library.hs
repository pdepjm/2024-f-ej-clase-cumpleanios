module Library where

import           PdePreludat

data Invitade = UnInvitade { cansancio :: Number
                           , felicidad :: Number
                           , cancionFavorita :: String
                           }
  deriving (Show, Eq)

ceci :: Invitade
ceci = UnInvitade { cansancio = 79
                  , felicidad = 85
                  , cancionFavorita = "Tango Lloron"
                  }

valentin :: Invitade
valentin =
  UnInvitade { cansancio = 99, felicidad = 150, cancionFavorita = "No se" }

estaCansade :: Invitade -> Bool
estaCansade invitade = cansancio invitade > 80

cansarse :: Invitade -> Invitade
cansarse invitade = invitade { cansancio = cansancio invitade + 10 }

cansarse' :: Invitade -> Invitade
cansarse' invitade = UnInvitade { cansancio = cansancio invitade + 10
                                , felicidad = felicidad invitade
                                , cancionFavorita = cancionFavorita invitade
                                }

cansarse'' :: Invitade -> Invitade
cansarse'' (UnInvitade cansancio felicidad cancionFavorita) =
  UnInvitade (cansancio + 10) felicidad cancionFavorita

disfrutar :: Invitade -> Invitade
disfrutar invitade =
  invitade { felicidad = felicidad invitade + 100 - cansancio invitade }

-- plancitos

type Plancito = Invitade -> Invitade

-- definir funciones: f a = g a
charlitaDeFulbo :: Plancito
charlitaDeFulbo invitade = disfrutar invitade

-- definir funciones: f = g
charlitaDeFulbo' :: Plancito
charlitaDeFulbo' = disfrutar

bailar :: Plancito
bailar invitade = cansarse (disfrutar invitade)

-- definir funciones: f = h.g
bailar' :: Plancito
bailar' = cansarse . disfrutar

mesaDulce :: Plancito
mesaDulce = disfrutar . cansarse

tieneBuenGusto :: Invitade -> Bool
tieneBuenGusto = even . length . cancionFavorita

tieneBuenGusto' :: Invitade -> Bool
tieneBuenGusto' invitade = even (length (cancionFavorita invitade))

-- > leVaADarFiaca ceci mesaDulce  
-- False
-- leVaADarFiaca :: Invitade -> (Invitade -> Invitade) -> Bool
leVaADarFiaca :: Invitade -> Plancito -> Bool
leVaADarFiaca invitade plancito = estaCansade (plancito invitade)

leVaADarFiaca' :: Invitade -> Plancito -> Bool
leVaADarFiaca' invitade plancito = (estaCansade . plancito) invitade

type Bandurria = [Invitade]

-- playlist :: [Invitade] -> [String]
playlist :: Bandurria -> [String]
playlist bandurria = map cancionFavorita bandurria
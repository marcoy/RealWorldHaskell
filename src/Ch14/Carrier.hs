module Ch14.Carrier where

import qualified Data.Map as M

type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = Honest_Bobs_Phone_Network
                   | Morrisas_Marvelous_Mobiles
                   | Petes_Plutocratic_Phones
                   deriving (Eq, Ord, Show)

findCarrierBillingAddress person phoneMap carrierMap addressMap =
        lookup phoneMap person >>= lookup carrierMap >>= lookup addressMap
    where
        lookup = flip M.lookup

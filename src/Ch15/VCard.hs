module Ch15.VCard where

import Control.Monad ( MonadPlus, mplus, mzero )

data Context = Home | Mobile | Business
               deriving (Eq, Show)

type Phone = String

albulena = [(Home, "+355-652-55512")]

nils = [(Mobile, "+47-922-55-512"), (Business, "+47-922-12-121"),
        (Home, "+47-925-55-121"), (Business, "+47-922-25-551")]

twalumba = [(Business, "+260-02-55-5121")]


onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = lookup Business ps `mplus` lookup Mobile ps

allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones ps = map snd $ filter (contextIs Home) ps `mplus`
                                 filter (contextIs Mobile) ps
                       where
                           contextIs a (b, _) = a == b

lookupM :: (MonadPlus m, Eq a) => a -> [(a, b)] -> m b
lookupM _ []    = mzero
lookupM k ((x,y):xys)
    | x == k    = return y `mplus` lookupM k xys
    | otherwise = lookupM k xys

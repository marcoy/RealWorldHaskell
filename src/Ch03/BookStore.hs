module Ch03.BookStore where

type CustomerID = Int
type ReviewBody = String
type CardHolder = String
type CardNumber = String
type Address = [String]
type BookRecord = (BookInfo, BookReview)

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

data BookReview = BookReview BookInfo CustomerID String

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

data Customer = Customer {
                customerID :: CustomerID
              , customerName :: String
              , customerAddress :: Address
              } deriving (Show)


myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

customer2 = Customer {
            customerID = 271828
          , customerAddress = ["1048576 Disk Drive",
                               "Milpitas, CA 95134",
                               "USA"]
          , customerName = "Jane Q. Citizen"
          }

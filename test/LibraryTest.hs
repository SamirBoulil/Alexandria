import Test.Hspec

refactoring = Book "ref-1234" "Refactoring" "Martin Fowler"
xpExplained = Book "ref-456" "XP Explained" "Kent Beck"

main :: IO ()
main = hspec $ do

  describe "Manage the Akeneo Library"$ do
    it "Does not have any books" $
      books (Library []) `shouldBe` []

    it "references books" $
      books (Library [(refactoring, 1), (xpExplained, 1)]) `shouldBe` [(refactoring, 1), (xpExplained, 1)]

    it "references a new book to an existing library" $
      referenceBook library refactoring `shouldBe` (Library [(refactoring, 1), (xpExplained, 1)])
      where library = Library [(xpExplained, 1)]

    {-TODO-}
    {-it "can have multiple copies of the same book" $-}
      {-referenceBook library refactoring `shouldBe` (Library [refactoring, xpExplained])-}
      {-where library = Library [xpExplained]-}


type ISBN = String
type Title = String
type Author = String
data Book = Book { isbn :: ISBN, title :: Title, author:: Author } deriving (Show, Eq)
type Copies = Integer
data Library = Library { books :: [(Book, Copies)] } deriving (Show, Eq)

referenceBook :: Library -> Book -> Library
referenceBook library newBook = library { books = (newBook, 1):(books library) }

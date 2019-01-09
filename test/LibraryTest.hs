import Test.Hspec
import Data.List

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
      let library = Library [(xpExplained, 1)]
      in referenceBook refactoring library `shouldBe` (Library [(refactoring, 1), (xpExplained, 1)])

    it "can have multiple copies of the same book" $
      let library = Library [(xpExplained, 1)]
      in referenceBook xpExplained library `shouldBe` (Library [(xpExplained, 2)])

{-TODO
 -
 - Add UUID identifier for a book
 - Library [Book]
 -
 - OR
 - Book <- *
 - Copy / Exemplaire
 - type: (pdf ou livre)
 - Number of copies
 -
 - -}



type ISBN = String
type Title = String
type Author = String
data Book = Book { isbn :: ISBN, title :: Title, author:: Author } deriving (Show, Eq)
type Copies = Integer
data Library = Library { books :: [(Book, Copies)] } deriving (Show, Eq)

referenceBook :: Book -> Library -> Library
referenceBook newBook library = case maybeABook of Nothing -> addNewBook newBook library
                                                   _       -> updateCopies library newBook
                                where maybeABook = findBook newBook library

findBook :: Book -> Library -> Maybe Book
findBook bookToFind library =
  case maybeABook of Nothing -> Nothing
                     Just aBook -> Just $ fst aBook
  where maybeABook = find (\(aBook, _) -> aBook == bookToFind) (books library)

updateCopies:: Library -> Book -> Library
updateCopies library newBook = Library $ map (\(aBook, copies) -> if aBook == newBook then (aBook, copies+1) else (aBook, copies)) $ books library

addNewBook :: Book -> Library -> Library
addNewBook bookToAdd library = library { books = (bookToAdd, 1):(books library) }

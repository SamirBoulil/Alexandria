{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LibrarySpec where

import Test.Hspec
import Data.List
import Numeric.Natural

refactoring = Book "ref-1234" "Refactoring" "Martin Fowler" [Ebook]
xpExplained = Book "ref-456" "XP Explained" "Kent Beck" [Paper 4]

main :: IO ()
main = putStrLn "Test suite not yet implemented"

spec :: Spec
spec = describe "Manage the Akeneo Library"$ do
    it "Does not have any books" $
      books (Library []) `shouldBe` []

    it "references books" $
      books (Library [refactoring, xpExplained]) `shouldBe` [refactoring, xpExplained]

    -- it "references a new book to an existing library" $
    --   let library = Library [xpExplained]
    --   in referenceBook refactoring library `shouldBe` (Library [refactoring, xpExplained])

    -- it "can have multiple copies of the same book" $
    --   let library = Library [(xpExplained, 1)]
    --   in referenceBook xpExplained library `shouldBe` (Library [(xpExplained, 2)])

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
data Copy = Ebook
          | Paper { nbCopy :: Natural }
    deriving (Show, Eq)
data Book = Book { isbn :: ISBN, title :: Title, author :: Author, copies :: [Copy] } deriving (Show, Eq)
data Library = Library { books :: [Book] } deriving (Show, Eq)

-- referenceBook :: Book -> Library -> Library
-- referenceBook newBook library = case maybeABook of Nothing -> addNewBook newBook library
--                                                    _       -> updateCopies newBook library
--                                 where maybeABook = findBook newBook library

-- findBook :: Book -> Library -> Maybe Book
-- findBook bookToFind library = find (== bookToFind) (books library)

-- addNewBook :: Book -> Library -> Library
-- addNewBook bookToAdd library = library { books = bookToAdd:(books library) }

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LibrarySpec where

import Test.Hspec
import Data.List
import Numeric.Natural

spec :: Spec
spec = describe "Manage the Akeneo Library" $ do
    let refactoring = Book "ref-1234" "Refactoring" "Martin Fowler" []
    let xpExplained = Book "ref-456" "XP Explained" "Kent Beck" []

    it "Does not have any books" $
      books (Library []) `shouldBe` []

    it "references books" $
      books (Library [refactoring, xpExplained]) `shouldBe` [refactoring, xpExplained]

    it "references a new book to an existing library" $ do
      let library = Library [xpExplained]
      referenceBook refactoring library `shouldBe` (Library [refactoring, xpExplained])


    it "does not references an existing book twice" $ do
      let library = Library [xpExplained]
      referenceBook xpExplained library `shouldBe` library

    it "can add a new available ebook copy for a book" $ do
      let library = Library [xpExplained]
      let ebookXpEplained = (Book "ref-456" "XP Explained" "Kent Beck" [Ebook])
      addAvailableCopy xpExplained Ebook library `shouldBe` (Library [ebookXpEplained])


-- referenceBook :: Book -> Library -> Library
-- referenceBook newBook library = case maybeABook of Nothing -> addNewBook newBook library
--                                                    _       -> updateCopies newBook library
--                                 where maybeABook = findBook newBook library



-- addNewBook :: Book -> Library -> Library
-- addNewBook bookToAdd library = library { books = bookToAdd:(books library) }

type ISBN = String
type Title = String
type Author = String
data Copy = Ebook
          | Paper { nbCopy :: Natural }
    deriving (Show, Eq)
data Book = Book { isbn :: ISBN, title :: Title, author :: Author, copies :: [Copy] } deriving (Show, Eq)
data Library = Library { books :: [Book] } deriving (Show, Eq)

referenceBook :: Book -> Library -> Library
referenceBook newBook library = case maybeABook of Nothing -> library { books = newBook:(books library) }
                                                   _       -> library
                                where maybeABook = findBook newBook library

addAvailableCopy :: Book -> Copy -> Library -> Library
addAvailableCopy book copy library = Library $ map (\b -> if b == book then book { copies = copy:(copies book)} else b) (books library)
            
findBook :: Book -> Library -> Maybe Book
findBook bookToFind library = find (== bookToFind) (books library) 

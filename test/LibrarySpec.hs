{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LibrarySpec where

import Test.Hspec
import Data.List
import Numeric.Natural

spec :: Spec
spec = describe "Manage the Akeneo Library" $ do
    let refactoring = Book "ref-1234" "Refactoring" "Martin Fowler" NotAvailable 0
    let xpExplained = Book "ref-456" "XP Explained" "Kent Beck" NotAvailable 0

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

    it "can add make an ebook version available" $ do
      let library = Library [xpExplained]
      let expectedBook = (Book "ref-456" "XP Explained" "Kent Beck" Available 0)
      setEbookAvailability "ref-456" Available library `shouldBe` (Library [expectedBook])

    -- it "cannot add a new available ebook copy twice" $ do
    --   let ebookXpExplained = (Book "ref-456" "XP Explained" "Kent Beck" [Ebook])
    --   let library = Library [ebookXpExplained]
    --   addAvailableCopy xpExplained Ebook library `shouldBe` (Library [ebookXpExplained])

    -- it "can add a new available paper copy" $ do
    --   let ebookXpExplained = (Book "ref-456" "XP Explained" "Kent Beck" [Ebook])
    --   let library = Library [ebookXpExplained]
    --   let allCopies = (Book "ref-456" "XP Explained" "Kent Beck" [Paper { nbCopy = 1}, Ebook])
    --   addAvailableCopy xpExplained Paper { nbCopy = 1 } library `shouldBe` (Library [allCopies])

type ISBN = String
type Title = String
type Author = String
type NumberOfPaperCopies = Int

data IsAvailableAsEbook = Available | NotAvailable deriving (Show, Eq)

data Book = Book { isbn :: ISBN, title :: Title, author :: Author, isAvailableAsEbook :: IsAvailableAsEbook, nbPaperCopy :: NumberOfPaperCopies } deriving (Show, Eq)

data Library = Library { books :: [Book] } deriving (Show, Eq)

referenceBook :: Book -> Library -> Library
referenceBook newBook library = case maybeABook of Nothing -> library { books = newBook:(books library) }
                                                   _       -> library
                                where maybeABook = findBook newBook library
            
findBook :: Book -> Library -> Maybe Book
findBook bookToFind library = find (== bookToFind) (books library) 


setEbookAvailability :: ISBN -> IsAvailableAsEbook -> Library -> Library
setEbookAvailability bookIsbn isAvailable library = Library $
                                                map (\b -> 
                                                  if bookIsbn == isbn b
                                                    then b { isAvailableAsEbook = isAvailable }
                                                    else b
                                                  )
                                                  (books library)

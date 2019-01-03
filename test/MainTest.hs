import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "List the books of the Akeneo Library"$ do
    it "Does not have any books" $
      listBooks Library `shouldBe` []

    {-it "lists the Akeneo Books from the library" $-}
      {-listBooks Library `shouldBe` [BookA, BookB, BookC]-}

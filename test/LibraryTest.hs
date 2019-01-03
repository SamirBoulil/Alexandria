import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "List the books of the Akeneo Library"$ do
    it "Does not have any books" $
      listBooks (Library []) `shouldBe` []


data ISBN = ISBN String deriving (Show, Eq)
data Title = Title String deriving (Show, Eq)
data Author = Author String deriving (Show, Eq)
data Book = Book ISBN Title Author deriving (Show, Eq)
data Library = Library [Book] deriving (Show, Eq)


listBooks :: Library -> [Book]
listBooks _ = []


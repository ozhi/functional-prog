type Title = String
type Year = Int
type Book = (Title, Year)
type Library = [Book]

main :: IO()
main = do
	print (findUniques lib) -- → [”SICP”,”Real World Haskell”]
	print (longestTitleYear lib) -- → 2011
	
	where
	lib :: Library
	lib = [ ("SICP", 1996),
 			("Learn You a Haskell for Great Good", 2011),
 			("Real World Haskell", 2008),
 			("Programming in Haskell", 2011)]


findUniques :: Library -> [Title]
-- get the titles of all books, for which there are no other books of the same year in the library
findUniques library = (map getTitle (filter noOtherBooksFromSameYear library))
	where
	
	noOtherBooksFromSameYear :: Book -> Bool
	noOtherBooksFromSameYear book = ((length (filter isOfSameYearAsBook library)) <= 1)
		where
		isOfSameYearAsBook :: Book -> Bool
		isOfSameYearAsBook (_, year) = (year == (getYear book))

longestTitleYear :: Library -> Year -- get the year of the book with longest title
longestTitleYear library = (getYear (bookOfLongestTitle library))

    where
    bookOfLongestTitle :: Library -> Book
    bookOfLongestTitle [] = ("", 0)
    bookOfLongestTitle (book:restOfLibrary) =
        if ((length (getTitle book)) >= (length (getTitle bookOfLongestTitleInRestOfLibrary)))
        then book
        else bookOfLongestTitleInRestOfLibrary
        
        where
        bookOfLongestTitleInRestOfLibrary :: Book
        bookOfLongestTitleInRestOfLibrary = (bookOfLongestTitle restOfLibrary)

getTitle :: Book -> Title
getTitle (title, _) = title

getYear :: Book -> Year
getYear (_, year) = year


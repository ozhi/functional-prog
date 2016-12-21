type Title = String
type Year = Int
type Book = (Title, Year)
type Library = [Book]

getTitle :: Book -> Title
getTitle (title, _) = title

getYear :: Book -> Year
getYear (_, year) = year

main :: IO()
main = do
	print (findUniques lib1) -- -> [”SICP”,”Real World Haskell”]
	print (longestTitleYear lib1) -- -> 2011
	
	print (findUniques lib2) -- -> ["bb","c",”ddd”,"f","h","ii"]
	print (longestTitleYear lib2) -- -> 2000
	
		where
		lib1 :: Library
		lib1 = [ ("SICP", 1996),
	 		  	 ("Learn You a Haskell for Great Good", 2011),
	 			 ("Real World Haskell", 2008),
	 			 ("Programming in Haskell", 2011)]
	 	
	 	lib2 :: Library
		lib2 = [ ("a"    , 2000),
	 		  	 ("bb"   , 2011),
	 			 ("c"    , 2002),
	 			 ("ddd"  , 2009),
	 			 ("ee"   , 2000),
	 			 ("f"    , 2004),
	 			 ("ggggg", 2000),
	 			 ("h"    , 2003),
	 			 ("ii"   , 2010)] 	

-- get the titles of all books, for which there are no other books of the same year in the library
findUniques :: Library -> [Title]
findUniques library = map getTitle (filter noOtherBooksFromSameYear library)
	
	where
	noOtherBooksFromSameYear :: Book -> Bool
	noOtherBooksFromSameYear book =
		(length (filter (\ (_,year) -> year==(getYear book)) library)) <= 1

longestTitleYear :: Library -> Year -- get the year of the book with longest title
longestTitleYear library = getYear (bookOfLongestTitle library)

    where
    bookOfLongestTitle :: Library -> Book
    bookOfLongestTitle [] = ("", 0)
    bookOfLongestTitle (book:restOfLibrary) =
        if (length (getTitle book)) >= (length (getTitle bookOfLongestTitleInRestOfLibrary))
        then book
        else bookOfLongestTitleInRestOfLibrary
        
        where
        bookOfLongestTitleInRestOfLibrary :: Book
        bookOfLongestTitleInRestOfLibrary = bookOfLongestTitle restOfLibrary


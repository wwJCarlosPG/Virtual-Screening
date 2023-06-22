module DSLData
where

data Request = SelectRequest{bullseye::String, file::String, function::String, fileToSave::String}
                |OpenRequest{file::String, condition::String}
                |SortRequest{file::String, sortingFunction::String}
                |OpenAndSaveRequest{file::String, condition::String, fileToSave::String}
                |PutRequest{request::String, file::String}

{-aminoacidos hidrofobicos-}
hidrophobAA :: [Char]
hidrophobAA = ['M', 'A', 'G', 'C', 'T', 'P', 'Y', 'S']



module DSLData
where

data Request = SelectRequest{bullseye::String, file::String, function::String}
                |OpenRequest{file::String, condition::String}
                |SortRequest{file::String, sortingFunction::String}
                |PutRequest{request::[String], fileToSave::String}
instance Show Request where
  show (SelectRequest bullseye file function) = "SelectRequest { bullseye = " ++ show bullseye ++ ", file = " ++ show file ++ ", function = " ++ show function ++ " }"
  show (OpenRequest file condition) = "OpenRequest { file = " ++ show file ++ ", condition = " ++ show condition ++ " }"
  show (SortRequest file sortingFunction) = "SortRequest { file = " ++ show file ++ ", sortingFunction = " ++ show sortingFunction ++ " }"
  show (PutRequest request fileToSave) = "PutRequest { request = " ++ show request ++ ", fileToSave = " ++ show fileToSave ++ " }"

{-aminoacidos hidrofobicos-}
hidrophobAA :: [Char]
hidrophobAA = ['M', 'A', 'G', 'C', 'T', 'P', 'Y', 'S']



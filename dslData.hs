module DSLData
where

data Request = SelectRequest{bullseye::String, file::String}
                |OpenRequest{file::String, condition::String}
                

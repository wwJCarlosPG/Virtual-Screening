module DSLData
where

data Request = SelectRequest{bullseye::String, file::String, condition::String}
                |OpenRequest{file::String, condition::String}
                

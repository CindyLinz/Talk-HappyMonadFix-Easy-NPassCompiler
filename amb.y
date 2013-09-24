{

module Main where

-- vim: filetype=haskell

}

%name ambParse
%tokentype { T }
%error { parseError }

%token
  T { T }

%%

program :: { [Char] }
  : T T T { "3" }
  | T inject T { "2" }

inject :: { [Char] }
  : { "I" }

{

data T = T

}

if exists('b:current_syntax') | finish|  endif

"Keywords and functions
syntax keyword waccKeyword bool done return if then else fi while do tuple pair int char string newpair newtuple fst snd
syntax keyword waccFunction print println skip
highlight link waccKeyword Keyword
highlight link waccFunction Function

"Comments
syntax match waccComment "\v#.*$"
highlight link waccComment Comment

"Characters
syntax match waccChar "\'.\'"
highlight waccChar ctermfg=226 guifg=#ffff00 

"Operators
syntax match waccOperator "\v\*"
syntax match waccOperator "\v/"
syntax match waccOperator "\v\+"
syntax match waccOperator "\v-"
syntax match waccOperator "\v\="
syntax match waccOperator "\v\<"
syntax match waccOperator "\v\>"
highlight link waccOperator Operator

"Strings
syntax region waccString start=/\v"/ skip=/\v\\./ end=/\v"/
highlight link waccString String
hi String ctermfg=124 guifg=#af0000

syntax match Begin "begin"
syntax match End "end"
syntax match Is "is"
highlight Begin ctermfg=106 guifg=#87af00
highlight End ctermfg=106 guifg=#87af00
highlight Is ctermfg=106 guifg=#87af00

syntax match True "true"
syntax match False "false"
highlight True ctermfg=89 guifg=#87005f 
highlight False ctermfg=89 guifg=#87005f

let b:current_syntax = 'wacc'

{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( parseYaml
    ) where
    
import Text.Parsec
import Data.Functor.Identity

type YParser s = ParsecT s () Identity

data YSymbol 
    = C_printable           Char
    | NB_json               Char
    | C_byte_order_mark
    | C_sequence_entry
    | C_mapping_key                         -- 5
    | C_mapping_value
    | C_collect_entry
    | C_sequenct_start
    | C_sequence_end
    | C_mapping_start                       -- 10
    | C_mapping_end
    | C_comment
    | C_anchor
    | C_alias
    | C_tag                                 -- 15
    | C_literal
    | C_folded
    | C_single_quote
    | C_double_quote
    | C_directive                           -- 20
    | C_reserved            Char
    | C_indicator           YSymbol
    | C_flow_indicator      YSymbol
    | B_line_feed
    | B_carriage_return                     -- 25
    | B_char                YSymbol
    | NB_char               Char
    | B_break               [YSymbol]
    | B_as_line_feed        
    | B_non_content         YSymbol         -- 30
        

data YSerialNode s
    = YSNSeq String (s YSerialNode)     -- tag, sequence of nodes
    | YSNAlias String String            -- tag, alias name
    | YSNScalar String String           -- tag, content
    | YSNMap String (m YSerialNode YSerialNode) -- tag, map between nodes

data Context
    = Block_in
    | Block_out
    | Flow_in
    | Flow_out
    | Block_key
    | Flow_key

-- YAML 1.2: chapter 5: Characters
 
-- 1    
c_printable :: Stream s Identity Char => YParser s Char
c_printable = satisfy (\c ->
    ( '\x20' <= c && c <= '\x7d')
    || c == '\n' 
    || c == '\r' 
    || c == \t' 
    || c == '\x85' 
    || ( '\xA0' <= c && c <= '\xD7FF')
    || ( '\xE000' <= c && c <= '\xFFFD')
    || ( '\x10000' <= c && c <= '\x10FFFF')
    )
    
-- 2
nb_json :: Stream s Identity Char => YParser s Char
nb_json = satisfy (\c -> c == '\x09' || (c <= '\x10FFFF' && c >= '\x20'))

-- 3
c_byte_order_mark :: Stream s Identity Char => YParser s Char
c_byte_order_mark = char '\xFEFF'

-- 4
c_sequence_entry :: Stream s Identity Char => YParser s Char
c_sequence_entry = char '-'

-- 5
c_mapping_key :: Stream s Identity Char => YParser s Char
c_mapping_key = char '?'

-- 6    
c_mapping_value :: Stream s Identity Char => YParser s Char
c_mapping_value = char ':'

-- 7
c_collect_entry :: Stream s Identity Char => YParser s Char
c_collect_entry = char ','

-- 8
c_sequence_start :: Stream s Identity Char => YParser s Char
c_sequence_start = char '['

-- 9
c_sequence_end :: Stream s Identity Char => YParser s Char
c_sequence_end = char ']'
    
-- 10
c_mapping_start :: Stream s Identity Char => YParser s Char
c_mapping_start = char '{'
    
-- 11
c_mapping_end :: Stream s Identity Char => YParser s Char
c_mapping_end = char '}'
    
-- 12
c_comment :: Stream s Identity Char => YParser s Char
c_comment = char '#'
    
-- 13
c_anchor :: Stream s Identity Char => YParser s Char
c_anchor = char '&'

-- 14
c_alias :: Stream s Identity Char => YParser s Char
c_alias = char '*'

-- 15
c_tag :: Stream s Identity Char => YParser s Char
c_tag = char '!'

-- 16
c_literal :: Stream s Identity Char => YParser s Char
c_literal = char '|'

-- 17
c_folded :: Stream s Identity Char => YParser s Char
c_folded = char '>'

-- 18
c_single_quote :: Stream s Identity Char => YParser s Char
c_single_quote = char '\''
    
-- 19
c_double_quote :: Stream s Identity Char => YParser s Char
c_double_quote = char '"'
    
-- 20
c_directive :: Stream s Identity Char => YParser s Char
c_directive = char '%'
    
-- 21
c_reserved :: Stream s Identity Char => YParser s Char
c_reserved = oneOf "@`"

-- 22
c_indicator :: Stream s Identity Char => YParser s Char
c_indicator = choice 
    [ c_sequence_entry
    , c_mapping_key
    , c_mapping_value
    , c_collect_entry
    , c_sequence_start
    , c_sequence_end
    , c_mapping_start
    , c_mapping_end
    , c_comment
    , c_anchor
    , c_alias
    , c_tag
    , c_literal
    , c_folded
    , c_single_quote
    , c_double_quote
    , c_directive
    , c_reserved
    ]

-- 23    
c_flow_indicator :: Stream s Identity Char => YParser s Char
c_flow_indicator = choice
    [ c_collect_entry
    , c_sequence_start
    , c_sequence_end
    , c_mapping_start
    , c_mapping_end
    ] <?> "one of \"[](),\""
    
-- 24
b_line_feed :: Stream s Identity Char => YParser s Char
b_line_feed = char '\n'

-- 25
b_carriage_return :: Stream s Identity Char => YParser s Char
b_carriage_return = char '\r'

-- 26
b_char :: Stream s Identity Char => YParser s Char
b_char = choice
    [ b_line_feed
    , b_carriage_return
    ] <?> "\\n or \\r"

-- 27
nb_char :: Stream s Identity Char => YParser s Char
nb_char = satisfy (\c -> -- same as c_printable except \n and \r
    ( '\x20' <= c && c <= '\x7d')
    || c == '\t' 
    || c == '\x85' 
    || ( '\xA0' <= c && c <= '\xD7FF')
    || ( '\xE000' <= c && c <= '\xFFFD' && c /= '\xFEFF')
    || ( '\x10000' <= c && c <= '\x10FFFF')
    )
    
-- 28
b_break :: Stream s Identity Char => YParser s Char
b_break = choice
    [ try $ do
        b_carriage_return
        b_line_feed
        return '\n'
    , b_line_feed
    , b_carriage_return
    ]
    
-- 29 
-- all combinations are parsed as line feeds inside scalars (YAML 1.2 Ch 5.4)
b_as_line_feed :: Stream s Identity Char => YParser s Char
b_as_line_feed = do
    b_break
    return '\n'

-- 30
b_non_content :: Stream s Identity Char => YParser s ()
b_non_content = do
    b_break
    return ()

-- 31
s_space :: Stream s Identity Char => YParser s Char
s_space = char '\x20'

-- 32
s_tab :: Stream s Identity Char => YParser s Char
s_tab = char '\x09'

-- 33
s_white :: Stream s Identity Char => YParser s Char
s_white = choice
    [ s_space
    , s_tab
    ] 
    
-- 34
ns_char :: Stream s Identity Char => YParser s Char
ns_char = satisfy (\c ->    -- is equal to nb_char except s_white
    ( '\x21' <= c && c <= '\x7d')
    || c == '\x85' 
    || ( '\xA0' <= c && c <= '\xD7FF')
    || ( '\xE000' <= c && c <= '\xFFFD' && c /= '\xFEFF')
    || ( '\x10000' <= c && c <= '\x10FFFF')
    ) <?> "a non-whitespace character"
    
-- 35
ns_dec_digit :: Stream s Identity Char => YParser s Char
ns_dec_digit = digit

-- 36
ns_hex_digit :: Stream s Identity Char => YParser s Char
ns_hex_digit = hexDigit

-- 37
ns_ascii_letter :: Stream s Identity Char => YParser s Char
ns_ascii_letter = satisfy (\c ->
    ('\x41' <= c && c <= '\x5A')
    || ('\x61' <= c && c <= '\x7A')
    ) <?> "a character in a-z or A-Z"

-- 38 
ns_word_char :: Stream s Identity Char => YParser s Char
ns_word_char = choice 
    [ ns_dec_digit
    , ns_ascii_letter
    , char '-'
    ] <?> "an ASCII letter, a number, or \"-\""
    
-- 39
-- Must keep formatting of uri characters as-is (YAML 1.2 Ch 5.6)
ns_uri_char :: Stream s Identity Char => YParser s String
ns_uri_char = choice
    [ try $ do
        char '%'
        f <- ns_hex_digit
        s <- ns_hex_digit
        return $ ['%', f, s]
    , do
        c <- ns_word_char
        return $ [c]
    , do 
        c <- oneOf "#;/?:@&=+$,_.!~*'()[]"
        return $ [c]
    ] <?> "valid uri character"

-- 40
ns_tag_char :: Stream s Identity Char => YParser s String
ns_tag_char = choice  -- same as NS_uri_char except all of "![]()," are not allowed
    [ try $ do
        char '%'
        f <- ns_hex_digit
        s <- ns_hex_digit
        return $ ['%', f, s]
    , do 
        c <- ns_word_char
        return $ [c]
    , do
        c <- oneOf "#;/?:@&=+$_.~*'"
        return $ [c]
    ] <?> "valid tag character"
        
-- Escape sequences are presentation details only.
-- 41
c_escape :: Stream s Identity Char => YParser s ()
c_escape = do 
    char '\\'
    return ()

-- 42
ns_esc_null :: Stream s Identity Char => YParser s Char
ns_esc_null = do
    char '0'
    return '\x00'

-- 43
ns_esc_bell :: Stream s Identity Char => YParser s Char
ns_esc_bell = do
    char 'a'
    return '\x07'

-- 44
ns_esc_backspace :: Stream s Identity Char => YParser s Char
ns_esc_backspace = do
    char 'b'
    return '\x08'

-- 45
ns_esc_horizontal_tab :: Stream s Identity Char => YParser s Char
ns_esc_horizontal_tab = do
    choice
        [ char 't'
        , char '\x09'
        ]
    return '\x09'

-- 46
ns_esc_line_feed :: Stream s Identity Char => YParser s Char
ns_esc_line_feed = do 
    char 'n'
    return '\x0A'

-- 47
ns_esc_vertical_tab :: Stream s Identity Char => YParser s Char
ns_esc_vertical_tab = do 
    char 'v'
    return '\x0B'

-- 48
ns_esc_form_feed :: Stream s Identity Char => YParser s Char
ns_esc_form_feed = do
    char 'f'
    return '\x0C'

-- 49
ns_esc_carriage_return :: Stream s Identity Char => YParser s Char
ns_esc_carriage_return = do
    char 'r'
    return '\x0d'

-- 50
ns_esc_escape :: Stream s Identity Char => YParser s Char
ns_esc_escape = do
    char 'e'
    return '\x1B'

-- 51
ns_esc_space :: Stream s Identity Char => YParser s Char
ns_esc_space = char ' '

-- 52
ns_esc_double_quote :: Stream s Identity Char => YParser s Char
ns_esc_double_quote = char '"'

-- 53
ns_esc_slash :: Stream s Identity Char => YParser s Char
ns_esc_slash = char '/'

-- 54
ns_esc_backslash :: Stream s Identity Char => YParser s Char
ns_esc_backslash = char '\\'

-- 55
ns_esc_next_line :: Stream s Identity Char => YParser s Char
ns_esc_next_line = do
    char 'N'
    return '\x85'

-- 56
ns_esc_non_breaking_space :: Stream s Identity Char => YParser s Char
ns_esc_non_breaking_space = do
    char '_'
    return '\xA0'
    
-- 57
ns_esc_line_separator :: Stream s Identity Char => YParser s Char
ns_esc_line_separator = do
    char 'L'
    return '\x2028'

-- 58
ns_esc_paragraph_separator :: Stream s Identity Char => YParser s Char
ns_esc_paragraph_separator = do
    char 'P'
    return '\x2029'

-- 59
ns_esc_8_bit :: Stream s Identity Char => YParser s Char
ns_esc_8_bit = do
    char 'x'
    eightBit <- count 2 ns_hex_digit
    return . head . read $ ("\"\\x" ++ eightBit ++ "\"")
    
-- 60
ns_esc_16_bit :: Stream s Identity Char => YParser s Char
ns_esc_16_bit = do
    char 'u'
    sixteenBit <- count 4 ns_hex_digit
    return . head . read $ ("\"\\x" ++ sixteenBit ++ "\"")
    
-- 61
ns_esc_32_bit :: Stream s Identity Char => YParser s Char
ns_esc_32_bit = do
    char 'U'
    thirtyTwoBit <- count 8 ns_hex_digit
    return . head . read $ ("\"\\x" ++ thirtyTwoBit ++ "\"")
    
-- 62
c_ns_esc_char :: Stream s Identity Char => YParser s Char
c_ns_esc_char = do
    char '\\'
    choice
        [ ns_esc_null
        , ns_esc_bell
        , ns_esc_backspace
        , ns_esc_horizontal_tab
        , ns_esc_line_feed
        , ns_esc_vertical_tab
        , ns_esc_form_feed
        , ns_esc_carriage_return
        , ns_esc_escape
        , ns_esc_space
        , ns_esc_double_quote
        , ns_esc_slash
        , ns_esc_backslash
        , ns_esc_next_line
        , ns_esc_non_breaking_space
        , ns_esc_line_separator
        , ns_esc_paragraph_separator
        , ns_esc_8_bit
        , ns_esc_16_bit
        , ns_esc_32_bit
        ]
    
-- 63.0
s_indent :: Stream s Identity Char => YParser s Int
s_indent = do
    string <- many s_space
    return . length $ string

-- 63.5
-- is a presentation detail
s_indent_n :: Stream s Identity Char => Int -> YParser s ()
s_indent_n n = do
    count n s_space
    return ()
    
-- 64
s_indent_lt_n :: Stream s Identity Char => Int -> YParser s Int
s_indent_lt_n n = do
    r <- many s_space
    if length r < n
        then return . length $ r
        else unexpected "indentation level"
        
-- 65
s_indent_lteq_n :: Stream s Identity Char => Int -> YParser s Int
s_indent_lteq_n n = do
    r <- many s_space
    if length r <= n
        then return . length $ r
        else unexpected "indentation level"
        
-- 66
s_start_of_line :: Stream s Identity Char => YParser s ()
s_start_of_line = do
    pos <- getPosition
    guard (sourceColumn pos == 1)

-- is a Presentation detail: representation in serialisation tree not allowed.
s_separate_in_line :: Stream s Identity Char => YParser s () 
s_separate_in_line = do choice 
    [ do 
        many1 s_white
        return ()
    , s_start_of_line
    ]

-- 67
-- Is a presentation detail
s_line_prefix :: Stream s Identity Char => Int -> Context -> YParser s ()
s_line_prefix n Block_out = s_block_line_prefix n
s_line_prefix n Block_in = s_block_line_prefix n
s_line_prefix n Flow_out = s_flow_line_prefix n
s_line_prefix n Flow_in = s_flow_line_prefix n
s_line_prefix _ _ = error "Wrong context"

-- 68
s_block_line_prefix :: Stream s Identity Char => Int -> YParser s ()
s_block_line_prefix n = s_indent_n n

-- 69
s_flow_line_prefix :: Stream s Identity Char => Int -> YParser s ()
s_flow_line_prefix n = do
    s_indent_n n
    optional s_separate_in_line   -- written as "s_separate_in_line?" in the spec
    return ()
    
-- 70
l_empty :: Stream s Identity Char => Int -> Context -> YParser s Char
l_empty n c = do
    choice 
        [ try $ s_line_prefix n c
        , s_indent_lt_n n
        ]
    b_as_line_feed

-- 71
b_l_trimmed :: Stream s Identity Char => Int -> Context -> YParser s String
b_l_trimmed n c = do
    b_non_content
    many1 $ l_empty n c
    
-- 72
b_as_space :: Stream s Identity Char => YParser s Char
b_as_space = do
    b_break
    return ' '
    
-- 73
b_l_folded :: Stream s Identity Char => Int -> Context -> YParser s String
b_l_folded n c = choice
    [ try $ b_l_trimmed n c
    , do
        b_as_space
        return " "
    ]

-- 74
s_flow_folded :: Stream s Identity Char => Int -> YParser s String
s_flow_folded n = do
    optional s_separate_in_line
    lines <- b_l_folded n Flow_in
    s_flow_line_prefix n
    return lines
    
-- 75
c_nb_comment_text :: Stream s Identity Char => YParser s ()
c_nb_comment_text = do
    char '#'
    many nb_char
    return ()
    
-- 76
b_comment :: Stream s Identity Char => YParser s ()
b_comment = 
    b_non_content
    <|> eof
    <?> "end of line"

-- 77
s_b_comment :: Stream s Identity Char => YParser s ()
s_b_comment = do
    optional (try $ do
        s_separate_in_line
        optional c_nb_comment_text)
    b_comment
    
-- 78
l_comment :: Stream s Identity Char => YParser s ()
l_comment = do
    s_separate_in_line
    optional c_nb_comment_text
    b_comment
    
-- 79
s_l_comments :: Stream s Identity Char => YParser s ()
s_l_comments = do
    (try $ s_b_comment <|> s_start_of_line <?> "whitespace or comment")
    skipMany l_comment
    
-- 80
s_separate :: Stream s Identity Char => Int -> Context -> YParser s ()
s_separate n Block_out  = s_separate_lines n
s_separate n Block_in   = s_separate_lines n
s_separate n Flow_out   = s_separate_lines n
s_separate n Flow_in    = s_separate_lines n
s_separate n Block_key  = s_separate_in_line
s_separate n Flow_key   = s_separate_in_line

-- 81
s_separate_lines :: Stream s Identity Char => Int -> YParser s ()
s_separate_lines n = choice
    [ try $ do
        s_l_comments
        s_flow_line_prefix n
    , s_separate_in_line
    ]

-- 82
l_directive :: Stream s Identity Char => YParser s ()
l_directive = do
    char '%'
    choice 
        [ try ns_yaml_directive
        , try ns_tag_directive
        , ns_reserved_directive
        ]
    s_l_comments
    
-- 83
ns_reserved_directive :: Stream s Identity Char => YParser s ()
ns_reserved_directive = do
    ns_directive_name
    many $ do
        s_separate_in_line
        ns_directive_parameter

-- 84
ns_directive_name :: Stream s Identity Char => YParser s String
ns_directive_name = many1 ns_char

-- 85
ns_directive_parameter :: Stream s Identity Char => YParser s String
ns_directive_name = many1 ns_char

-- 86
ns_yaml_directive :: Stream s Identity Char => YParser s ()
ns_yaml_directive = string "YAML"

-- 87
ns_yaml_version :: Stream s Identity Char => YParser s String
ns_yaml_version = do 
    whole <- many1 ns_dec_digit
    char '.'
    fractional <- many1 ns_dec_digit
    return (whole ++ "." ++ fractional)
    
-- 88
ns_tag_directive :: Stream s Identity Char => YParser s ()
ns_tag_directive = string "TAG"

-- 89
c_tag_handle :: Stream s Identity Char => YParser s String
c_tag_handle = choice
    [ try $ c_named_tag_handle
    , try $ c_secondary_tag_handle
    , do 
        c_primary_tag_handle
        return "!"
    ]
    
-- 90
c_primary_tag_handle :: Stream s Identity Char => YParser s Char
c_primary_tag_handle = char '!'

-- 91
c_secondary_tag_handle :: Stream s Identity Char => YParser s String
c_secondary_tag_handle = string "!!"

-- 92
c_named_tag_handle :: Stream s Identity Char => YParser s String
c_named_tag_handle = do
    char '!'
    tagName <- many1 ns_char
    char '!'
    return '!' : (tagName ++ "!")
    
-- 93
ns_tag_prefix :: Stream s Identity Char => YParser s String
ns_tag_prefix = choice 
    [ try $ c_ns_local_tag_prefix
    , ns_global_tag_prefix
    ]
    
-- 94
c_ns_local_tag_prefix :: Stream s Identity Char => YParser s String
c_ns_local_tag_prefix = do
    char '!'
    tagName <- many1 ns_uri_char
    return ('!' : tagName)
    
-- 95
ns_global_tag_prefix :: Stream s Identity Char => YParser s String
ns_global_tag_prefix = do
    first <- ns_tag_char
    rest <- many ns_uri_char
    return $ first : rest
    
-- 96
c_ns_properties :: Stream s Identity Char => Int -> Context -> YParser s ()
c_ns_properties n c = choice
    [ try $ do
        c_ns_tag_property
        optional try $ do
            s_separate n c
            c_ns_anchor_property
    , do
        c_ns_anchor_property
        optional try $ do
            s_separate n c
            c_ns_tag_property
    ]
    return () -- TODO: return some useful information
    
-- 97
c_ns_tag_property :: Stream s Identity Char => YParser s String
c_ns_tag_property = choice
    [ try $ c_verbatim_tag
    , try $ c_ns_shorthand_tag
    , c_non_specific_tag
    ]
    
-- 98
c_verbatim_tag :: Stream s Identity Char => YParser s String
c_verbatim_tag = do
    string "!<"
    tagName <- many1 ns_uri_char
    char '>'
    return $ "!<" ++ tagName ++ ">"
    
-- 99
c_ns_shorthand_tag :: Stream s Identity Char => YParser s String
c_ns_shorthand_tag = do
    prefix <- c_tag_handle
    suffix <- many1 ns_tag_char
    return prefix ++ suffix


-- 100
c_non_specific_tag :: Stream s Identity Char => YParser s Char 
c_non_specific_tag = char '!'


-- 101
c_ns_anchor_property :: Stream s Identity Char => YParser s Char  
c_ns_anchor_property = do
    char '&'
    name <- ns_anchor_name
    return (namew : ns_anchor_name)

-- 102
ns_anchor_char :: Stream s Identity Char => YParser s Char
ns_anchor_char = satisfy (\c ->    -- is equal to ns_char except "[]{},"
    (notElem c "[]{}," ) &&
        (( '\x21' <= c && c <= '\x7d')
            || c == '\x85' 
            || ( '\xA0' <= c && c <= '\xD7FF')
            || ( '\xE000' <= c && c <= '\xFFFD' && c /= '\xFEFF')
            || ( '\x10000' <= c && c <= '\x10FFFF'))) 
    <?> "a non-whitespace character"
 
-- 103
ns_anchor_name :: Stream s Identity Char => YParser s String
ns_anchor_name = many1 ns_anchor_char

-- 104
c_ns_alias_node :: Stream s Identity Char => YParser s String
c_ns_alias_node = do
    char '*'
    ns_anchor_name

-- 105
e_scalar :: Stream s Identity Char => YParser s ()
e_scalar = return ()

-- 106
e_node :: Stream s Identity Char => YParser s ()
e_node = e_scalar

-- 107
nb_double_char :: Stream s Identity Char => YParser s Char
nb_double_char = choice 
    [ c_ns_esc_char
    , satisfy (\c -> 
        (notElem c "\\\"") && 
            (c == '\x09' 
            || (c <= '\x10FFFF' && c >= '\x20')))
    ] 

-- 108
ns_double_char :: Stream s Identity Char => YParser s Char
ns_double_char = choice 
    [ c_ns_esc_char -- same as above but without whitespace
    , satisfy (\c -> 
        (notElem c "\\\"\x20\x09") && -- not one of \ " space or tab
            (c == '\x09' 
            || (c <= '\x10FFFF' && c >= '\x20')))
    ] 

-- 109
c_double_quoted :: Stream s Identity Char => Int -> Context -> YParser s String
c_double_quoted n c = between (char '"') (char '"') (nb_double_text n c)    

-- 110
nb_double_text :: Stream s Identity Char => Int -> Context -> YParser s String
nb_double_text n Flow_out = nb_double_multi_line n
nb_double_text n Flow_in = nb_double_multi_line n
nb_double_text _ BLock_key = nb_double_one_line
nb_double_text _ Flow_key = nb_double_one_line

-- 111
nb_double_one_line =  Stream s Identity Char => YParser s String
nb_double_one_line = many ns_double_char

-- 112
s_double_escaped :: Stream s Identity Char => Int -> YParser s String
s_double_escaped n = do
    trailing <- many s_white
    char '\\'
    b_non_content
    lines <- many (l_empty n Flow_in)
    s_flow_line_prefix n
    return (trailing ++ lines)

-- 113
s_double_break :: Stream s Identity Char => Int -> YParser s String 
s_double_break n = choice
    [ try (s_double_escaped n)
    , s_flow_folded n
    ]

-- 114
nb_ns_double_in_line :: Stream s Identity Char => YParser s String 
nb_ns_double_in_line = do
    strings <- many do
        spaces <- many s_white
        char <- ns_double_char
        return (spaces ++ char)
    return (concat strings)

-- 115
s_double_next_line :: Stream s Identity Char => Int -> YParser s String 
s_double_next_line n = do
    whitespace <- s_double_break n
    next <- try (option "" do
        firstChar <- ns_double_char
        restOfLine <- nb_ns_double_in_line
        nextLines <- choice
            [ try (s_double_next_line n)
            , many s_white
            ] )
        return (firstChar : (restOfLine ++ nextLines))
    return (whitespace ++ next)

-- 116
nb_double_multi_line :: Stream s Identity Char => Int -> YParser s String 
nb_double_multi_line n = do
    line <- nb_ns_double_in_line
    rest <- choice
        [ try (s_double_next_line n)
        , many s_white
        ]
    return (line ++ rest)

-- 117
c_quoted_quote :: Stream s Identity Char => YParser s Char
c_quoted_quote = do
    string "''"
    return '\''

-- 118
nb_single_char :: Stream s Identity Char => YParser s Char
nb_single_char = choice
    [ try c_quoted_quote
    , satisfy (\c -> 
        (c != '\'') && (c == '\x09' || (c <= '\x10FFFF' && c >= '\x20')))
    ]

-- 119
ns_single_char :: Stream s Identity Char => YParser s Char
ns_single_char = choice
    [ try c_quoted_quote
    , satisfy (\c -> 
        (notElem c "' ") && (c == '\x09' || (c <= '\x10FFFF' && c >= '\x20')))
    ]

-- 120
c_single_quoted :: Stream s Identity Char => Int -> Context -> YParser s String
c_single_quoted = between (char '\'') (char '\'') (nb_single_text n c)

-- 121
nb_single_text :: Stream s Identity Char => Int -> Context -> YParser s String
nb_single_text n Flow_out = nb_single_multi_line n
nb_single_text n Flow_in = nb_single_multi_line n
nb_single_text _ Block_key = nb_single_one_line
nb_single_text _ Flow_key = nb_single_one_line

-- 122 
nb_single_one_line :: Stream s Identity Char => YParser s String
nb_single_one_line = many nb_single_char  
    
-- 123
nb_ns_single_in_line :: Stream s Identity Char => YParser s String
nb_ns_single_in_line = do
    strings <- many do
        spaces <- many s_white
        char <- ns_singlechar
        return (spaces ++ char)
    return (concat strings)

-- 124
s_single_next_line :: Stream s Identity Char => Int -> YParser s String
s_single_next_line n = do
    whitespace <- s_flow_folded n
    next <- try (option "" do
        firstChar <- ns_single_char
        restOfLine <- nb_ns_single_in_line
        nextLines <- choice
            [ try (s_single_next_line n)
            , many s_white
            ] )
        return (firstChar : (restOfLine ++ nextLines))
    return (whitespace ++ next)

-- 125 
nb_single_multi_line :: Stream s Identity Char => Int -> YParser s String
nb_single_multi_line n = do 
    line <- nb_ns_single_in_line
    nextLines <- choice
        [ try (s_single_next_line n)
        , many s_white
        ] )
    return (line ++ nextLines)

-- 126 
ns_plain_first :: Stream s Identity Char => Context -> YParser s Char
ns_plain_first c = choice 
    [ do
        notFollowedBy c_flow_indicator
        ns_char 
    , do
        ch <- oneOf "?:-"
        try . lookAhead (ns_plain_safe c)
        return ch


-- 127
ns_plain_safe :: Stream s Identity Char => Context -> YParser s Char
ns_plain_safe Flow_out = ns_plain_safe_out
ns_plain_safe Flow_in = ns_plain_safe_in
ns_plain_safe Block_out = ns_plain_safe_out
ns_plain_safe Block_in = ns_plain_safe_in
ns_plain_safe _ = error "ns_plain_safe: Invalid context"

-- 128
ns_plain_safe_out :: Stream s Identity Char => YParser s Char
ns_plain_safe_out = ns_char

-- 129
ns_plain_safe_in :: Stream s Identity Char => YParser s Char
ns_plain_safe_in = do
    notFollowedBy c_flow_indicator
    ns_char 
 
-- 130
-- pass previous character to this parser to make sure it's not a space.
s_ns_plain_char :: Stream s Identity Char => Context -> YParser s Char
s_ns_plain_char c = choice
    [ do
        notFollowedBy . oneOf $ ":#"
        ns_plain_safe c
    , do
        char ':'
        try . lookAhead (ns_plain_safe c)
        return ':'
    ]

ns_ns_plain_char :: Stream s Identity Char => Context -> YParser s Char
ns_ns_plain_char c = choice
    [ do
        notFollowedBy . char $ ':'
        ns_plain_safe c
    , do
        char ':'
        try . lookAhead (ns_plain_safe c)
        return ':'
    ]

-- 131
ns_plain :: Stream s Identity Char => Int -> Context -> YParser s String
ns_plain n Flow_out = ns_plain_multi_line n c
ns_plain n Flow_in = ns_plain_multi_line n c
ns_plain n Block_key = ns_plain_one_line c
ns_plain n Flow_key = ns_plain_one_line c
ns_plain _ _ = error "ns_plain: Invalid context."

-- 132
-- must include check to make sure ns_plain_char is preceeded by a space
nb_ns_plain_in_line :: Stream s Identity Char => Context -> YParser s String
nb_ns_plain_in_line c = do
    strings <- many . choice
        [ do 
            whitespace <- many1 s_white
            ch <- s_ns_plain_char c 
            return (whitespace ++ [ch])
        , do
            ch <- ns_ns_plain_char
            return [ch]
    return . concat $ strings

-- 133
ns_plain_one_line :: Stream s Identity Char => Context -> YParser s String
ns_plain_one_line c = do
    ch <- ns_plain_first c
    line <- nb_ns_plain_in_line c
    return (ch : line)

-- 134
s_ns_plain_next_line :: Stream s Identity Char => Int -> Context -> YParser s String
s_ns_plain_next_line n c = do
    whitespace <- s_flow_folded n
    ch <- if (n == 0 && null whitespace)
        then ns_ns_plain_char c -- two different version for whitespace prefix.
        else s_ns_plain_char c 
    line <- nb_ns_plain_in_line c
    return (whitespace ++ [ch] ++ line)

-- 135
ns_plain_multi_line :: Stream s Identity Char => Int -> Context -> YParser s String
ns_plain_multi_line n c = do
    firstLine <- ns_plain_one_line c
    rest <- many s_ns_plain_test_line
    return (firstLine ++ concat rest)

-- 136










parseYaml = undefined

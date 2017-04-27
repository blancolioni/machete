with GCS.Lexer;
with GCS.Styles;                       use GCS.Styles;

with Machete.Parser.Tokens;            use Machete.Parser.Tokens;

pragma Elaborate_All (GCS.Lexer);

private package Machete.Parser.Lexical is
  new GCS.Lexer (Token              => Token,
                 Tok_None           => Tok_None,
                 Tok_End_Of_File    => Tok_End_Of_File,
                 Tok_Bad_Character  => Tok_Bad_Character,
                 Tok_Identifier     => Tok_Identifier,
                 Tok_String         => Tok_String_Literal,
                 Tok_Character      => Tok_Character_Literal,
                 Tok_Integer        => Tok_Integer_Constant,
                 Tok_Float          => Tok_None,
                 First_Keyword      => Tok_Dot,
                 Keywords           => ".",
                 First_Symbol       => Tok_Left_Paren,
                 Symbols            => "( ) , ; [ ] { } | ! []",
                 Identifier_Start   =>
                       "abcdefghijklmnopqrstuvwxyz"
                     & "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                     & "_"
                     & "#$&*+-./:<=>?@^~\",
                 Identifier_Body    =>
                       "abcdefghijklmnopqrstuvwxyz"
                     & "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                     & "0123456789"
                     & "_"
                     &  "#$&*+-./:<=>?@^~\",
                 Identifier_Group    => "#$&*+-./:<=>?@^~\",
                 Line_Comment_Start  => "%",
                 Block_Comment_Start => "/*",
                 Block_Comment_End   => "*/",
                 Properties          =>
                   (Multi_Characters           => True,
                    Case_Sensitive_Identifiers => True,
                    Two_Quote_Escape           => True,
                    others                     => False));

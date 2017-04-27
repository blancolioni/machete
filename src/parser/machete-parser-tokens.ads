private package Machete.Parser.Tokens is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier, Tok_Integer_Constant,
       Tok_String_Literal, Tok_Character_Literal,

       Tok_Dot,

       Tok_Left_Paren, Tok_Right_Paren, Tok_Comma, Tok_Semicolon,
       Tok_Left_Bracket, Tok_Right_Bracket,
       Tok_Left_Brace, Tok_Right_Brace,
       Tok_Vertical_Bar, Tok_Cut,
       Tok_Empty_List);

end Machete.Parser.Tokens;

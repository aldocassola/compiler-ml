type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum

val linePos = ErrorMsg.linePos

fun err(p1,p2) = ErrorMsg.error p1

val commentLevel = ref 0

val commentPos:pos list ref = ref []

val instring = ref false

val curstr = ref ""

val strpos = ref 0

fun makeIdToken (toktype, text, pos) = 
  toktype(text, pos, pos+size(text))

fun makeNumToken (toktype, text, pos) = 
  toktype(valOf (Int.fromString text), pos, pos+size(text))

fun makeToken (toktype, pos, text) = 
  toktype(pos, pos+size(text))

fun inc aref = aref := !aref+1

fun dec aref = aref := !aref-1

fun push levelref stackref pos =
  (inc levelref;
   stackref := (pos :: !stackref))

fun checkpop levelref stackref pos msg = 
  if !levelref > 0 then
    (dec levelref;
     stackref := tl(!stackref))
  else ErrorMsg.error pos msg

fun eof() = let 
  val pos = hd(!linePos)
in 
  if !instring then 
    ErrorMsg.error (!strpos) 
                   ("unterminated string constant " ^ 
                    !curstr)
  else if !commentLevel > 0 then
        ErrorMsg.error (hd(!commentPos)) "unterminated comment"
       else ();
  Tokens.EOF(pos,pos) 
end
%% 

ident = [a-zA-Z][a-zA-Z0-9_]* ;
digit = [0-9] ;
ws = [\ \t] ;

%s STRING ESCAPE COMMENT LINECONT ;

%%
\n => (push lineNum linePos yypos; lex());
<INITIAL>{ws}+ => (lex());
<INITIAL>type => (makeToken(Tokens.TYPE, yypos, yytext));
<INITIAL>array => (makeToken(Tokens.ARRAY, yypos, yytext));
<INITIAL>of => (makeToken(Tokens.OF, yypos, yytext));
<INITIAL>var => (makeToken(Tokens.VAR, yypos, yytext));
<INITIAL>nil => (makeToken(Tokens.NIL, yypos, yytext));
<INITIAL>function => (makeToken(Tokens.FUNCTION, yypos, yytext));
<INITIAL>let => (makeToken(Tokens.LET, yypos, yytext));
<INITIAL>in => (makeToken(Tokens.IN, yypos, yytext));
<INITIAL>end => (makeToken(Tokens.END, yypos, yytext));
<INITIAL>break => (makeToken(Tokens.BREAK, yypos, yytext));
<INITIAL>do => (makeToken(Tokens.DO, yypos, yytext));
<INITIAL>to => (makeToken(Tokens.TO, yypos, yytext));
<INITIAL>for => (makeToken(Tokens.FOR, yypos, yytext));
<INITIAL>while => (makeToken(Tokens.WHILE, yypos, yytext));
<INITIAL>else => (makeToken(Tokens.ELSE, yypos, yytext));
<INITIAL>then => (makeToken(Tokens.THEN, yypos, yytext));
<INITIAL>if => (makeToken(Tokens.IF, yypos, yytext));
<INITIAL>"." => (makeToken(Tokens.DOT, yypos, yytext));
<INITIAL>"," => (makeToken(Tokens.COMMA, yypos,yytext));
<INITIAL>":=" => (makeToken(Tokens.ASSIGN, yypos, yytext));
<INITIAL>":" => (makeToken(Tokens.COLON, yypos, yytext));
<INITIAL>";" => (makeToken(Tokens.SEMICOLON, yypos, yytext));
<INITIAL>"{" => (makeToken(Tokens.LBRACE, yypos, yytext));
<INITIAL>"}" => (makeToken(Tokens.RBRACE, yypos, yytext));
<INITIAL>"[" => (makeToken(Tokens.LBRACK, yypos, yytext));
<INITIAL>"]" => (makeToken(Tokens.RBRACK, yypos, yytext));
<INITIAL>"(" => (makeToken(Tokens.LPAREN, yypos, yytext));
<INITIAL>")" => (makeToken(Tokens.RPAREN, yypos, yytext));
<INITIAL>"&" => (makeToken(Tokens.AND, yypos, yytext));
<INITIAL>"|" => (makeToken(Tokens.OR, yypos, yytext));
<INITIAL>">" => (makeToken(Tokens.GT, yypos, yytext));
<INITIAL>">=" => (makeToken(Tokens.GE, yypos, yytext));
<INITIAL>"<" => (makeToken(Tokens.LT, yypos, yytext));
<INITIAL>"<=" => (makeToken(Tokens.LE, yypos, yytext));
<INITIAL>"=" => (makeToken(Tokens.EQ, yypos, yytext));
<INITIAL>"<>" => (makeToken(Tokens.NEQ, yypos, yytext));
<INITIAL>"/" => (makeToken(Tokens.DIVIDE, yypos, yytext));
<INITIAL>"*" => (makeToken(Tokens.TIMES, yypos, yytext));
<INITIAL>"-" => (makeToken(Tokens.MINUS, yypos, yytext));
<INITIAL>"+" => (makeToken(Tokens.PLUS, yypos, yytext));
<INITIAL>{ident} => (makeIdToken(Tokens.ID, yytext, yypos));
<INITIAL>{digit}+ => (makeNumToken(Tokens.INT, yytext, yypos));
<INITIAL>\" => 
  (curstr := ""; strpos:=yypos; instring := true; YYBEGIN STRING; lex());
<INITIAL,COMMENT>"/*" => 
  (push commentLevel commentPos yypos; YYBEGIN COMMENT; lex());
<COMMENT>"*/" => 
  (checkpop commentLevel 
            commentPos 
            yypos 
            "illegal comment terminator ";
   if !commentLevel = 0 then YYBEGIN INITIAL else YYBEGIN COMMENT;
   lex());
<COMMENT>. => (lex());

<STRING>\\ => (YYBEGIN ESCAPE; lex());
<STRING>\" =>  
  (instring := false;
   YYBEGIN INITIAL;
   makeIdToken(Tokens.STRING, !curstr, !strpos));
<STRING>. => (curstr := !curstr ^ yytext; lex());

<ESCAPE>n => (curstr := !curstr ^ "\n"; YYBEGIN STRING; lex());
<ESCAPE>t => (curstr := !curstr ^ "\t"; YYBEGIN STRING; lex());
<ESCAPE>\^[@-_] => 
  (curstr := !curstr ^ str(chr(ord(String.sub(yytext,1)) - ord(#"@"))); 
   YYBEGIN STRING; lex());
<ESCAPE>\\ => (curstr := !curstr ^ "\\"; YYBEGIN STRING; lex());
<ESCAPE>\n => (YYBEGIN LINECONT; lex());
<ESCAPE>{ws}+ => (YYBEGIN LINECONT; lex());
<ESCAPE>{digit}{3} => 
  (curstr := !curstr ^ str(chr(valOf (Int.fromString yytext))); 
   YYBEGIN STRING; 
   lex());
<ESCAPE>\" => (curstr := !curstr ^ "\""; YYBEGIN STRING; lex());
<ESCAPE>. => (ErrorMsg.error yypos 
                             ("illegal escape sequence " ^ yytext);
              YYBEGIN STRING;
              lex());

<LINECONT>\\ => (YYBEGIN STRING; lex());
<LINECONT>{ws}+ => (lex());
<LINECONT>. => (ErrorMsg.error yypos
                               ("illegal character in line continuation "
                                ^ yytext);
                YYBEGIN STRING;
                lex());

.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); lex());

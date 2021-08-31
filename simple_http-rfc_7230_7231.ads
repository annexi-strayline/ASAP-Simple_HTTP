------------------------------------------------------------------------------
--                                                                          --
--                              Simple HTTP                                 --
--                                                                          --
--                Basic HTTP 1.1 support for API endpoints                  --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2021, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai (ANNEXI-STRAYLINE)                                        --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--                                                                          --
--      * Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--                                                                          --
--      * Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--                                                                          --
--      * Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     --
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A --
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT      --
--  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT        --
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,   --
--  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY   --
--  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT     --
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE   --
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.    --
--                                                                          --
------------------------------------------------------------------------------

-- This package contains items defined or under the perview of IETF RFCs
-- 7230 & 7231

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;

package Simple_HTTP.RFC_7230_7231 is
   
   package Latin_1 renames Ada.Characters.Latin_1;
   
   -- RFC 7230 --
   
   CR  : Character renames Latin_1.CR;
   LF  : Character renames Latin_1.LF;
   HT  : Character renames Latin_1.HT;
   SP  : Character renames Latin_1.Space;
   CRLF: constant String := Latin_1.CR & Latin_1.LF;
   
   function Is_Whitespace (C: Character) return Boolean is (C in HT | SP);
   
   function Is_VCHAR (C: Character) return Boolean is
     (Character'Pos (C) in 16#21# .. 16#7E#);

   -- RFC 7230 defines VCHAR via RFC 5234, Appendix B, which defines it:
   -- VCHAR = %x21-7E
   
   function Is_ALPHA (C: Character) return Boolean
     renames Ada.Characters.Handling.Is_Letter;
   
   function Is_DIGIT (C: Character) return Boolean
     renames Ada.Characters.Handling.Is_Digit;
   
   function Is_Token (C: Character) return Boolean is
     (Is_ALPHA (C) or else Is_DIGIT (C)
        or else (C in '!' | '#' | '$' | '%' | '&' | ''' | '*' |
                   '+' | '-' | '.' | '^' | '_' | '`' | '|' | '~'));
   
   function Is_obs_text (C: Character) return Boolean is
     (Character'Pos (C) in 16#80# .. 16#FF#);
   
   type Request_Method is 
     (GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE);
   
   Request_Method_Max_Length: constant := 7;
   -- "CONNECT"
   
   -- We do not support extension-methods
   
   type Response_Status is range 100 .. 599;
   
   subtype Standard_Response_Status is Response_Status with
     Static_Predicate => Standard_Response_Status in 
                         100 .. 101 | 200 .. 206 | 300 .. 307 |
                         400 .. 417 | 426 |
                         500 .. 505;
   
   -- RFC 7231 --
   
   -- Status phrases
   
   Phrase_100: constant String := "Continue";
   Phrase_101: constant String := "Switching Protocols";
   
   Phrase_200: constant String := "OK";
   Phrase_201: constant String := "Created";
   Phrase_202: constant String := "Accepted";
   Phrase_203: constant String := "Non-Authoritative Information";
   Phrase_204: constant String := "No Content";
   Phrase_205: constant String := "Reset Content";
   Phrase_206: constant String := "Partial Content";
   
   Phrase_300: constant String := "Multiple Choices";
   Phrase_301: constant String := "Moved Permanently";
   Phrase_302: constant String := "Found";
   Phrase_303: constant String := "See Other";
   Phrase_304: constant String := "Not Modified";
   Phrase_305: constant String := "Use Proxy";
   Phrase_307: constant String := "Temporary Redirect";
   
   Phrase_400: constant String := "Bad Request";
   Phrase_401: constant String := "Unauthorized";
   Phrase_402: constant String := "Payment Required";
   Phrase_403: constant String := "Forbidden";
   Phrase_404: constant String := "Not Found";
   Phrase_405: constant String := "Method Not Allowed";
   Phrase_406: constant String := "Not Acceptable";
   Phrase_407: constant String := "Proxy Authentication Required";
   Phrase_408: constant String := "Request Time-out";
   Phrase_409: constant String := "Conflict";
   Phrase_410: constant String := "Gone";
   Phrase_411: constant String := "Length Required";
   Phrase_412: constant String := "Precondition Failed";
   Phrase_413: constant String := "Request Entity Too Large";
   Phrase_414: constant String := "Request-URI Too Large";
   Phrase_415: constant String := "Unsupported Media Type";
   Phrase_416: constant String := "Requested range not satisfiable";
   Phrase_417: constant String := "Expectation Failed";
   Phrase_426: constant String := "Upgrade Required";
   
   Phrase_500: constant String := "Internal Server Error";
   Phrase_501: constant String := "Not Implemented";
   Phrase_502: constant String := "Bad Gateway";
   Phrase_503: constant String := "Service Unavailable";
   Phrase_504: constant String := "Gateway Time-out";
   Phrase_505: constant String := "HTTP Version not supported";
   
end Simple_HTTP.RFC_7230_7231;

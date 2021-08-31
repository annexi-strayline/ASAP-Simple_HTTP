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

-- This package handles parsing of HTTP-schemed URIs (RFC 3986)

with Ada.Strings.Bounded;
with Ada.Characters.Handling;

package Simple_HTTP.RFC_3986 is
   
   -- RFC 3986 1.6
   
   function Is_alpha (Item: in Character) return Boolean
     renames Ada.Characters.Handling.Is_Letter;
   
   -- "ALPHA"
   
   function Is_digit (Item: in Character) return Boolean
     renames Ada.Characters.Handling.Is_Digit;
   
   -- "DIGIT"
   
   function Is_alphanum (Item: in Character) return Boolean
     renames Ada.Characters.Handling.Is_Alphanumeric;
        
   -- "ALPHA" | "DIGIT"
   
   function Is_gen_delim (Item: in Character) return Boolean is
     (Item in ':' | '/' | '?' | '#' | '[' | ']' | '@');
   
   -- gen-delims
   
   function Is_sub_delim (Item: in Character) return Boolean is
     (Item in '!' | '$' | '&' | ''' | '(' | ')' | '*' | '+' | ',' | ';' | '=');
   
   -- sub-delims
   
   function Is_reserved (Item: in Character) return Boolean is
     (Is_gen_delim (Item) or else Is_sub_delim (Item));
   
   function Is_unreserved (Item: in Character) return Boolean is
     (Is_alphanum (Item) or else (Item in '-' | '.' | '_' | '~'));
   
   Escape_Preamble: constant Character := '%';
   
   ----------------
   -- URI_Parser --
   ----------------
   
   -- The URI_Parser package provides facilities designed for handling URI
   -- inputs of an external origin, and therefore provide safe checking of
   -- URI syntax, avoiding the raising of exceptions on invalid input.
   
   generic
      with package URI_Strings is
        new Ada.Strings.Bounded.Generic_Bounded_Length (<>);
      
   package URI_Parser is
      
      subtype URI_String is URI_Strings.Bounded_String;
      type Port_Number is range 0 .. 2**16 - 1;
      
      procedure Unescape (Sequence: in out URI_String;
                          Valid   :    out Boolean);
   
      -- Converts any escape codes in Sequence to their encoded Character, in-
      -- line. If the process is successful, the unescaped sequence is assigned
      -- to Sequence, and Valid is set to True. If there are any mal-formed
      -- escape sequences (such as "%%" or invalid hex characters), Sequence is
      -- not modified, and Valid is set to False
      
      procedure Scheme (URI   : in     URI_String;
                        Scheme:    out URI_String;
                        Valid :    out Boolean);
      
      -- Attempts to retrieve the sceme, and also normalizes it to lower-case.
      -- The scheme will be everything up to the first ':'.
      --
      -- If no scheme is found, Scheme is set to Null_Bounded_String, and
      -- Valid is set to True.
      --
      -- If the scheme component violates RFC 3986, Valid is set to False, and
      -- Scheme is set to a Null_Bounded_String.
      
      procedure Parse_Authority
        (URI         : in     URI_String;
         Default_Port: in     Port_Number := 80;
         Valid       :    out Boolean;
         userinfo    :    out URI_String;
         host        :    out URI_String;
         port        :    out Port_Number);
      
      -- Dissects the authority portion of a URI (if present), setting the
      -- output parameters accordingly.
      --
      -- The authority component is between the scheme and path. In practical
      -- terms, it typically looks like this: "scheme://authority/path"
      --
      -- Sequence must be the full URI. Dissected_Authority invokes Authority
      -- on Sequence.
      --
      -- If there is no authority component, userinfo and host will be set to
      -- Null_Bounded_String. If userinfo and host do not exist in the
      -- authority, the respective output parameter is set to
      -- Null_Bounded_String.
      --
      -- If the URI is not valid (according to RFC 3986), Valid is set to
      -- False, and userinfo, host, and port are set to Null_Bounded_String.
      -- Parse_Authority does _full_ checking against RFC 3986.
      --
      -- If there is no port component (including the case of "abc@xyz.net:"),
      -- port is set to the value given for Default_Port. The default value of
      -- Default_Port (80) is as specified by RFC 2616 (HTTP 1.1).
      
      function Path (Sequence: URI_String) return URI_String;
      
      -- Returns the entire path-abempty part of a URI (RFC 3986-3.3).
      -- This including the leading '/' (if any), but not including the first
      -- '?' of the query part, or '#' of the first fragment, if any. 
      --
      -- The returned path is not normalized.
      --
      -- if the path violates RFC 3986 (Section 3), Constraint_Error is
      -- raised.
      
      procedure First_Query (URI             : in     URI_String;
                             Query           :    out URI_String;
                             Next_Query_Start:    out Natural;
                             Valid           :    out Boolean);
                            
      -- Sets Query to the first query value, if any. This value is the text
      -- that follows the first '?', and extends until '&', '#', or the end of
      -- Sequence (exclusive).
      --
      -- Note that RFC 3986 does not actually provision the '&' separation,
      -- but this is the common convention on the web, and particularily for
      -- APIs.
      --
      -- If there is something incorrect about the query structure (such as a
      -- pattern like "&&", or query text that does not meet the requirements
      -- of RFC 3986, Query will be set to Null_Bounded_String,
      -- Next_Query_Start will be set to zero, and Value will be set to False.
      --
      -- If there is an '&' ending the query, Next_Query_Start is the index of
      -- Sequence that yields the first character immediately following the
      -- '&', otherwise Next_Query_Start is set to zero.
      --
      -- If there is no query (no '?'), a Null_Bounded_String is returned,
      -- Next_Query_Start is set to zero, and Valid is set to True.
      --
      -- Note that any empty query (such as the sequence "?&" or "&&")
      -- is considered to be invalid.
      
      procedure Next_Query (URI             : in     URI_String;
                            Query           :    out URI_String;
                            Next_Query_Start: in out Natural;
                            Valid           :    out Boolean);
      
      -- Sets Query to the next query that is expected to start at
      -- Next_Query_Start as set by a previous call to First_Query or
      -- Next_Query.
      --
      -- If Next_Query_Start = 0, or is not in the rage of URI, Valid is set to
      -- False.
      --
      -- Otherwise, the mechanics follow those of First_Query.

      
   end URI_Parser;
   
end Simple_HTTP.RFC_3986;

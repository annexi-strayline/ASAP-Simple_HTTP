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

with Ada.Strings.Maps;
with Ada.Characters.Handling;

with Hex.Unsigned_8;

separate (Simple_HTTP.RFC_3986)

package body URI_Parser is
   
   use URI_Strings;
   
   ------------------
   -- Valid_Escape --
   ------------------
   
   -- Given the position of a '%' character, determine if two hex digits follow
   
   function Valid_Escape (Sequence: URI_String; Starting: Positive)
                         return Boolean
   is begin
      if Starting + 2 > Length (Sequence) then
         return False;
         
      else
         return Hex.Valid_Hex_String 
           (Slice (Source => Sequence, 
                   Low    => Starting + 1,
                   High   => Starting + 2));
         
      end if;
   end;
   
   --------------
   -- Unescape --
   --------------
   
   procedure Unescape (Sequence: in out URI_String;
                       Valid   :    out Boolean)
   is
      use Hex, Hex.Unsigned_8;
      
      Escaped: URI_String;
      I: Positive := 1;
      E: Character;
      Sequence_Length: constant Natural := Length (Sequence);
      
      procedure Unescape_At (Index: Positive) 
      with Inline is
      begin
         if Index + 2 > Sequence_Length then
            raise Constraint_Error with
              "Invalid URI escape: escape sequence too short.";
         end if;
         
         declare
            Code: constant String := Slice 
              (Source => Sequence, Low => Index + 1, High => Index + 2);
         begin
            if not Valid_Hex_String (Code) then
               Valid := False;
               return;
            end if;
            
            Append (Source   => Escaped,
                    New_Item => Character'Val (Decode (Code)));
         end;
         
      end Unescape_At;
   begin
      Valid := True;
      
      -- Short circuit scan
      if Index (Source => Sequence, Pattern => "%", From => 1) = 0 then
         -- No escapes here
         return;
      end if;
      
      while I <= Sequence_Length loop
         E := Element (Source => Sequence, Index => I);
         
         if E = '%' then
            Unescape_At (I);
            
            if not Valid then
               return;
            else
               I := I + 2;
            end if;
            
         else
            Append (Source => Escaped, New_Item => E);
            
         end if;
         
         I := I + 1;
      end loop;
      
      Sequence := Escaped;
      
   end Unescape;
   
   ------------
   -- Scheme --
   ------------
   
   procedure Scheme (URI   : in     URI_String;
                     Scheme:    out URI_String;
                     Valid :    out Boolean)
   is
      Scheme_Start: constant Natural := Index (Source  => URI,
                                               Pattern => ":",
                                               From    => 1);
      E: Character;
      Scheme_String: URI_String;
   begin
      Scheme := Null_Bounded_String;
      
      if Scheme_Start = 0 then -- short circuit
         Valid  := True;
         return;
      end if;
      
      for I in 1 .. Scheme_Start - 1 loop
         E := Element (Source => URI, Index => I);
         
         if Is_unreserved (E) then
            E := Ada.Characters.Handling.To_Lower (E);
            Append (Source => Scheme, New_Item => E);
         else
            Valid := False;
            return;
         end if;
      end loop;
      
      Valid := True;
   end Scheme;
   
   ---------------------
   -- Parse_Authority --
   ---------------------
   
   procedure Parse_Authority
     (URI         : in     URI_String;
      Default_Port: in     Port_Number := 80;
      Valid       :    out Boolean;
      userinfo    :    out URI_String;
      host        :    out URI_String;
      port        :    out Port_Number)
   is separate;
   
   ----------
   -- Path --
   ----------
   
   function Path (Sequence: URI_String) return URI_String is
      First, Last: Natural;
      
      Sequence_Length: constant Natural := Length (Sequence);
      
      Path_End_Set: constant Ada.Strings.Maps.Character_Set
        := Ada.Strings.Maps.To_Set ("?#");
   begin
      -- If there is a "//" then we skip it. Otherwise, we seek to
      -- the first '/' and until we reach the end or '?'
      
      First := Index (Source  => Sequence, Pattern => "//", From => 1);
      
      if First = 0 then
         First := 1;
      else
         First := First + 2;
         if First > Sequence_Length then
            return Null_Bounded_String;
         end if;
      end if;
      
      First := Index (Source => Sequence, Pattern => "/", From => First);
      
      if First = 0 then
         return Null_Bounded_String;
      end if;
      
      Last := Index (Source => Sequence,
                     Set    => Path_End_Set,
                     From   => First + 1);
      
      if Last = 0 then
         Last := Length (Sequence);
      else
         Last := Last - 1;
      end if;
      
      return Bounded_Slice (Source => Sequence, Low => First, High => Last);
      
   end Path;
   
   -----------------
   -- First_Query --
   -----------------
   
   function Valid_Query (Candidate  : URI_String;
                         First, Last: Positive)
                        return Boolean
   is
      E: Character;
   begin
      for I in First .. Last loop
         E := Element (Source => Candidate, Index => I);
         
         if E = Escape_Preamble
           and then not Valid_Escape (Sequence => Candidate,
                                      Starting => I)
         then
            return False;
            
         elsif not Is_unreserved (E)
           or else not Is_sub_Delim (E)
           or else E not in ':' | '@' | '/' | '?'
         then
            return False;
            
         end if;
      end loop;
      
      return True;
   end Valid_Query;
                        
   ----------------------------------------------------------------------
   
   procedure Set_Query_End (Sequence        : in     URI_String;
                            Sequence_Length : in     Natural;
                            First           : in     Natural;
                            Last            :    out Natural;
                            Next_Query_Start:    out Natural;
                            Valid           :    out Boolean)
   is
      -- Used by both First_Query and Next_Query to set the Last marker,
      -- as well as Next_Query_String.
      --
      -- For an empty query "?&" / "&&", Last is set to zero, indicating
      -- a Null_Bounded_String should be returned (by the caller). 
      
      Query_End_Set: constant Ada.Strings.Maps.Character_Set
        := Ada.Strings.Maps.To_Set ("&#");
      
   begin
      Last := Index (Source => Sequence, Set => Query_End_Set, From => First);
      
      if Last = 0 then
         Last := Sequence_Length;
         Next_Query_Start := 0;
         Valid := True;
         
      elsif Last = First then
         -- This means we found a query that starts with '&' or '#', since
         -- First is supposed to be the first character of the actual
         -- query. Empty "queries" like this are considered to be invalid.
         Valid := False;
         
      else
         
         if Last = Sequence_Length
           or else Element (Source => Sequence, Index => Last) = '#'
         then
            -- RFC 3986 indicates that '#' terminates the entire query.
            -- And, obviously, the end of the URI itself means the same
            Next_Query_Start := 0;
         else
            Next_Query_Start := Last + 1;
         end if;
         
         Last := Last - 1;
         Valid := True;

      end if;
   end Set_Query_End;
   
   ----------------------------------------------------------------------
   
   procedure First_Query (URI             : in     URI_String;
                          Query           :    out URI_String;
                          Next_Query_Start:    out Natural;
                          Valid           :    out Boolean)
   is
      First, Last: Natural;
      URI_Length: constant Natural := Length (URI);
      
      procedure Set_Invalid is
      begin
         Query := Null_Bounded_String;
         Next_Query_Start := 0;
         Valid := False;
      end;
      
   begin
      First := Index (Source => URI, Pattern => "?", From => 1);
      Query := Null_Bounded_String;
      
      if First = 0 then
         Next_Query_Start := 0;
         Valid := True;
         return;
      else
         First := First + 1;
         if First > URI_Length then
            -- Invalid URI - ends with '?'
            Set_Invalid;
            return;
         end if;
      end if;
      
      Set_Query_End (Sequence         => URI, 
                     Sequence_Length  => URI_Length,
                     First            => First,
                     Last             => Last,
                     Next_Query_Start => Next_Query_Start,
                     Valid            => Valid);

      if not Valid then
         Set_Invalid;
         return;
         
      else
         Bounded_Slice (Source => URI,
                        Target => Query,
                        Low    => First,
                        High   => Last);
         
         Valid := True;
      end if;
      
   end First_Query;
   
   ----------------
   -- Next_Query --
   ----------------
   
   procedure Next_Query (URI             : in     URI_String;
                         Query           :    out URI_String;
                         Next_Query_Start: in out Natural;
                         Valid           :    out Boolean)
   is
      First, Last: Natural;
      URI_Length: constant Natural := Length (URI);
      
      procedure Set_Invalid is
      begin
         Query := Null_Bounded_String;
         Next_Query_Start := 0;
         Valid := False;
      end;
      
   begin
      if Next_Query_Start = 0 
        or else Next_Query_Start > URI_Length
      then
         Set_Invalid;
         return;
      end if;
      
      First := Next_Query_Start;
      
      Set_Query_End (Sequence         => URI, 
                     Sequence_Length  => URI_Length,
                     First            => First,
                     Last             => Last,
                     Next_Query_Start => Next_Query_Start,
                     Valid            => Valid);
      
      if not Valid then
         Set_Invalid;
         return;
         
      else
         Bounded_Slice (Source => URI,
                        Target => Query,
                        Low    => First,
                        High   => Last);
         
         Valid := True;
      end if;
      
   end Next_Query;
   
end URI_Parser;

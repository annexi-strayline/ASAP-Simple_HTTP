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



separate (Simple_HTTP.RFC_3986.URI_Parser)

procedure Parse_Authority
  (URI         : in     URI_String;
   Default_Port: in     Port_Number := 80;
   Valid       :    out Boolean;
   userinfo    :    out URI_String;
   host        :    out URI_String;
   port        :    out Port_Number)
is
   Sequence_Length: constant Natural := Length (URI);

   
   function URI_Element (Source: URI_String := URI;
                         Index : Positive)
                        return Character
     renames URI_Strings.Element;
   
   procedure Convert_Port (Low, High: in     Positive;
                           Result   :    out Port_Number;
                           OK       :    out Boolean);
   
   procedure Probe_End (From  : in     Positive;
                        Result:    out Positive;
                        OK    :    out Boolean);
   
   -- Attempts to find the end of the authority component, starting at From.
   -- If an invalid character appears, OK is set to False, otherwise, Result
   -- is set to the character representing the last character of the authority
   -- component
   
   function Valid_Userinfo (Low, High: Positive) return Boolean;
   function Valid_Host (Low, High: Positive) return Boolean;
   
   procedure Set_Empty;
   procedure Set_Invalid;
   
   ----------------------------------------------------------------------
   
   
   procedure Convert_Port (Low, High: in     Positive;
                           Result   :    out Port_Number;
                           OK       :    out Boolean)
   is
      Port_String: constant String := Slice (Source => URI,
                                             Low    => Low,
                                             High   => High);
   begin
      if (for some C of Port_String => not Is_digit (C)) then
         -- Not a number!
         OK := False;
         return;
      end if;
      
      Result := Port_Number'Value (Port_String);
      OK := True;
      
   exception
      when others =>
         OK := False;
         return;
   end;
   
   ----------------------------------------------------------------------
   
   procedure Probe_End (From  : in     Positive;
                        Result:    out Positive;
                        OK    :    out Boolean)
   is
      Authority_Termination_Set: constant Ada.Strings.Maps.Character_Set
        := Ada.Strings.Maps.To_Set ("/?#");
      
      Mark: constant Natural := Index (Source => URI,
                                       Set    => Authority_Termination_Set,
                                       From   => From);
                                       
   begin
      if Mark /= 0 then
         if Mark = From then
            Result := Mark;
         else
            Result := Mark - 1;
         end if;
         OK := URI_Element (Index => Mark) = '/';
      else
         Result := Sequence_Length;
         OK := True;
      end if;
      
   end Probe_End;
   
   ----------------------------------------------------------------------
   
   function Valid_Userinfo (Low, High: Positive) return Boolean is
      function Valid_Char (Item: in Character) return Boolean is
        (Is_unreserved (Item)
           or else Is_sub_delim (Item)
           or else (Item in Escape_Preamble | ':'));
      
   begin
      for I in Low .. High loop
         if not Valid_Char (URI_Element (Index => I)) then
            return False;
         end if;
      end loop;
      
      return True;
   end Valid_Userinfo;
   
   ----------------------------------------------------------------------
   
   function Valid_Host (Low, High: Positive) return Boolean is
      -- This is quite a bit more involved than userinfo..
      
      function Valid_IP_Literal   return Boolean;
      function Valid_IPv4_Address return Boolean;
      function Valid_Reg_Name     return Boolean;
      
      ------------------------------------------------------------
      
      function Valid_IP_Literal return Boolean is
         -- We're not going to recognize "IPvFuture". If that is ever one
         -- day a thing, we can add that here. So far we're expecting only
         -- an IPv6 address in square-brackets
         
      begin
         -- The shortest possible IPv6 addres is "::", so we need at least
         -- 4 characters for this to have any chance of working, since it
         -- must also be enclosed in "[]"
         --
         -- Conversely, the longest address is
         -- "0000:0000:0000:0000:0000:0000:0000:0000" (39) + "[]" (2) = 41
         --
         -- We cannot see more than one occurance of consecutive '::'
         
         if High - Low not in 3 .. 40
           or else URI_Element (Index => Low) /= '['
           or else URI_Element (Index => High) /= ']'
         then
            return False;
         end if;
         
         -- This is a lazy check, we're really trying to exclude things that
         -- are obviously not IPv6. The rest can be left up to the stack, as
         -- we can be sure we're not going to have something insane
         
         declare
            E: Character;
            Colon_Count       : Natural := 0;
            Digit_Count       : Natural := 0;
            Consecutive_Colons: Boolean := False;
         begin
            for I in Low + 1 .. High - 1 loop
               E := URI_Element (Index => I);
               
               if E = ':' then
                  Digit_Count := 0;
                  Colon_Count := Colon_Count + 1;
                  
                  if Colon_Count >= 7 then
                     -- Too many colons to be valid
                     return False;
                     
                  elsif URI_Element (Index => Low - 1) = ':' then
                     if Consecutive_Colons then
                        -- There shall only be one case of "::" per IPv6
                        -- address
                        return False;
                        
                     else
                        Consecutive_Colons := True;
                     end if;
                  end if;
                  
               elsif E in Hex.Hex_Character then
                  if Digit_Count = 4 then
                     -- Too many hex digits
                     return False;
                  else
                     Digit_Count := Digit_Count + 1;
                  end if;
                  
               else
                  return False;
               end if;
               
            end loop;
            
            -- There should be at least two ':', but no more than 7.
            if Colon_Count not in 2 .. 7 then
               return False;
            end if;
         end;
         
         -- Everything checks-out
         return True;
         
      end Valid_IP_Literal;
      
      ------------------------------------------------------------
      
      function Valid_IPv4_Address return Boolean is
         -- Significantly simpler than IPv6. We expect four numbers
         -- that are not more than 3 characters long, separated by
         -- '.', and nothing else.
         --
         -- We won't be pedantic about the values - like "999.999.999.999"
         
         E: Character;
         Period_Count: Natural := 0;
         Octet_Digits: String (1 .. 3) := "   ";
         Octet_Place : Natural := 0;
      begin
         -- Longest  is 255.255.255.255 = 15,
         -- Shortest is 0.0.0.0         = 7
         
         if High - Low not in 6 .. 14 then
            return False;
         end if;
         
         for I in Low .. High loop
            E := URI_Element (Index => I);
            
            if E = '.' then
               if Period_Count >= 3 then
                  -- There has already been three '.', this is too much
                  return False;
                  
               elsif Octet_Place < 1 then
                  -- This would mean there was no number preceeding this '.'.
                  -- That is not ok.
                  return False;
                  
               elsif Natural'Value (Octet_Digits(1 .. Octet_Place))
                 not in 0 .. 255
               then
                  -- The numeric value of the preceeding octet is not valid.
                  return False;
                  
               else
                  Period_Count := Period_Count + 1;
                  Octet_Place  := 1;
                  Octet_Digits := "   ";
               end if;
               
            elsif Is_digit (E) then
               if Octet_Place >= 3 then
                  -- Too many digits!
                  return False;
                  
               else
                  Octet_Place := Octet_Place + 1;
                  Octet_Digits(Octet_Place) := E;
               end if;
               
            end if;
         end loop;
         
         return True;
         
      end Valid_IPv4_Address;
      
      ------------------------------------------------------------
      
      function Valid_Reg_Name return Boolean is
         E: Character;
      begin
         if High - Low < 1 then
            return False;
         end if;
         
         for I in Low .. High loop
            E := URI_Element (Index => I);
            
            if not 
              (Is_unreserved (E)
                 or else E = '%'
                 or else Is_sub_delim (E))
            then
               return False;
            end if;
         end loop;
         
         return True;
         
      end Valid_Reg_Name;
      
      ------------------------------------------------------------
      
   begin
      
      return Valid_IP_Literal
        or else Valid_IPv4_Address
        or else Valid_Reg_Name;
      
   end Valid_Host;
   
   ----------------------------------------------------------------------
   
   procedure Set_Empty is
   begin
      userinfo := Null_Bounded_String;
      host     := Null_Bounded_String;
      port     := Default_Port;
   end;
   
   ----------------------------------------------------------------------
   
   procedure Set_Invalid is
   begin
      Set_Empty;
      Valid := False;
   end;
   
   ----------------------------------------------------------------------
   
   User_Start, User_End, Host_Start, Host_End, Port_Start, Port_End: Natural;
   
begin
   if Sequence_Length = 0 then
      Set_Empty;
      Valid := True;
      return;
      
   end if;
   
   -- We expect to find "//" that must preceed the heir part, which exists
   -- if the authority component exists. If we don't find "//" then we
   -- can assume there is no authority at all
   
   User_Start := Index (Source  => URI,
                        Pattern => "//",
                        From    => 1);
   
   if User_Start = 0 then
      Set_Empty;
      Valid := True;
      return;
   else
      User_Start := User_Start + 2;
   end if;
   
   
   if User_Start > Sequence_Length then
      Set_Invalid;
      return;
   end if;
   
   -- userinfo part. --
   
   User_End := Index (Source  => URI,
                      Pattern => "@",
                      From    => User_Start);
   
   if User_End = 0 then
      userinfo := Null_Bounded_String;
      Host_Start := User_Start;
      
   elsif User_End = 1 then
      -- invalid
      Set_Invalid;
      return;
      
   else
      Host_Start := User_End + 1;
      User_End := User_End - 1;
      
      if not Valid_Userinfo (Low => 1, High => User_End) then
         Set_Invalid;
         return;
      end if;
      
      userinfo := Bounded_Slice (Source => URI,
                                 Low    => 1,
                                 High   => User_End);
      
      if Host_Start > Sequence_Length then
         -- We can't have just a user part!
         Set_Invalid;
         return;
      end if;
   end if;
   
   -- host part --
   
   Host_End := Index (Source  => URI,
                      Pattern => ":",
                      From    => Host_Start);
   
   if Host_End = 0 then
      -- This just means no port part. But we still need to find the start of
      -- the path, if any. Note that a path component *must* follow the
      -- authority. That means if we get this far, we expect to find '/', or
      -- the end. If we find any intervening '?' or '#', it is invalid
      
      Port_Start := 0;
      
      declare
         OK: Boolean;
      begin
         Probe_End (From   => Host_Start,
                    Result => Host_End,
                    OK     => OK);
         
         if not OK then
            Set_Invalid;
            return;
         end if;
      end;
      
   elsif Host_End = Sequence_Length then
      -- No port component, which is OK
      Port_Start := 0;
      Host_End := Host_End - 1;
      
   elsif Host_End = Host_Start then
      -- No host at all. Not ok.
      Set_Invalid;
      return;
      
   else
      -- We have a ':' that has something (should be a port number) following
      -- it
      Port_Start := Host_End + 1;
      Host_End   := Host_End - 1;
      
   end if;
   
   
   pragma Assert (Host_End > Host_Start);
   
   if not Valid_Host (Low => Host_Start, High => Host_End) then
      Set_Invalid;
      return;
      
   else
      host := Bounded_Slice (Source => URI,
                             Low    => Host_Start,
                             High   => Host_End);
   end if;
   
   -- port component (if any) --
   
   if Port_Start > 0 then
      
      declare
         OK: Boolean;
      begin
         Probe_End (From   => Port_Start,
                    Result => Port_End,
                    OK     => OK);
         
         if not OK then
            Set_Invalid;
            return;
         end if;
      end;
      
      if Port_End = Port_Start then
         port := Default_Port;
         
      else
         Port_End := Port_End - 1;
         
         declare
            Port_OK: Boolean;
         begin
            Convert_Port (Low    => Port_Start,
                          High   => Port_End,
                          Result => port,
                          OK     => Port_OK);
            
            if not Port_OK then
               Set_Invalid;
               return;
            end if;
         end;
         
      end if;
   end if;
   
   -- If we got this far, everything looks good
   Valid := True;
   
end Parse_Authority;

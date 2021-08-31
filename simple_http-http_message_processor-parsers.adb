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

with Ada.TexT_IO;

with Simple_HTTP.RFC_3986;
with Simple_HTTP.RFC_7230_7231;

separate (Simple_HTTP.HTTP_Message_Processor)

package body Parsers is
   
   package TIO renames Ada.Text_IO;
   
   use type Ada.Calendar.Time;
   
   CR  : Character renames RFC_7230_7231.CR;
   LF  : Character renames RFC_7230_7231.LF;
   CRLF: String    renames RFC_7230_7231.CRLF;
   
   --
   -- Utilities
   --
   
   -------------------------------
   -- Fetch_Skipping_Whitespace --
   -------------------------------
   
   -- Only reads up to Limit characters.
   -- Catches illegal CR/LF combinations -- All CRs must be followed by LF.
   
   -- Exits if CRLF is encountered, setting Got_CRLF to True.
   
   -- Status will be set to OK if no issue was encountered, or to
   -- Timeout or Bad_Input
   
   procedure Fetch_Skipping_Whitespace
     (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
      Deadline: in       Ada.Calendar.Time;
      C       :     out  Character;
      Got_CRLF:     out  Boolean;
      Status  :     out  HTTP_Message_Status)
      
   with Post => (if Got_CRLF then C = LF else 
                   (Status = OK) = (not RFC_7230_7231.Is_Whitespace (C)))
   is
      use RFC_7230_7231;
      
      use type Ada.Calendar.Time;
      
      Last_Is_CR: Boolean := False;
      LWS_Count: Natural := 0;
   begin
      Status   := OK;
      Got_CRLF := False;
      
      loop
         if Ada.Calendar.Clock > Deadline then
            Status := Timeout;
            return;
         end if;
         
         Character'Read (Stream, C);
         
         if (Last_Is_CR and then C /= LF) or else C = LF then
            -- CR should only appear immediately preceeding LF.
            Status := Bad_Input;
            return;
            
         elsif Last_Is_CR then
            pragma Assert (C = LF); -- Implied by the first if statement
            Got_CRLF := True;
            return;
         end if;
         
         exit when not Is_Whitespace (C);
         
         Last_Is_CR := (C = CR);
         LWS_Count := LWS_Count + 1;
         
         if LWS_Count > Whitespace_Limit then
            Status := Bad_Input;
            return;
         end if;

      end loop;
   end Fetch_Skipping_Whitespace;
   
   -----------------------
   -- Read_HTTP_Version --
   -----------------------
   
   -- Parses an expected HTTP_Version
   
   procedure Read_HTTP_Version
     (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
      Status  :    out HTTP_Message_Status;
      Deadline: in     Ada.Calendar.Time)
   with Post => Status in OK | Bad_Input | Bad_Version is
      use RFC_7230_7231;
      use type Ada.Calendar.Time;
      
      Expected_Version: constant String (1 .. 8) := "HTTP/1.1";
      Version_Buffer  : String (Expected_Version'Range);
      Version_OK      : Boolean := False;
   begin
      Status := Bad_Input;
      
      -- First is the HTTP-Version part. We will accept any version, but
      -- if it is not 1.1, we will report a non-OK status
      
      for C of Version_Buffer loop
         Character'Read (Stream, C);
         
         if Ada.Calendar.Clock > Deadline then
            Status := Timeout;
            return;
         end if;
      end loop;
      
      if Version_Buffer /= Expected_Version then
         if Version_Buffer (1 .. 5) /= Expected_Version (1 .. 5)
           or else not Is_DIGIT (Version_Buffer (6))
           or else not Is_DIGIT (Version_Buffer (8))
           or else Version_Buffer (7) /= '.'
         then
            -- The format itself is wrong
            Status := Bad_Input;
            return;
         else
            -- This version is not recognized. Note and continue.
            Status := Bad_Version;
         end if;
      else
         Status := OK;
      end if;
      
   end Read_HTTP_Version;
   
   -------------------------
   -- Read_Request_Method --
   -------------------------
   
   procedure Read_Request_Method
     (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
      Method  :     out  RFC_7230_7231.Request_Method;
      Status  :     out  HTTP_Message_Status;
      Deadline: in       Ada.Calendar.Time)
   is
      use RFC_7230_7231;
      
      Method_Buffer: String (1 .. Request_Method_Max_Length + 1);
      Method_Mark  : Positive := Method_Buffer'First;
   begin
      Method := OPTIONS;
      -- A generic method default to avoid warnings for not setting
      -- Method
      
      loop
         if Ada.Calendar.Clock > Deadline then
            Status := Timeout;
            return;
         end if;
         
         Character'Read (Stream, Method_Buffer(Method_Mark));
         
         exit when Method_Buffer(Method_Mark) = SP;
         -- RFC 7230 says the method ends specifically with a space
         
         if Method_Buffer(Method_Mark) in CR | LF then
            Status := Bad_Input;
            return;
            
         elsif Method_Mark = Method_Buffer'Last then
            Status := Bad_Method;
            return;
         end if;

         Method_Mark := Method_Mark + 1;
      end loop;
      
      pragma Assert (Is_Whitespace (Method_Buffer(Method_Mark)));
      
      if Method_Mark <= Method_Buffer'First then
         Status := Bad_Method;
         return;
      else
         Method_Mark := Method_Mark - 1;
      end if;
      
      begin
         Method := Request_Method'Value 
           (Method_Buffer (Method_Buffer'First .. Method_Mark));
      exception
         when others =>
            Status := Bad_Method;
            return;
      end;
      
      Status := OK;
      
   end Read_Request_Method;
   
   
   --
   -- Implementation
   --
   
   -----------------------
   -- Read_Request_Line --
   -----------------------
   
   procedure Read_Request_Line
     (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
      Method  :     out  RFC_7230_7231.Request_Method;
      URI     :     out  Path_Strings.Bounded_String;
      Status  :     out  HTTP_Message_Status;
      Deadline: in       Ada.Calendar.Time)
   is
      use Path_Strings;
      
      Buffer: Character;
      
      In_Fragment: Boolean := False;
      Fragment_Ghost: Natural := 0;
   begin
      -- RFC 7230 states:
      -- request-line   = method SP request-target SP HTTP-version CRLF
      --
      -- We will adhere to this strictly.
      
      Read_Request_Method (Stream, Method, Status, Deadline);
      
      if Status /= OK then
         return;
      end if;
      
      URI := Path_Strings.Null_Bounded_String;
      
      -- We've already read-in the SP part that follows method. The next
      -- part is to read-in the URI until we see a space, but we'll also
      -- do a cursary verification the content as we go (not a semantic check).
      --
      -- If we encounter a fragment, we will continue to verify it, but the
      -- fragment will not be added to the URI.
      
      loop
         if Ada.Calendar.Clock > Deadline then
            Status := Timeout;
            return;
         elsif Length (URI) = Path_Strings.Max_Length then
            Status := URI_Too_Long;
            return;
         end if;
         
         Character'Read (Stream, Buffer);
         
         exit when Buffer = RFC_7230_7231.SP;
         
         if (not RFC_3986.Is_unreserved (Buffer))
           and then (not RFC_3986.Is_reserved (Buffer))
         then
            Status := Bad_URI;
            return;
            
         elsif In_Fragment then
            if Fragment_Ghost >= Path_Strings.Max_Length then
               Status := URI_Too_Long;
               return;
            end if;
            
            Fragment_Ghost := Fragment_Ghost + 1;
            
         elsif Buffer = '#' then
            In_Fragment := True;
            Fragment_Ghost := Length (URI) + 1;
            -- We want to apply the same length restriction to all URIs,
            -- even if we discard fragments
            
         else
            Append (Source => URI, New_Item => Buffer);
         end if;
      end loop;
      
      if Length (URI) = 0 then
         Status := Bad_URI;
         return;
      end if;
      
      if Ada.Calendar.Clock > Deadline then
         Status := Timeout;
         return;
      end if;
      
      -- HTTP version. This shall be followed immediately with CRLF
      Read_HTTP_Version (Stream, Status, Deadline);
      
      if Status not in OK | Bad_Version then
         return;
      end if;
      
      if Character'Input (Stream) /= CR then
         Status := Bad_Input;
         return;
      end if;
      
      if Ada.Calendar.Clock > Deadline then
         Status := Timeout;
         return;
      end if;
      
      if Character'Input (Stream) /= LF then
         Status := Bad_Input;
         return;
      end if;
      
      -- If we got this far, it seems to check-out. Status should be OK from
      -- the original call to Read_Request_Method, or Bad_Version if the
      -- http-version line was not indicating 1.1
      
      pragma Assert (Status in OK | Bad_Version);
      
   end Read_Request_Line;
   
   ----------------------
   -- Read_Status_Line --
   ----------------------
   
   procedure Read_Status_Line
     (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
      Code    :    out RFC_7230_7231.Response_Status;
      Phrase  :    out Phrase_Strings.Bounded_String;
      Status  :    out HTTP_Message_Status;
      Deadline: in     Ada.Calendar.Time)
   is
      use RFC_7230_7231;
      
      Status_Code_Buffer: String (1 .. 3);
      Version_OK: Boolean;
   begin
      
      -- Avoid warnings for out parameters by setting some sane "defaults"
      Code := 400; -- "Bad Request";
      Phrase := Phrase_Strings.Null_Bounded_String;
      
      -- http-version + SP
      
      Read_HTTP_Version (Stream, Status, Deadline);
      
      Version_OK := (Status = OK);
      
      if Status not in OK | Bad_Version then
         return;
      elsif Character'Input (Stream) /= RFC_7230_7231.SP then
         Status := Bad_Input;
         return;
      elsif Ada.Calendar.Clock > Deadline then
         Status := Timeout;
         return;
      end if;
      
      -- Next is the status code. RFC 7230 specifies that the status code is
      -- always exactly 3 digits.
      
      for C of Status_Code_Buffer loop
         Character'Read (Stream, C);
         
         if Ada.Calendar.Clock > Deadline then
            Status := Timeout;
            return;
         elsif not Is_DIGIT (C) then
            Status := Bad_Status;
            return;
         end if;
      end loop;
      
      begin
         Code := Response_Status'Value (Status_Code_Buffer);
      exception
         when others =>
            Status := Bad_Status;
            return;
      end;
      
      -- RFC 7230-3.1.2 says that clients "SHOULD" ignore the reason-phrase
      -- content - so we won't actually require anything to be there, but
      -- if there is something, we'll still verify the content
      
      Phrase := Phrase_Strings.Null_Bounded_String;
      
      declare
         C: Character;
         Started: Boolean := False;
      begin
         loop
            Character'Read (Stream, C);
            
            if Ada.Calendar.Clock > Deadline then
               Status := Timeout;
               return;
               
            elsif C = CR then
               -- LF must follow.
               Character'Read (Stream, C);
               
               if C = LF then
                  exit;
               else
                  Status := Bad_Status;
                  return;
               end if;
               
            elsif not Started
              and then C = SP
            then 
               -- We don't want to include the first space following the
               -- status-code to be included in the reason phrase, however
               -- pretty much everything else is
               Started := True;
               
            elsif not Is_VCHAR (C)
              and then C not in HT | SP
              and then not Is_obs_text (C)
            then
               -- Invalid content according to RFC 7230-3.1.2
               Status := Bad_Status;
               return;
               
            elsif Phrase_Strings.Length (Phrase)
              = Phrase_Strings.Max_Length
            then
               -- Out of room
               Status := Bad_Status;
               return;
               
            else
               -- Checks-out
               Phrase_Strings.Append (Source   => Phrase,
                                      New_Item => C);
               
            end if;
            
         end loop;
      end;
      
      
      pragma Assert (Status = OK);
      
      if not Version_OK then
         Status := Bad_Version;
      end if;
      
   end Read_Status_Line;
   
   ------------------
   -- Load_Headers --
   ------------------
   
   procedure Load_Headers
     (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
      Target  : in  out  Header_Set;
      Status  :     out  HTTP_Message_Status;
      Deadline: in       Ada.Calendar.Time)
   is
      use RFC_7230_7231;
      use type Ada.Containers.Count_Type;
      
      procedure Load_One_Header (Lead_Character: Character) is
         Buffer             : Character;
         Got_CRLF           : Boolean := True;
         Last_Was_Whitespace: Boolean := False;
         
         New_Header: HTTP_Header;
         Field_In: Field_Strings.Bounded_String renames New_Header.Field;
         Value_In: Value_Strings.Bounded_String renames New_Header.Value;
      begin
         -- Load the Field
         
         Buffer := Lead_Character;
         
         while Buffer not in ':' loop
            
            if Field_Strings.Length (Field_In)
              = Field_Strings.Max_Length
            then
               Status := Header_Field_Too_Long;
               return;
               
            elsif not Is_Token (Buffer) then
               Status := Bad_Header;
               return;
               
            else
               Field_Strings.Append (Source => Field_In, New_Item => Buffer);
            end if;
            
            Character'Read (Stream, Buffer);
            
            if Ada.Calendar.Clock > Deadline then
               Status := Timeout;
               return;
            end if;
            
         end loop;
         
         if Field_Strings.Length (Field_In) = 0 then
            -- This would indicate that there was no field name, which of
            -- course is not legal
            Status := Bad_Header;
            return;
         end if;
         
         -- Now the field value can be separated by "optional whitespace
         -- (OWS)", which could be any amount of whitespace, but not a CR/LF
         
         Fetch_Skipping_Whitespace
           (Stream   => Stream,
            Deadline => Deadline,
            C        => Buffer,
            Got_CRLF => Got_CRLF,
            Status   => Status);
         
         if Status /= OK then
            return;
         elsif Got_CRLF then
            -- This would mean no value
            Status := Bad_Header;
            return;
         end if;
         
         -- Load the value
         
         loop
            if Buffer = CR then
               Character'Read (Stream, Buffer);
               
               if Buffer = LF then
                  exit;
               else
                  Status := Bad_Header;
                  return;
               end if;
               
            elsif Value_Strings.Length (Value_In)
              = Value_Strings.Max_Length
            then
               Status := Header_Value_Too_Long;
               return;
               
            elsif Buffer in SP | HT then
               if Last_Was_Whitespace then
                  -- RFC 7230 3.2 only allows a single space/tab
                  Status := Bad_Header;
                  return;
                  
               else
                  Last_Was_Whitespace := True;
               end if;
               
            elsif Is_VCHAR (Buffer) or else Is_obs_text (Buffer) then
               -- This is fine
               
               Last_Was_Whitespace := False;
            end if;
            
            Value_Strings.Append (Source => Value_In, New_Item => Buffer);
            
            Character'Read (Stream, Buffer);
            
            if Ada.Calendar.Clock > Deadline then
               Status := Timeout;
               return;
            end if;
            
         end loop;
         
         
         -- We should now have a valid header which we can then pass to
         -- the specialed Internal_Add_Header subprogram, which will take
         -- care of the rest
         
         Internal_Add_Header (Target => Target,
                              Header => New_Header,
                              Status => Status);
      end Load_One_Header;
      
   begin
      Status := OK;
      
      while Status = OK loop
         declare
            Lead: constant Character := Character'Input (Stream);
         begin
            
            exit when Lead = CR
              and then Character'Input (Stream) = LF;

            Load_One_Header (Lead_Character => Lead);               
         end;
         
         if Ada.Calendar.Clock > Deadline then
            Status := Timeout;
            return;
         end if;
         
      end loop;
   end Load_Headers;
   
   
end Parsers;

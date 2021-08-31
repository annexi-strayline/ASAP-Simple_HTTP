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

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Strings.Maps.Constants;

with Simple_HTTP.RFC_3986;
with Simple_HTTP.RFC_7230_7231;

package body Simple_HTTP.HTTP_Message_Processor is
   
   --
   -- Header/Query Utilities
   --
   
   procedure Internal_Add_Header (Target: in out Header_Set;
                                  Header: in     HTTP_Header;
                                  Status:    out HTTP_Message_Status);
   -- This automatically translate the Field component of Header to lower-case
   -- and then adds Header to Target.
   --
   -- Status is set to one of:
   -- 
   -- OK                   : Operation was successful.
   --
   -- Header_Value_Too_Long: Header for field already exists, but appending the
   --                        new value was not possible
   --
   -- Too_Many_Headers     : A new header could not be added because Target is
   --                        saturated.

   
   function Header_Key (Header: HTTP_Header)
                       return Field_Strings.Bounded_String 
   is (Header.Field);
   
   package Field_Lookup is new Header_Sets.Generic_Keys
     (Key_Type        => Field_Strings.Bounded_String,
      Key             => Header_Key,
      Hash            => Header_Field_Hash,
      Equivalent_Keys => Field_Strings."=");
   
   
   function Field_Lookup_Case_Sensitive
     (Container: Header_Sets.Set; Field: String) return Header_Sets.Cursor
   is
      Term: constant Field_Strings.Bounded_String
        := Field_Strings.To_Bounded_String (Field);
   begin
      return Field_Lookup.Find (Container, Key => Term);
   end;
      
   function Field_Lookup_Case_Insensitive
     (Container: Header_Sets.Set; Field: String) return Header_Sets.Cursor
   is (Field_Lookup_Case_Sensitive 
         (Container, Field => Ada.Characters.Handling.To_Lower (Field)));
   
   -- Not to be confused from "Case_Insensitive", Field_Lookup_Case_Insensitive
   -- is to be used when Set is known to be case insensitive, and therefore all
   -- Fields within it are always lower-case
   
   --
   -- Parsers
   --
   
   package Parsers is
      
      procedure Read_Request_Line
        (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
         Method  :     out  RFC_7230_7231.Request_Method;
         URI     :     out  Path_Strings.Bounded_String;
         Status  :     out  HTTP_Message_Status;
         Deadline: in       Ada.Calendar.Time);
      
      -- Expects a "request-line" (RFC 7230-3.1.1), and sets Status according
      -- to the result. If Status is set to Bad_Version, this means everything
      -- else about the Request-Line was acceptable.
      --
      -- Note that URI includes any queries, but never includes any fragments,
      -- which are simply discarded.
      
      procedure Read_Status_Line
        (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
         Code    :    out RFC_7230_7231.Response_Status;
         Phrase  :    out Phrase_Strings.Bounded_String;
         Status  :    out HTTP_Message_Status;
         Deadline: in     Ada.Calendar.Time);
      
      -- Expects a "status-line" (RFC 7230-3.1.2), and sets Status according to
      -- the result. If Status is set to Bad_Version, this means everything
      -- else about status-line was acceptable.
      
      procedure Load_Headers
        (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
         Target  : in  out  Header_Set;
         Status  :     out  HTTP_Message_Status;
         Deadline: in       Ada.Calendar.Time);
      
      -- Expects a set of zero or more headers that follows the Request-Line
      -- Sets Status according to the result.
      --
      -- Header content is validated against RFC 7230.
      --
      -- Note that this parser does not accept trailing whitespace after a
      -- field content. If more than 2 consecutive spaces appear anywhere
      -- between the start of the field value and the final CRLF, the headers
      -- will be rejected.
      --
      -- If Status is OK, the payload will be immedately available on Stream.
      
   end Parsers;
   
   package body Parsers is separate;
   
   --
   -- Query Parser Tools
   --
   
   package Query_Parser_Tools is new RFC_3986.URI_Parser (Path_Strings);
   
   --
   -- HTTP_Message
   --
   
   ---------------
   -- To_Header --
   ---------------
   
   function To_Header (Field, Value: String) return HTTP_Header is
   begin
      return Header: HTTP_Header do
         Field_Strings.Set_Bounded_String (Target => Header.Field,
                                           Source => Field);
         
         Value_Strings.Set_Bounded_String (Target => Header.Value,
                                           Source => Value);
      end return;
   end;
   
   ------------
   -- Status --
   ------------
   
   function Status (Message: HTTP_Message) return HTTP_Message_Status is
     (Message.Status);
   
   -------------------
   -- Clear_Headers --
   -------------------
   
   procedure Clear_Headers (Message: in out HTTP_Message) is
   begin
      Message.Headers.Clear;
   end;
   
   ------------------
   -- Header_Count --
   ------------------
   
   function Header_Count (Message: HTTP_Message)
                         return Ada.Containers.Count_Type
   is (Message.Headers.Length);
   
   -------------------------
   -- Internal_Add_Header --
   -------------------------
   
   procedure Internal_Add_Header (Target: in out Header_Set;
                                  Header: in     HTTP_Header;
                                  Status:    out HTTP_Message_Status)
   is 
      use type Ada.Containers.Count_Type;
      use Header_Sets;
      
      Existing_Header: Cursor;
      
      Lowered_Header: constant HTTP_Header
        := (Field => Field_Strings.Translate
              (Source  => Header.Field,
               Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map),
            Value => Header.Value);
      
   begin
      Existing_Header := Field_Lookup.Find (Container => Target,
                                            Key       => Lowered_Header.Field);
      
      if Existing_Header /= No_Element then
         begin
            Value_Strings.Append (Source   => Target(Existing_Header).Value,
                                  New_Item => ',');
            Value_Strings.Append (Source   => Target(Existing_Header).Value,
                                  New_Item => Header.Value);
         exception
            when Ada.Strings.Length_Error =>
               Status := Header_Value_Too_Long;
         end;
            
      elsif Target.Length = Target.Capacity then
         Status := Too_Many_Headers;
         
      else
         Target.Insert (Lowered_Header);
         Status := OK;
      end if;
   end Internal_Add_Header;
   
   ----------------
   -- Add_Header --
   ----------------
   
   procedure Add_Header (Message      : in out HTTP_Message;
                         Field, Value : in     String)
   is
      Status: HTTP_Message_Status;
   begin
      if Field'Length = 0 then
         raise Constraint_Error with
           "Attempt to append a header with no Field.";
      end if;
      
      if Value'Length = 0 then
         raise Constraint_Error with
           "Attempt to append a header with no Value.";
      end if;
      
      Internal_Add_Header (Target => Message.Headers,
                           Header => To_Header (Field, Value),
                           Status => Status);
      
      case Status is
         when OK =>
            return;
         
         when Header_Value_Too_Long =>
            raise Constraint_Error with
              "Header value is too long";
            
         when Too_Many_Headers =>
            raise Constraint_Error with
              "Too many headers in message";
            
         when others =>
            raise Constraint_Error with
              "Internal_Add_Header failed with Status = "
              & HTTP_Message_Status'Image (Status);
      end case;
      

   end Add_Header;
   
   ------------------
   -- Header_Value --
   ------------------
   
   function Header_Value (Message: HTTP_Message; Field: String)
                         return String
   is
      use Header_Sets;
      
      Lookup: constant Cursor 
        := Field_Lookup_Case_Insensitive
          (Container => Message.Headers, Field => Field);
   begin
      if Lookup = No_Element then
         return "";
         
      else
         return Value_Strings.To_String (Message.Headers(Lookup).Value);
         
      end if;
   end Header_Value;
   
   ---------------------
   -- Iterate_Headers --
   ---------------------
   
   procedure Iterate_Headers
     (Message: in HTTP_Message;
      Process: not null access procedure (Field, Value: in String))
   is begin
      for Header of Message.Headers loop
         Process (Field => Field_Strings.To_String (Header.Field),
                  Value => Value_Strings.To_String (Header.Value));
      end loop;
   end Iterate_Headers;
   
   --
   -- HTTP_Request
   --
   
   ------------
   -- Method --
   ------------
   
   function Method (Request: HTTP_Request) 
                   return RFC_7230_7231.Request_Method 
   is (Request.Method);
   
   ----------------
   -- Set_Method --
   ----------------
   
   procedure Set_Method (Request: in out HTTP_Request;
                         Method : in     RFC_7230_7231.Request_Method)
   is begin
      Request.Method := Method;
   end;
   
   --------------
   -- Base_URI --
   --------------
   
   function Base_URI (Request: HTTP_Request) return String is
     (Path_Strings.To_String (Request.URI));
   
   ------------------
   -- Set_URI_Path --
   ------------------
   
   procedure Set_Base_URI (Request: in out HTTP_Request;
                           URI    : in     String)
   is begin
      Path_Strings.Set_Bounded_String (Target => Request.URI,
                                       Source => URI);
   end;
   
   -----------------
   -- Query_Count --
   -----------------
   
   function Query_Count (Request: HTTP_Request)
                        return Ada.Containers.Count_Type
   is (Request.Queries.Length);
   
   -------------------
   -- Clear_Queries --
   -------------------
   
   procedure Clear_Queries (Request: in out HTTP_Request) is
   begin
      Request.Queries.Clear;
   end;
   
   ---------------
   -- Add_Query --
   ---------------
   
   procedure Add_Query (Request     : in out HTTP_Request;
                        Field, Value: in     String)
   is
      Converted_Field: constant String
        := (if Query_Fields_Case_Insensitive then 
               Ada.Characters.Handling.To_Lower (Field)
            else
               Field);
      
   begin
      if Field'Length = 0 then
         raise Constraint_Error with
           "Attempt to append a query with no Field.";
      end if;
      
      -- Queries are allowed to be empty
      
      Request.Headers.Insert (To_Header (Field, Value));
   end;
   
   ------------------
   -- Query_Exists --
   ------------------
   
   function Query_Exists (Request: HTTP_Request; Field: String)
                         return Boolean
   is
      use Header_Sets;
   begin
      if Query_Fields_Case_Insensitive then
         return Field_Lookup_Case_Insensitive 
           (Container => Request.Queries, Field => Field)
           /= No_Element;
      else
         return Field_Lookup_Case_Sensitive 
           (Container => Request.Queries, Field => Field)
           /= No_Element;
      end if;
   end Query_Exists;
   
   -----------
   -- Query --
   -----------
   
   function Query (Request: HTTP_Request; Field: String) return String is
      use Header_Sets;
      
      Position: Cursor;
   begin
      if Query_Fields_Case_Insensitive then
         Position := Field_Lookup_Case_Insensitive
           (Container => Request.Queries, Field => Field);
         
      else
         Position := Field_Lookup_Case_Sensitive
           (Container => Request.Queries, Field => Field);
         
      end if;
      
      if Position = No_Element then
         return "";
      else
         return Value_Strings.To_String (Request.Queries(Position).Value);
      end if;
      
   end Query;
   
   ---------------------
   -- Iterate_Queries --
   ---------------------
   
   procedure Iterate_Queries
     (Request: in HTTP_Request;
      Process: not null access procedure (Field, Value: in String))
   is begin
      for Query of Request.Queries loop
         Process (Field => Field_Strings.To_String (Query.Field),
                  Value => Value_Strings.To_String (Query.Value));
      end loop;
   end Iterate_Queries;
   
   -------------------
   -- Write_Request --
   -------------------
   
   procedure Write_Request
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Request: in HTTP_Request)
   is
      use RFC_7230_7231;
      use type Ada.Containers.Count_Type;
   begin
      -- Request-line (RFC 2616-5.1)
      -- "Request-Line = Method SP Request-URI SP HTTP-Version CRLF"
      
      if Request.Status /= OK then
         raise Constraint_Error with "Status must be ""OK"" on output";
      end if;
      
      -- Method and path
      String'Write (Stream, Request_Method'Image (Request.Method) & SP);
      String'Write (Stream, Path_Strings.To_String (Request.URI));
      
      -- Queries
      if Request.Queries.Length > 0 then
         declare
            First_Query: Boolean := True;
         begin
            Character'Write (Stream, '?');
            
            for Query of Request.Queries loop
               if not First_Query then
                  Character'Write (Stream, '&');
               else
                  First_Query := False;
               end if;
               
               String'Write (Stream, Field_Strings.To_String (Query.Field));
               
               if Value_Strings.Length (Query.Value) > 0 then
                  Character'Write (Stream, '=');
                  String'Write (Stream, Value_Strings.To_String (Query.Value));
               end if;
            end loop;
         end;
      end if;
      
      String'Write (Stream, SP & "HTTP/1.1" & CRLF);
      
      -- Headers
      for Header of Request.Headers loop
         String'Write (Stream, Field_Strings.To_String (Header.Field));
         String'Write (Stream, ": ");
         String'Write (Stream, Value_Strings.To_String (Header.Value));
         String'Write (Stream, CRLF);
      end loop;
      
      -- Body preamble
      String'Write (Stream, CRLF);
   end Write_Request;
   
   ------------------
   -- Read_Request --
   ------------------
   
   procedure Read_Request
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Request: out HTTP_Request)
   is
      -- Unlike Write_Request, the read process is going to be much more
      -- strict
      
      use type Ada.Calendar.Time;

      Deadline: constant Ada.Calendar.Time
        := Ada.Calendar.Clock + Slowloris_Timeout;
      
      Query_Start: Natural;
      Version_OK: Boolean;
      
      procedure Load_Queries is
         Query_Segment: Path_Strings.Bounded_String;
         Query_Actual : HTTP_Header;
         
         Next_Query: Natural;
         Valid: Boolean;
         
         procedure Separate_Query is
            Split: constant Natural := Path_Strings.Index
              (Source  => Query_Segment,
               Pattern => "=",
               From    => 1);
            
            Segment_End: constant Positive
              := Path_Strings.Length (Query_Segment);
            
            Field_End: constant Natural
              := (if Split > 0 then Split - 1 else Segment_End);
            
            Value_Start: constant Natural
              := (if Split > 0 then Split + 1 else 0);
            
         begin
            if Split = 1 then
               -- Queries like "?=x", with no field names are not permitted.
               Request.Status := Bad_Query;
               return;
            end if;
            
            Field_Strings.Set_Bounded_String 
              (Target => Query_Actual.Field,
               Source => Path_Strings.Slice (Source => Query_Segment,
                                             Low    => 1,
                                             High   => Field_End));
            
            if Query_Fields_Case_Insensitive then
               Field_Strings.Translate 
                 (Source  => Query_Actual.Field,
                  Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
            end if;
            
            if Split > 0 then
               Value_Strings.Set_Bounded_String
                 (Target => Query_Actual.Value,
                  Source => Path_Strings.Slice (Source => Query_Segment,
                                                Low    => Value_Start,
                                                High   => Segment_End));
            else
               Query_Actual.Value := Value_Strings.Null_Bounded_String;
            end if;
            
         end Separate_Query;
         
         
         procedure Insert_Query is
            Discard: Header_Sets.Cursor;
            Inserted: Boolean;
         begin
            Request.Queries.Insert (New_Item => Query_Actual,
                                    Position => Discard,
                                    Inserted => Inserted);
         
            if not Inserted then
               Request.Status := Bad_Query;
            else
               Request.Status := OK;
            end if;
         end Insert_Query;
         
      begin
         Query_Parser_Tools.First_Query (URI              => Request.URI,
                                         Query            => Query_Segment,
                                         Next_Query_Start => Next_Query,
                                         Valid            => Valid);
         
         if not Valid then
            Request.Status := Bad_Query;
            return;
         elsif Path_Strings.Length (Query_Segment) = 0 then
            -- No queries
            return;
         end if;
         
         Separate_Query;
         Insert_Query;
         
         if Request.Status /= OK then return; end if;
         
         while Next_Query > 0 loop
            Query_Parser_Tools.Next_Query (URI              => Request.URI,
                                           Query            => Query_Segment,
                                           Next_Query_Start => Next_Query,
                                           Valid            => Valid);
            
            if not Valid then
               Request.Status := Bad_Query;
               return;
            end if;
            
            Separate_Query;
            Insert_Query;
            
            if Request.Status /= OK then return; end if;
         end loop;
         
      end Load_Queries;
      
   begin
      -- Force a default init of Request
      Request := (HTTP_Message with others => <>);
      
      Parsers.Read_Request_Line
        (Stream   => Stream,
         Method   => Request.Method,
         URI      => Request.URI,
         Status   => Request.Status,
         Deadline => Deadline);
      
      if Request.Status in OK | Bad_Version then
         Version_OK := (Request.Status = OK);
      else
         return;
      end if;
      
      -- See if we have any queries, and make note of the start position
      -- so that we can remove it.
      
      if Path_Strings.Length (Request.URI) > 0 then
         Query_Start := Path_Strings.Index (Source  => Request.URI,
                                            Pattern => "?",
                                            From    => 1);
         Load_Queries;
         
         if Request.Status /= OK then
            return;
         end if;
      else
         Query_Start := 0;
      end if;
      
      
      if Query_Start > 0 then
         Request.URI := Path_Strings.Head (Source => Request.URI,
                                           Count  => Query_Start - 1);
      end if;
      
      Parsers.Load_Headers
        (Stream   => Stream,
         Target   => Request.Headers,
         Status   => Request.Status,
         Deadline => Deadline);
      
      if Request.Status /= OK then
         return;
      elsif not Version_OK then
         Request.Status := Bad_Version;
      end if;
      
      -- Headers are loaded and the stream should be exactly at the payload.
      
   end Read_Request;
   
   -------------------
   -- Input_Request --
   -------------------
   
   function Input_Request
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class)
     return HTTP_Request
   is begin
      return Request: HTTP_Request do
         Read_Request (Stream, Request);
      end return;
   end Input_Request;
   
   --
   -- HTTP_Response
   --
   
   -----------------
   -- Status_Code --
   -----------------
   
   function Status_Code (Response: HTTP_Response)
                        return RFC_7230_7231.Response_Status
   is (Response.Status_Code);
   
   -------------------
   -- Reason_Phrase --
   -------------------
   
   function Reason_Phrase (Response: HTTP_Response) return String is
     (Phrase_Strings.To_String (Response.Reason_Phrase));
   
   -------------------------
   -- Set_Standard_Status --
   -------------------------
   
   procedure Set_Standard_Status
     (Response: in out HTTP_Response;
      Code    : in     RFC_7230_7231.Standard_Response_Status)
   is
      package RFC_7231 renames RFC_7230_7231;
      
      procedure Set_Phrase (Phrase: in String) with Inline is
      begin
         Phrase_Strings.Set_Bounded_String (Target => Response.Reason_Phrase,
                                            Source => Phrase);
      end;
   begin
      case Code is
         when 100 => Set_Phrase (RFC_7231.Phrase_100);
         when 101 => Set_Phrase (RFC_7231.Phrase_101);
            
         when 200 => Set_Phrase (RFC_7231.Phrase_200);
         when 201 => Set_Phrase (RFC_7231.Phrase_201);
         when 202 => Set_Phrase (RFC_7231.Phrase_202);
         when 203 => Set_Phrase (RFC_7231.Phrase_203);
         when 204 => Set_Phrase (RFC_7231.Phrase_204);
         when 205 => Set_Phrase (RFC_7231.Phrase_205);
         when 206 => Set_Phrase (RFC_7231.Phrase_206);
            
         when 300 => Set_Phrase (RFC_7231.Phrase_300);
         when 301 => Set_Phrase (RFC_7231.Phrase_301);
         when 302 => Set_Phrase (RFC_7231.Phrase_302);
         when 303 => Set_Phrase (RFC_7231.Phrase_303);
         when 304 => Set_Phrase (RFC_7231.Phrase_304);
         when 305 => Set_Phrase (RFC_7231.Phrase_305);
         when 307 => Set_Phrase (RFC_7231.Phrase_307);
            
         when 400 => Set_Phrase (RFC_7231.Phrase_400);
         when 401 => Set_Phrase (RFC_7231.Phrase_401);
         when 402 => Set_Phrase (RFC_7231.Phrase_402);
         when 403 => Set_Phrase (RFC_7231.Phrase_403);
         when 404 => Set_Phrase (RFC_7231.Phrase_404);
         when 405 => Set_Phrase (RFC_7231.Phrase_405);
         when 406 => Set_Phrase (RFC_7231.Phrase_406);
         when 407 => Set_Phrase (RFC_7231.Phrase_407);
         when 408 => Set_Phrase (RFC_7231.Phrase_408);
         when 409 => Set_Phrase (RFC_7231.Phrase_409);
         when 410 => Set_Phrase (RFC_7231.Phrase_410);
         when 411 => Set_Phrase (RFC_7231.Phrase_411);
         when 412 => Set_Phrase (RFC_7231.Phrase_412);
         when 413 => Set_Phrase (RFC_7231.Phrase_413);
         when 414 => Set_Phrase (RFC_7231.Phrase_414);
         when 415 => Set_Phrase (RFC_7231.Phrase_415);
         when 416 => Set_Phrase (RFC_7231.Phrase_416);
         when 417 => Set_Phrase (RFC_7231.Phrase_417);
         when 426 => Set_Phrase (RFC_7231.Phrase_426);
            
         when 500 => Set_Phrase (RFC_7231.Phrase_500);
         when 501 => Set_Phrase (RFC_7231.Phrase_500);
         when 502 => Set_Phrase (RFC_7231.Phrase_500);
         when 503 => Set_Phrase (RFC_7231.Phrase_500);
         when 504 => Set_Phrase (RFC_7231.Phrase_500);
         when 505 => Set_Phrase (RFC_7231.Phrase_500);
            
         when others =>
            raise Constraint_Error with "Status code is not a standard code.";
            
      end case;
      
      Response.Status_Code := Code;
      
   end Set_Standard_Status;
   
   -------------------------
   -- Set_Explicit_Status --
   -------------------------
   
   procedure Set_Explicit_Status
     (Response: in out HTTP_Response;
      Code    : in     RFC_7230_7231.Response_Status;
      Phrase  : in     String)
   is begin
      Response.Status_Code := Code;
      Phrase_Strings.Set_Bounded_String (Target => Response.Reason_Phrase,
                                         Source => Phrase);
   end Set_Explicit_Status;
   
   --------------------
   -- Write_Response --
   --------------------
   
   procedure Write_Response
     (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
      Response: in HTTP_Response)
   is
      CRLF: String renames RFC_7230_7231.CRLF;
   begin
      -- RFC 7230, 3.1.2 - Status line
      String'Write (Stream, "HTTP/1.1");
      String'Write (Stream, RFC_7230_7231.Response_Status'Image 
                      (Response.Status_Code) & ' ');
      -- Note that the image of the resonse code contains the requisite 'SP'
      -- that needs to follow HTTP-version
      
      String'Write (Stream, Phrase_Strings.To_String (Response.Reason_Phrase));
      String'Write (Stream, CRLF);
      
      -- Headers
      for Header of Response.Headers loop
         String'Write (Stream, Field_Strings.To_String (Header.Field) & ": ");
         String'Write (Stream, Value_Strings.To_String (Header.Value));
         String'Write (Stream, CRLF);
      end loop;
      
      String'Write (Stream, CRLF);
      
   end Write_Response;
   
   -------------------
   -- Read_Response --
   -------------------
   
   procedure Read_Response
     (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
      Response: out HTTP_Response)
   is
      use type Ada.Calendar.Time;
      Deadline: constant Ada.Calendar.Time
        := Ada.Calendar.Clock + Slowloris_Timeout;
      
      Version_OK: Boolean;
      
   begin
      -- Status line
      Parsers.Read_Status_Line
        (Stream   => Stream,
         Code     => Response.Status_Code,
         Phrase   => Response.Reason_Phrase,
         Status   => Response.Status,
         Deadline => Deadline);
      
      if Response.Status in OK | Bad_Version then
         Version_OK := (Response.Status = OK);
      else
         return;
      end if;
      
      Parsers.Load_Headers
        (Stream   => Stream,
         Target   => Response.Headers,
         Status   => Response.Status,
         Deadline => Deadline);
      
      if Response.Status /= OK then
         return;
      elsif not Version_OK then
         Response.Status := Bad_Version;
      end if;
      
      -- Headers are loaded and the stream should be exactly at the payload.
      
   end Read_Response;
   
   --------------------
   -- Input_Response --
   --------------------
   
   function Input_Response
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class)
     return HTTP_Response
   is begin
      return Response: HTTP_Response do
         Read_Response (Stream, Response);
      end return;
   end Input_Response;
      
   
end Simple_HTTP.HTTP_Message_Processor;

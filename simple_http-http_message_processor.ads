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

with Ada.Streams;
with Ada.Strings.Bounded;
with Ada.Strings.Bounded.Hash;
with Ada.Containers.Bounded_Hashed_Sets;

with Simple_HTTP.RFC_7230_7231;

generic
   Path_Bounds : in Positive := 200;
   
   -- This the bounds for the actual path component of a Request URI
   
   Field_Bounds: in Positive := 80;
   Value_Bounds: in Positive := 200;
   
   -- These are the bounds for the Field and Value strings for both
   -- the Headers, and for Request URI query components.
   
   Header_Limit: in Ada.Containers.Count_Type := 20;
   
   -- This is the maximum number of Headers that can be in a message
   
   Query_Limit : in Ada.Containers.Count_Type := 20;
   
   -- This is the maximum number of Queries that can be in a Request message
   
   Query_Fields_Case_Insensitive: in Boolean := False;
   
   -- RFC 2616 requres that header fields are case-insensitive, but no such
   -- provision is made for queries. If true, all query field names will be
   -- converted to lower-case, including when used in a search.
   --
   -- The prevailing consensus seems to suggest that queries fields follow
   -- generally the convention of common programming languages used by APIs,
   -- which are typically case-sensistive.
   
   Slowloris_Timeout: in Duration := 10.0;
   
   -- This is the _total_ amount of time that any messare 'Read or 'Input
   -- operation can take to complete. This should be combined with a similar
   -- timeout value for source stream itself, if possible, to mitigate
   -- slowloris attacks.

   
   Whitespace_Limit: in Positive := 60;
   
   -- This is the maximum number of consecutive "LWS" whitespace characters
   -- that may appear when skipping whitespace during parsing of an HTTP
   -- message.

   
package Simple_HTTP.HTTP_Message_Processor is
   
   use type Ada.Containers.Hash_Type;
   use type Ada.Containers.Count_Type;
   
   ------------------
   -- HTTP_Message --
   ------------------
   
   type HTTP_Message_Status is
     (OK, 
      -- Message is valid
      
      Timeout,
      -- Message took too long to complete (Slowloris_Timeout exceeded)
      
      Too_Much_Whitespace,
      -- Whitespace_Limit was exceeded.
      
      Bad_Input,
      -- A general violation of the input data was encountered.
      
      Bad_Version,
      -- Message version was not HTTP 1.1.
      -- These messages are still parsed as if they are HTTP 1.1,
      -- and so the user can decide how they wish to proceed
      
      Bad_Method,
      -- The given request method was not recognized.
      
      Bad_Status,
      -- The given reply status value was out of range or of an unexpected
      -- format
      
      Bad_URI,
      -- The format of the request URI was invalid
      
      Bad_Query,
      -- This typicaly means a query without a field name was found in the URI,
      -- an empty query was found, or a duplicate.
      -- For example "GET /index.html?=1337", "GET /?x=y&&", or "GET /?x=y&x=q"
      
      URI_Too_Long,
      -- The URI of the request was longer than Path_Bounds
      
      Bad_Header,
      -- The format of a Header was incorrect
      
      Header_Field_Too_Long,
      -- The header's field was longer than Field_Bounds
      
      Header_Value_Too_Long,
      -- The header's value was longer than Value_Bounds
      
      Too_Many_Headers);
   -- Header_Capacity was exceeded
   
   
   type HTTP_Message is abstract tagged private;
   
   -- HTTP_Message is the root type for both HTTP_Request and HTTP_Response
   -- messages.
   
   function  Status (Message: HTTP_Message) return HTTP_Message_Status;
   
   
   procedure Clear_Headers (Message: in out HTTP_Message);
   
   -- Removes all Headers from Message.
   
   function Header_Count (Message: HTTP_Message)
                         return Ada.Containers.Count_Type;
   
   -- Returns the number of headers contained in the Message
   
   procedure Add_Header (Message      : in out HTTP_Message;
                         Field, Value : in     String)
   with Pre => Header_Count (Message) < Header_Limit
               and Field'Length > 0
               and Value'Length > 0;
   
   -- Adds a specified Header to Message. If this exceeds the the capacity
   -- of Message, or if Name or Value are null strings, Constraint_Error is
   -- raised.
   --
   -- If Field already exists in the message, Value is appended to the current
   -- value, immediately after a ','. This is the convention specified in
   -- RFC 7230-3.2.2, and preserves the order. Note that "Set-Cookie" (RFC6265)
   -- would need separate treatment. This parser does not treat "Set-Cookie"
   -- specially, so it will collapse multiple "Set-Cookies". See RFC7230-3.2.2
   -- for additional commentary on this issue.
   --
   -- Note that the 'Read and 'Input operations invoke (in effect) Add_Header,
   -- and will therefore also obey the order of multiple instances of the same
   -- header Field name.
   --
   -- RFC 7230 specifies that header field names are case-insensitive (Section
   -- 3.2). Therefore, Field is automatically converted to lower-case.
   --
   -- Add_Header does not validate the contents of Field or Value, however,
   -- 'Input/'Read does. It is therefore important that the user ensure that
   -- the contents of Field and Value are valid (including being properly
   -- escaped) when adding them programatically.
   --
   -- Notes:
   -- * Headers will be stored (and output) in an arbitrary order.
   -- * 'Input/'Read does NOT support "obsolete line folding". Such input will
   --   result in a Bad_Header status.
   
   function  Header_Value (Message: HTTP_Message; Field: String)
                          return String;
   
   -- Returns the Value of the specific Field. If the specified Field does not
   -- exist, a null String is returned. Field is case-insensitive. Note that
   -- header fields without a value are not accepted.
   
   procedure Iterate_Headers
     (Message: in HTTP_Message;
      Process: not null access procedure (Field, Value: in String));
   
   -- Invokes Process for each header, in an arbitrary order.
   
   ------------------
   -- HTTP_Request --
   ------------------
   
   type HTTP_Request is new HTTP_Message with private;
   
   function Method (Request: HTTP_Request) return RFC_7230_7231.Request_Method;
   
   
   procedure Set_Method (Request: in out HTTP_Request;
                         Method : in     RFC_7230_7231.Request_Method);
   
   -- Note that extension-methods are not supported by this implementation.
   -- Any such methods will cause the HTTP_Request to be invalid.
   
   
   function  Base_URI (Request: HTTP_Request) return String;
   procedure Set_Base_URI (Request: in out HTTP_Request;
                           URI    : in     String);
   
   -- Returns/Sets the part of the RFC2396 URI containing "scheme", "authority"
   -- and "net_path" / "abs_path" components of the as any user and authority
   -- components, if given. These components may be seperatedly identified and
   -- stripped via specialized functions in the RFC_2396 package. Base_URI
   -- shall not contain any query components when set.
   --
   -- During HTTP_Request'Read/'Input, the Query components are specially
   -- parsed and removed from the URI. During 'Write/'Output, any query
   -- components are added to the Request-Line URI automatically.
   --
   -- Note that fragments are not supported or recognized by this
   -- implementation. The presence of a fragment in Set_Base_URI will cause
   -- Constraint_Error. For 'Input/'Read, fragments are stripped from
   -- HTTP_Request URIs and discarded.
   
   function Query_Count (Request: HTTP_Request)
                        return Ada.Containers.Count_Type;
   
   -- Returns the number of queries contained in the Request
   
   procedure Clear_Queries (Request: in out HTTP_Request);
   
   -- Removes all queries from the request
   
   
   procedure Add_Query (Request     : in out HTTP_Request;
                        Field, Value: in     String)
   with Pre => Query_Count (Request) < Query_Limit
               and Field'Length > 0;
   
   -- Adds a specified query to Request. If this exceeds the the capacity
   -- of Request, or if Name or Value are null strings, Constraint_Error is
   -- raised.
   --
   -- Note: See the formal parameter Query_Fields_Case_Insensitive for
   -- details on the case handling of query Field names.
   --
   -- Note that headers will be stored (and output) in an arbitrary order.
   
   function Query_Exists (Request: HTTP_Request; Field: String)
                         return Boolean;
   
   -- Returns True if Field exists as a query.
   
   function Query (Request: HTTP_Request; Field: String) return String;
   
   -- Returns the Value for the given Field. If no such Query exits,
   -- a null String is returned. Note that a query might exist, but have
   -- a nil value. See Query_Exists to differentiate these cases.
   
   
   procedure Iterate_Queries
     (Request: in HTTP_Request;
      Process: not null access procedure (Field, Value: in String));
   
   -- Invokes Process for each query, in an arbitrary order.
   
   procedure Write_Request
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Request: in HTTP_Request)
   with Pre => Request.Status = OK;
   
   procedure Read_Request
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Request: out HTTP_Request);
   
   function Input_Request
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class)
     return HTTP_Request;
   
   for HTTP_Request'Write  use Write_Request;
   for HTTP_Request'Output use Write_Request;
   for HTTP_Request'Read   use Read_Request;
   for HTTP_Request'Input  use Input_Request;
   
   -- Read and Input will mark the Request as invalid immediately upon receipt
   -- of any invalidating data, and will not continue to read the stream.
   --
   -- Note that headers are always converted to lower-case
   
   -------------------
   -- HTTP_Response --
   -------------------
   
   type HTTP_Response is new HTTP_Message with private;
   
   function Status_Code (Response: HTTP_Response)
                        return RFC_7230_7231.Response_Status;
   
   function Reason_Phrase (Response: HTTP_Response) return String;
   
   -- Note that the Status_Phrase is limited to a maximum of 200 characters,
   -- and will be truncated if exceeded.
   
   procedure Set_Standard_Status
     (Response: in out HTTP_Response;
      Code    : in     RFC_7230_7231.Standard_Response_Status);
   
   -- Sets the response code and phrase, based on the code. Code must be
   -- one of the standard codes defined in RFC 7231, else Constraint_Error
   -- is explicitly raised (assuming predicate checking is not enabled).
   
   procedure Set_Explicit_Status
     (Response: in out HTTP_Response;
      Code    : in     RFC_7230_7231.Response_Status;
      Phrase  : in     String)
   with Pre => Phrase'Length <= 200;
   
   
   procedure Write_Response
     (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
      Response: in HTTP_Response);
   
   procedure Read_Response
     (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
      Response: out HTTP_Response);
   
   function Input_Response
     (Stream: not null access Ada.Streams.Root_Stream_Type'Class)
     return HTTP_Response;
   
   for HTTP_Response'Write  use Write_Response;
   for HTTP_Response'Output use Write_Response;
   for HTTP_Response'Read   use Read_Response;
   for HTTP_Response'Input  use Input_Response;
   
private
   
   package Field_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Field_Bounds);
   
   package Value_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Value_Bounds);
   
   package Path_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Path_Bounds);
   
   package Phrase_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
     (200);
   
   ------------------
   -- HTTP_Message --
   ------------------
   
   type HTTP_Header is
      record
         Field: Field_Strings.Bounded_String;
         Value: Value_Strings.Bounded_String;
      end record;
   
   function Header_Field_Hash is new Ada.Strings.Bounded.Hash (Field_Strings);
   
   function HTTP_Header_Hash (Header: HTTP_Header)
                             return Ada.Containers.Hash_Type
   is (Header_Field_Hash (Header.Field));
   
   function Equivalent_Header (Left, Right: HTTP_Header) return Boolean
   is (Field_Strings."=" (Left.Field, Right.Field));
   
   package Header_Sets is new Ada.Containers.Bounded_Hashed_Sets
     (Element_Type        => HTTP_Header,
      Hash                => HTTP_Header_Hash,
      Equivalent_Elements => Equivalent_Header);
   
   Default_Header_Set_Modulus: constant Ada.Containers.Hash_Type
     := Header_Sets.Default_Modulus (Header_Limit);
   
   subtype Header_Set is Header_Sets.Set
     (Capacity => Header_Limit,
      Modulus  => Default_Header_Set_Modulus);
   
   type HTTP_Message is abstract tagged
      record
         Status : HTTP_Message_Status;
         Headers: Header_Set;
      end record;
   
   ------------------
   -- HTTP_Request --
   ------------------
   
   Default_Query_Set_Modulus: constant Ada.Containers.Hash_Type
     := Header_Sets.Default_Modulus (Query_Limit);
   
   subtype Query_Set is Header_Sets.Set
     (Capacity => Query_Limit,
      Modulus  => Default_Query_Set_Modulus);
   
   type HTTP_Request is new HTTP_Message with
      record
         Method    : RFC_7230_7231.Request_Method;
         URI       : Path_Strings.Bounded_String;
         Queries   : Query_Set;
      end record;
   
   -------------------
   -- HTTP_Response --
   -------------------
   
   type HTTP_Response is new HTTP_Message with
      record
         Status_Code  : RFC_7230_7231.Response_Status;
         Reason_Phrase: Phrase_Strings.Bounded_String;
      end record;
   
end Simple_HTTP.HTTP_Message_Processor;

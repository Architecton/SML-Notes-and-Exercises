type status = { (* make status an alias for a record with two named fields. *)
    version : string,
    code : int
}

(* Create a constant of type status. *)
val my_status : status = { version = "HTTP/1.1", code = 418 };

(* statusToString: map status to its string representation *)
(* Note that use of sharp/projection notation is strongly discouraged... *)
fun statusToString(s : status) : string =
	(* Use concatenation. *)
    (#version s) ^ " " ^ Int.toString(#code s) ^ " " ^
    (case (#code s) of
          100 => "Continue"
        | 200 => "OK"
        | 301 => "Moved Permanently"
        | 404 => "Not Found"
        | 418 => "I'm a teapot"
        | 503 => "Service Unavailable"
        (* in še mnogo več ... *)
        | _ => "" (* neznan status *)
    )

(* Dates *)

(* make a type date that is an alias to a record with named fields. *)
type date = {
    dayOfWeek : string,
    dayOfMonth : int,
    month : string,
    year : int,
    hour : int,
    minute : int,
    second : int,
    timeZone : string
}


(* dateToString: create a string representation of date type value *)
(* Note: use of sharp notation is strongly discouraged. *)
fun dateToString (d : date) : string =
    (#dayOfWeek d) ^ ", " ^
    Int.toString(#dayOfMonth d) ^ " " ^
    (#month d) ^ " " ^
    Int.toString(#year d) ^ " " ^
    Int.toString(#hour d) ^ ":" ^
    Int.toString(#minute d) ^ ":" ^
    Int.toString(#second d) ^ " " ^
    (#timeZone d)

(* URI *)
(* Make a datatype host with three constructors. *)
datatype host = (* vsota tipov *)
      IPV4 of int * int * int * int
    | IPV6 of string * string * string * string * string * string * string * string
    | HostName of string list

(* hostToString: create a string representation of a value of host datatype *)
fun hostToString ( h : host ) : string =
    case h of (* Pattern match contructor *)
        IPV4 ipv4 => Int.toString(#1 ipv4) ^ "." ^
                     Int.toString(#2 ipv4) ^ "." ^
                     Int.toString(#3 ipv4) ^ "." ^
                     Int.toString(#4 ipv4)
      | IPV6 ipv6 => "[" ^
                     (#1 ipv6) ^ ":" ^
                     (#2 ipv6) ^ ":" ^
                     (#3 ipv6) ^ ":" ^
                     (#4 ipv6) ^ ":" ^
                     (#5 ipv6) ^ ":" ^
                     (#7 ipv6) ^ ":" ^
                     (#7 ipv6) ^ ":" ^
                     (#8 ipv6) ^ "]"
      | HostName hn => String.concatWith "." hn

(* Create a new type uri that is an alias to a record with named fields. *)
type uri = {
    scheme : string, (* to polje je obvezno ... *)
    user : string option, (* ... ostala polja pa ne, zato dodamo "option" *)
    password : string option,
    host : host option,
    port : int option,
    path : string list option,
    query : ((string * string) list) option, (* seznam ključev in vrednosti *)
    fragment : string option
}

(* pathToString: make a string representation of a path. *)
fun pathToString ( p : string list ) : string =
    if null p then "" (* If path is empty return empty string. *)
    else "/" ^ ( hd p ) ^ pathToString( tl p ) (* Concatenate / with head of list and make recursive call for tail. *)

(* queryToString : make a string representation of a query. *)
fun queryToString ( q : (string * string) list ) : string =
    if null q then "" (* If passed empty list return empty string. *)
    else (
        case q of
              a::nil => (#1 a) ^ "=" ^ (#2 a) (* If q contains only one element, concatenate two elements of tuples (Note use of sharp notation which is discouraged). *)
            | _ => (#1 (hd q)) ^ "=" ^ (#2 (hd q)) ^ "&" ^ queryToString (tl q) (* Else concatenate two element of head tuple, add "&" to and and make recursive call for tail *)
    )

(* uriToString: make a string representation of URI *)
fun uriToString (u : uri) : string =
    (#scheme u) ^ ":" ^ ( (* concatenate scheme to optional fields. *)
        if isSome(#host u) (* Check if optional fields are nonempty. *)
        then (
            "//" ^ (
                if isSome(#user u)
                then valOf(#user u) ^ (
                    if isSome(#password u)
                    then ":" ^ valOf(#password u)
                    else ""
                ) ^ "@"
                else ""
            ) ^ hostToString(valOf(#host u)) ^ (
                if isSome(#port u)
                then ":" ^ Int.toString(valOf(#port u))
                else ""
            )
        )
        else ""
    ) ^ (
        if isSome(#path u)
        then pathToString(valOf(#path u))
        else ""
    ) ^ (
        if isSome(#query u)
        then "?" ^ queryToString(valOf(#query u))
        else ""
    ) ^ (
        if isSome(#fragment u)
        then "#" ^ valOf(#fragment u)
        else ""
    )


(* HTTP RESPONSE *)

(* HEADER FIELDS *)

(* expires *)

(* datatype expires with two constructors. *)
datatype expires =
      ExpiresDate of date
      (* Be careful: this Date must not have the same name as the one in the "field" type. *)
    | Number of int

(* expiresToString: make a string representation of value of expires datatype. *)
fun expiresToString (e : expires) : string =
    case e of
          ExpiresDate d => dateToString(d) (* Reuse already defined function *)
        | Number s => (* Represent negative numbers with minus sign and not tilde. *)
            if s < 0
            then "-" ^ Int.toString(~s)
            else Int.toString(s)

(* datatype transferEncoding that has 5 constructors (possible values). *)
datatype transferEncoding = chunked | compress | deflate | gzip | identity

(* transferEncodingToString: make a string representation of a value of type transferEncoding. *)
fun transferEncodingToString(te : transferEncoding) : string =
    case te of
          chunked => "chunked"
        | compress => "compress"
        | deflate => "deflate"
        | gzip => "gzip"
        | identity => "identity"

(* header fields *)

(* make a datatype field that has 8 constructors. *)
datatype field =
      Server of string
    | ContentLength of int
    | ContentType of string
    | Location of uri
    | Date of date
    | Expires of expires
    | LastModified of date
    | TransferEncoding of transferEncoding

(* fieldToString: make a string representation of a value of field datatype *)
fun fieldToString (f : field) : string =
    case f of
          Server s => "Server: " ^ s
        | ContentLength cl => "Content-Length: " ^ Int.toString(cl)
        | ContentType ct => "Content-Type: " ^ ct
        | Location l => "Location: " ^ uriToString(l)
        | Date d => "Date: " ^ dateToString(d)
        | Expires e => "Expires: " ^ expiresToString(e)
        | LastModified lm => "Last-Modified: " ^ dateToString(lm)
        | TransferEncoding te => "Transfer-Encoding: " ^
                                 transferEncodingToString(te)

(* response *)

(* Make a type response which is an alias to a record with named fields *)
type response = {
    status : status,
    headers : field list,
    body : string
}

(* responseToString: make a string representation of value of response type. *)
fun responseToString(r : response) : string =
    statusToString(#status r) ^ "\n" ^
    (String.concatWith "\n" (map fieldToString (#headers r))) ^ "\n\n" ^
    (#body r) ^ "\n\n"

(* my_response: an example HTTP response *)
val my_response : response = {
    status = { (* status record *)
        version = "HTTP/1.1",
        code = 418
    },
    headers = [ (* headers list *)
        Server "gws",
        ContentLength 1024,
        ContentType "text/html; charset=UTF-8",
        Date {
            dayOfWeek = "Fri",
            dayOfMonth = 30,
            month = "Mar",
            year = 2018,
            hour = 20,
            minute = 16,
            second = 32,
            timeZone = "CEST"
        },
        (* Expires (Number ~1), *)
        Expires (ExpiresDate {
            dayOfWeek = "Thu",
            dayOfMonth = 29,
            month = "Mar",
            year = 2019,
            hour = 16,
            minute = 28,
            second = 26,
            timeZone = "CEST"
        }),
        Location {
            scheme = "http", (* obvezno polje, zato spredaj nima "SOME" *)
            host = SOME (IPV6 (
                "0000", "1234", "abcd", "01ab", "ffff", "9999", "534e", "a3f1"
            )),
            (* host = SOME (IPV4 (127, 0, 0, 1)), *)
            (* host = SOME (HostName ["google", "si"]), *)
            user = SOME "admin",
            password = SOME "ultra_safe_password_1A!",
            port = NONE, (* to polje ni podano *)
            path = SOME ["web", "page"],
            query = SOME [("a", "b"), ("x", "y")],
            fragment = SOME "top"
        },
        TransferEncoding gzip
    ],
    body = "Content of the web page." (* Body of the HTTP response - the contents of the web page. *)
}; (* This semicolon is not optional as it is followed by a non-reserved word (print is not a declaration but an expression). *)

(* Print string representation of response. *)
print(responseToString my_response);

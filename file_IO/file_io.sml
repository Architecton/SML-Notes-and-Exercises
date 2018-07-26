(* Function that writes strings to a file *)
fun write_poem(filename) =
    let 
        val file = TextIO.openOut(filename) (* Open file and save reference in file. If file does not exist, it is created. *)
        val _ = TextIO.output(file, "Roses are red,\nViolets are blue.\n") (* Write a string to file referenced by file. *)
        val _ = TextIO.output(file, "This is just a simple test!") (* Write another string to a file referenced by file. *)
    in 
        (* Close file reference. *)
        TextIO.closeOut(file)
    end

(* read_poem: read text from a file into a list of strings. *)
fun read_poem(filename) =
    let 
        val file = TextIO.openIn filename       (* Open file with name passed to function for input. *)
        val poem = TextIO.inputAll file         (* Read everything in file referenced by file input poem. *)
        val _ = TextIO.closeIn file             (* Close file reference. *)
    in 
        String.tokens (fn c => c = #"\n") poem  (* Split string into substrings delimited by newline character. *)
    end

(* get_poem: read text from a file into a string. *)
fun get_poem(filename) =
    let
        val file = TextIO.openIn filename       (* Open file for reading and save reference to file. *)
        val poem = TextIO.inputAll file         (* Get contents of file and save to poem. *)
        val _ = TextIO.closeIn file             (* Close file reference. *)
    in
        String.toString poem                    (* Convert results to string type. *)
    end



val _ = write_poem "roses.txt"                   (* Write to roses.txt. *)
val test_poem = read_poem "roses.txt"            (* save tokenized contents of roses.txt to test_poem. *)
val whole_poem = get_poem "roses.txt"            (* Save whole poem as a string to whole_poem. *)
fun write_to_file (filename : string) : unit  =
	let
		val file = TextIO.openOut(filename)
		val _ = TextIO.output(file, "This is just a simple test!")
	in
		TextIO.closeOut(file)
	end

fun read_from_file (filename : string) : string =
	let
		val file = TextIO.openIn(filename)
		val contents = TextIO.inputAll(file)
		val _ = TextIO.closeIn(file)
	in
		String.toString contents
	end
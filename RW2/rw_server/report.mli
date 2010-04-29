module Report :
sig
	val tor : Unix.file_descr	

	val report : Ast.report -> unit
end

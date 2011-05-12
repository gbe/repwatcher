module Core :
sig
	val fd : Unix.file_descr

	val add_watch : string -> Inotify.wd option -> bool -> unit
	val add_watch_children : string list -> unit
	val print_ht : unit -> unit
	val what_to_do : Inotify.event -> unit
end


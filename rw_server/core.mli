module Core :
sig
	val fd : Unix.file_descr
	val debug_event : bool

	val add_watch : string -> Inotify.wd option -> bool -> unit
	val add_watch_children : string list -> unit
	val print_ht : unit -> unit
	val file_created : Inotify.wd -> string -> unit
	val file_opened : ?created:bool -> Inotify.wd -> string -> unit
	val file_closed : ?written:bool -> Inotify.wd -> string -> unit
	val directory_created : Inotify.wd -> string -> unit
	val directory_moved_from : Inotify.wd -> string -> unit
	val directory_moved_to : Inotify.wd -> string -> unit
	val directory_deleted : Inotify.wd -> string -> unit
end


open CalendarLib
open Calendar
open Unix
open Types_date

class date =
object(self)

  val unix_raw = Unix.gettimeofday ()
  val mutable utc = None
  val mutable locale = None

  initializer
    utc <- Some (self#_set_Utc) ;
    locale <- Some (self#_set_Locale) ;


  method private _strip_option arg =
    match arg with
    | None -> assert false
    | Some a -> a


  method private _from_unix_to_cal tm =
    Calendar.make
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
      tm.tm_sec


  method private _set_Utc =
    self#_from_unix_to_cal (Unix.gmtime unix_raw)

  method private _set_Locale =
    self#_from_unix_to_cal (Unix.localtime unix_raw)


  method private _string_of_cal c =
    let string_of_month = function
      | Jan -> "01"
      | Feb -> "02"
      | Mar -> "03"
      | Apr -> "04"
      | May -> "05"
      | Jun -> "06"
      | Jul -> "07"
      | Aug -> "08"
      | Sep -> "09"
      | Oct -> "10"
      | Nov -> "11"
      | Dec -> "12"
    in  
    Printf.sprintf "%d-%s-%02d %02d:%02d:%02d"
      (Calendar.year c)
      (string_of_month (Calendar.month c))
      (Calendar.day_of_month c)
      (Calendar.hour c)
      (Calendar.minute c)
      (Calendar.second c)


  method get_utc = self#_strip_option utc

  method get_str_utc =
    self#_string_of_cal (self#get_utc)

  method print_utc =
    print_endline self#get_str_utc


  method private _get_locale = self#_strip_option locale

  method get_str_locale =
    self#_string_of_cal (self#_get_locale)

  method print_locale =
    print_endline self#get_str_locale


  method get_diff (d : date) =
    let diff = Calendar.precise_sub self#get_utc d#get_utc in
    let (years, months, days, seconds) = Period.ymds diff in
    
    let time = Time.lmake ~second:seconds () in
    {
      years = years;
      months = months;
      days = days;
      hours = Time.hour time;
      minutes = Time.minute time;
      seconds = Time.Second.to_int (Time.second time)
    }


  method get_diff_sec (d : date) =
    let diff = Calendar.sub self#get_utc d#get_utc in
    let time_diff = Calendar.Period.safe_to_time diff in
    Time.Second.to_float (Time.Period.to_seconds time_diff)

end;;

(*
let _ =
  let date = new date in
  sleep 3 ;
  let date2 = new date in

  date#print_utc;
  date#print_locale;

  print_endline "";

  date2#print_utc;
  date2#print_locale;


  let d = date2#get_diff date in
  Printf.printf ("%dy %dm %dd %dh %dm %ds\n") d.years d.months d.days d.hours d.minutes d.seconds;

  let total_sec = date2#get_diff_sec date in 
  Printf.printf ("Num total sec: %d\n") total_sec;

  Pervasives.flush Pervasives.stdout
;;
*)

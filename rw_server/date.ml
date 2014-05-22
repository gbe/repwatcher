open CalendarLib
open Calendar

let date () =
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

  let d = Calendar.now () in
  Printf.sprintf
    "%d-%s-%d %d:%d:%d"
    (Calendar.year d)
    (string_of_month (Calendar.month d))
    (Calendar.day_of_month d)
    (Calendar.hour d)
    (Calendar.minute d)
    (Calendar.second d)
;;

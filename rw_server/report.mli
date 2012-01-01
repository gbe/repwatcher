module Report :
sig
  val prepare_data : Types.f_file -> Types.file2clients
  val report       : Types.report -> Types.report_ret
end

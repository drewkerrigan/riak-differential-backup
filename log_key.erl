-module(log_key).
-export([log/1]).

log(Object) ->

  Operation =
  case dict:is_key(<<"X-Riak-Deleted">>, riak_object:get_metadata(Object)) of
          true -> 
           delete;
          _ ->
           store
  end,

  file:write_file("log/keyfile.log", 
  	io_lib:fwrite("~s,~s,~s\n", [
  		Operation,
  		binary_to_list(riak_object:bucket(Object)), 
  		binary_to_list(riak_object:key(Object))]), [append]),

  Object.
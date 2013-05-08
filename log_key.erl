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
  	io_lib:fwrite("~p,~p,~p\n", [
  		Operation,
  		binary_to_atom(riak_object:bucket(Object), utf8), 
  		binary_to_atom(riak_object:key(Object), utf8)]), [append]),

  Object.
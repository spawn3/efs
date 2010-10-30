%-define(NOTEST, true).
-ifdef(TEST).
-undef(debug).
-include_lib("eunit/include/eunit.hrl").
-endif.

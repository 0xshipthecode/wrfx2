% Copyright (C) 2013-2015 Martin Vejmelka, UC Denver
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
% of the Software, and to permit persons to whom the Software is furnished to do
% so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
% INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR
% A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(fmda).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([write_fmda_cfg/4]).

write_fmda_cfg(WrfInP,WrfOutPrevP,Covs,Fname) ->
  CovStr = string:join(lists:map(fun(X) -> ["\"",X,"\""] end, Covs), ", "),
  file:write_file(Fname, lists:flatten([
    "{\n",
    "\t'observations' : 'observations',\n",
    "\t'output_dir' : '.',\n",
    "\t'wrf_input' : '", WrfInP, "',\n",
    "\t'wrf_output_prev' : '",WrfOutPrevP,"',\n",
    "\t'write_fields' : 'moisture_only',\n",
    "\t'Q' : [1e-4, 5e-5, 1e-5, 1e-4, 1e-4],\n",
    "\t'P0' : [0.01, 0.01, 0.01, 0.1, 0.1],\n",
    "\t'covariates' : [", CovStr, "],\n",
    "\t'assimilation_time_window' : 3600\n",
    "}\n" ])).


%%
%% ct_surefire
%%
%% Copyright (c) 2011 Martin Scholl (ms@funkpopes.org)
%% Copyright (c) 2011 global infinipool GmbH
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(ct_surefire).

%% API
-export([
	 to_surefire_xml/1,
	 to_surefire_xml/2
	]).

-define(TAG(Name), ?TAG(Name, _, _)).
-define(TAG(Name, Childs), ?TAG(Name, _, Childs)).
-define(TAG(Name, Args, Childs), {<<??Name>>, Args, Childs}).

-record(res, {test_id,suite,case_id,duration,result,error}).

%%%===================================================================
%%% API
%%%===================================================================
to_surefire_xml([CtDir, OutputDir]) when is_list(CtDir), 
					 is_list(OutputDir) ->
    to_surefire_xml(CtDir, OutputDir).

%% computes a surefire XML report from a CT directory
%% @param CtDir directory where CT logs are stored
%% @param OutputDir Directory where XML report files will be stored
to_surefire_xml(CtDir, OutputDir) ->
    RunDir = find_latest_rundir(CtDir),
    Reports = available_reports(RunDir),
    [begin
	 ReportFilename = "TEST-" ++ App ++ ".xml",
	 OutputFilename = filename:join([OutputDir, ReportFilename]),
	 write_report(ReportDir, App, OutputFilename)
     end || {App, ReportDir} <- Reports],
    ok.


find_latest_rundir(Dir) ->
    Dirs = filelib:wildcard(filename:join([Dir, "ct_run.ct*"])),
    hd(lists:reverse(lists:sort(Dirs))).

available_reports(Dir) ->
    Apps = [string:substr(A, 5) || A <- filelib:wildcard("{apps,src}.*", Dir)],
    Dirs = filelib:wildcard(filename:join([Dir, "{apps,src}.*", "run.*"])),
    lists:zip(Apps, Dirs).

write_report(ReportDir, App, OutputFilename) ->
    LogFilename = filename:join(ReportDir, "suite.log.html"),
    Results = parse_results(LogFilename),
	io:format("results: ~p~n ~p~n", [LogFilename, Results]),
    Xml = results_to_xml(App, Results),
    ok = file:write_file(OutputFilename, Xml).

parse_results(Filename) ->
    {ok, RawHTML} = file:read_file(Filename),
    case ct_surefire_html:parse(RawHTML) of
	{<<"html">>, _, Doc} ->
		{ok, Rows} = find([<<"body">>, <<"table">>, <<"tbody">>], Doc),
	    filter_tests(Rows);
	Res ->
		io:format("ppp: ~p~n", [Res]),
	    error(unsupported_html, Filename)
    end.

results_to_xml(App, Results) ->
    XmlHdr = io_lib:format(
	       "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
	       "<testsuite tests=\"~b\" failures=\"0\" errors=\"~b\" skipped=\"~b\" time=\"~f\" name=\"ct.~s'\">", 
	       [
		length(Results),
		count_by_result(failed, Results),
		count_by_result(skipped, Results),
		sum_duration(Results),
		App
	       ]),
    Testcases = [xmlify_result(R) || R <- Results],
    XmlFooter = "</testsuite>",

    [XmlHdr, Testcases, XmlFooter].

find([], Doc) ->
	{ok, Doc};
find([Elem | Path], Doc) ->
	case lists:keyfind(Elem, 1, Doc) of
		{Elem, _, Inner} ->
			find(Path, Inner);
		false ->
			{error, notfound}
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
xmlify_result(#res{result = ok, suite = Suite, case_id = Case,
		   test_id = Id, duration = D}) ->
    io_lib:format(" <testcase name=\"~s.~s.~s\" time=\"~s\" />~n",
		  [Suite,Case,Id,D]);
xmlify_result(#res{error = Error, suite = Suite, case_id = Case,
		   test_id = Id, duration = D}) ->
    io_lib:format(" <testcase name=\"~s.~s.~s\" time=\"~s\">~n"
		  "  <error type=\"error\">~s</error>~n"
		  "  <system-out></system-out>~n"
		  " </testcase>~n",
		  [Suite,Case,Id,D,Error]).

filter_tests(HTML) ->
    lists:reverse(lists:foldl(fun fold_row/2, [], HTML)).

fold_row(Row, Result) ->
    case parse_row(Row) of
	false ->
	    Result;
	Res ->
	    [Res | Result]
    end.

parse_row(Row) ->
    case Row of
		{<<"tr">>, _, [
		  {<<"td">>, _, [{_, _, [TestNum]}]}
		, {<<"td">>, _, [{_, _, [Suite]}]}
		, {<<"td">>, _, [{_, _, [_Group]}]}
		, {<<"td">>, _, [{_, _, [Case]}]}
		, {<<"td">>, _, [{_, _, [_Log]}]}
		, {<<"td">>, _, [{_, _, [Time]}]}
		, {<<"td">>, _, [{_, _, [RawResult]}]}
		| Info
		]} ->
			io:format("num: ~p~nsuite: ~p~ncase: ~p~ntime: ~p~nraw: ~p~ninfo: ~p~n"
				, [TestNum, Suite, Case, Time, RawResult, Info]),
			case RawResult of
				<<"Ok">> ->
					{res, TestNum, Suite, Case, parse_duration(Time)};
				RawResult ->
					Error = iolist_to_binary(error_to_text(Info)),
					Duration = parse_duration(Time),
					Result = to_result(RawResult),
					{res, TestNum, Suite, Case, Duration, Result, Error}
			end;
	_ ->
	    false
    end.

to_result(<<"SKIPPED">>) ->
    skipped;
to_result(<<"FAILED">>) ->
    failed.

error_to_text(B) when is_binary(B) ->
    B;
error_to_text({_, _, Text}) ->
    error_to_text(Text);
error_to_text(HtmlError) when is_list(HtmlError) ->
    lists:map(fun error_to_text/1, HtmlError).

count_by_result(Status, Res) ->
    Filtered = lists:filter(
		 fun (#res{result=S}) -> S == Status end,
		 Res),
    length(Filtered).

sum_duration(Res) ->
    Durations = lists:map(
		  fun(#res{duration=D}) ->
			  list_to_float(binary_to_list(D))
		  end,
		  Res),
    lists:foldl(fun erlang:'+'/2, 0.0, Durations).				 

parse_duration(D) ->
    BS = byte_size(D)-1,
    <<Dura:BS/binary,"s">> = D,
    Dura.

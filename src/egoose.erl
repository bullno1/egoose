-module(egoose).
-behaviour(gen_server).
-export([start_link/1]).
-export([extract_from_url/2, extract_from_url/3,
         extract_from_html/3, extract_from_html/4]).
-export([title/1, canonical_url/1, meta_description/1,
         meta_keywords/1, top_image/1, body/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
-export_type([goose_ref/0, article/0]).

-record(article, {
	canonical_url :: binary(),
	title :: binary(),
	meta_description :: binary(),
	meta_keywords :: binary(),
	top_image :: binary(),
	body :: binary()
}).

-opaque article() :: #article{}.
-type goose_ref() :: pid().
-type goose_opt() :: {user_agent, binary()}
                   | {fetch_image, boolean()}
                   | {connection_timeout, pos_integer()}
                   | {imagemagick_convert_path, binary()}
                   | {imagemagick_identify_path, binary()}
                   | {local_storage_path, binary()}
                   | {min_bytes_for_images, pos_integer()}
                   | {socket_timeout, pos_integer()}.

-spec start_link(list(goose_opt())) -> {ok, goose_ref()}.
start_link(Opts) ->
	gen_server:start_link(?MODULE, Opts, [{timeout, 5000}]).

-spec title(article()) -> binary().
title(#article{title = Value}) -> Value.

-spec canonical_url(article()) -> binary().
canonical_url(#article{canonical_url = Value}) -> Value.

-spec meta_description(article()) -> binary().
meta_description(#article{meta_description = Value}) -> Value.

-spec meta_keywords(article()) -> binary().
meta_keywords(#article{meta_keywords = Value}) -> Value.

-spec top_image(article()) -> binary().
top_image(#article{top_image = Value}) -> Value.

-spec body(article()) -> binary().
body(#article{body = Value}) -> Value.

-spec extract_from_html(GooseRef, Url, Html) -> article() when
	 GooseRef :: goose_ref()
	,Url :: binary()
	,Html :: binary().
extract_from_html(GooseRef, Url, Html) -> extract_from_html(GooseRef, Url, Html, 5000).

-spec extract_from_html(GooseRef, Url, Html, Timeout) -> article() when
	 GooseRef :: goose_ref()
	,Url :: binary()
	,Html :: binary()
	,Timeout :: pos_integer() | infinity.
extract_from_html(GooseRef, Url, Html, Timeout) ->
	gen_server:call(GooseRef, {sync_cmd, {extract, Url, Html}}, Timeout).

-spec extract_from_url(GooseRef, Url) -> article() when
	 GooseRef :: goose_ref()
	,Url :: binary().
extract_from_url(GooseRef, Url) -> extract_from_url(GooseRef, Url, 5000).

-spec extract_from_url(GooseRef, Url, Timeout) -> article() when
	 GooseRef :: goose_ref()
	,Url :: binary()
	,Timeout :: pos_integer() | infinity.
extract_from_url(GooseRef, Url, Timeout) ->
	gen_server:call(GooseRef, {sync_cmd, {extract, Url}}, Timeout).

%--- gen_server callbacks ---

init(Opts) ->
	Port = open_port(
		{spawn_executable, os:find_executable("java")},
		[{packet, 4}
		,{args, [
			"-jar",
			filename:join(code:priv_dir(egoose), "GoosePort.jar")
		 ]}
		,exit_status
		,use_stdio
		,binary
		,hide
		,{parallelism, true}]),

	DefaultOpts = [
		{imagemagick_convert_path, list_to_binary(os:find_executable("convert"))},
		{imagemagick_identify_path, list_to_binary(os:find_executable("identify"))}
	],
	FinalOpts = lists:ukeymerge(1, lists:ukeysort(1, Opts), lists:ukeysort(1, DefaultOpts)),
	{ok, ok} = call_sync_cmd(Port, {init, FinalOpts}),

	{ok, Port}.

terminate(_Reason, _Port) -> ok.

handle_call({sync_cmd, Cmd}, _From, Port) ->
	case call_sync_cmd(Port, Cmd) of
		{ok, Result} -> {reply, Result, Port};
		{error, Reason} -> {stop, Reason, Port}
	end.

handle_cast(_Msg, State) -> {stop, unexpected, State}.

handle_info({Port, {exit_status, _Status}}, Port) -> {stop, port_closed, Port};
handle_info(_Info, State) -> {stop, unexpected, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%--- private ---

call_sync_cmd(Port, Cmd) ->
	port_command(Port, term_to_binary(Cmd)),
	receive
		{Port, {data, Data}} ->
			{ok, binary_to_term(Data)};
		{Port, {exit_status, _Status}} ->
			{error, port_closed}
	end.

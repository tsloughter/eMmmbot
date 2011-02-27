%%%----------------------------------------------------------------
%%% @author  Tristan Sloughter <tristan.sloughter@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2011 Tristan Sloughter
%%%----------------------------------------------------------------
-module(emb_db).

-behaviour(gen_server).

%% API
-export([start_link/3,
         all/1,
         find/2,
         create/2,
         update/3,
         next/2,
         prev/2,
         first/1,
         last/1,
         add_tags/3,
         remove_tag/3,
         terminate/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export_type([]).

-define(SERVER, ?MODULE).

-define(LIMIT, 12).

-record(state, {db, host}).

%%%===================================================================
%%% Public Types
%%%===================================================================

%%%===================================================================
%%% API
%%%===================================================================

start_link(Server, Port, DB) ->
    gen_server:start_link(?MODULE, [Server, Port, DB], []).

all(PID) ->
    gen_server:call(PID, all).

next(PID, Key) ->
    gen_server:call(PID, {next, Key}).

prev(PID, Key) ->
    gen_server:call(PID, {prev, Key}).

first(PID) ->
    gen_server:call(PID, first).

last(PID) ->
    gen_server:call(PID, last).

find(PID, ID) ->
    gen_server:call(PID, {find, ID}).

create(PID, Doc) ->
    gen_server:call(PID, {create, Doc}).

update(PID, ID, JsonDoc) ->
    gen_server:call(PID, {update, ID, JsonDoc}).

add_tags(PID, ID, Tags) ->
    gen_server:call(PID, {add_tags, ID, Tags}).

remove_tag(PID, ID, Tag) ->
    gen_server:call(PID, {remove_tag, ID, Tag}).

terminate(PID) ->
    gen_server:call(PID, terminate).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([Server, Port, DB]) ->
    SPort = io_lib:format("~p", [Port]),
    Host = list_to_binary("http://" ++ Server ++ ":" ++ SPort),
    CouchServer = couchbeam:server_connection(Server, Port, "", []),
    {ok, CouchDB} = couchbeam:open_db(CouchServer, DB),
    {ok, #state{db=CouchDB, host=Host}}.

%% @private
handle_call(all, _From, #state{db=DB, host=Host}=State) ->
    Docs = get_docs(Host, DB, [{descending, false}, {limit, ?LIMIT}]),
    {reply, mochijson2:encode(Docs), State};
handle_call({next, Key}, _From, #state{db=DB, host=Host}=State) ->
    Docs = get_docs(Host, DB, [{descending, false}, {startkey, list_to_binary(Key)}, {skip, 1}, {limit, ?LIMIT}]),
    {reply, mochijson2:encode(Docs), State};
handle_call({prev, Key}, _From, #state{db=DB, host=Host}=State) ->
    Docs = get_docs(Host, DB, [{descending, true}, {startkey, list_to_binary(Key)}, {skip, 1}, {limit, ?LIMIT}]),
    {reply, mochijson2:encode(Docs), State};
handle_call(first, _From, #state{db=DB, host=Host}=State) ->
    Docs = get_docs(Host, DB, [{descending, false}, {limit, ?LIMIT}]),
    {reply, mochijson2:encode(Docs), State};
handle_call(last, _From, #state{db=DB, host=Host}=State) ->
    Docs = get_docs(Host, DB, [{descending, true}, {limit, ?LIMIT}]),
    {reply, mochijson2:encode(Docs), State};
handle_call({find, ID}, _From, #state{db=DB, host=Host}=State) ->
    [Doc] = get_docs(Host, DB, [{key, list_to_binary(ID)}]),
    {reply, mochijson2:encode(Doc), State};
handle_call({create, Doc}, _From, #state{db=DB}=State) ->
    {ok, Doc1} = couchbeam:save_doc(DB, Doc),
    {NewDoc} = couchbeam_doc:set_value(<<"id">>, couchbeam_doc:get_id(Doc1), Doc1),
    {reply, mochijson2:encode({struct, NewDoc}), State};
handle_call({update, ID, NewDoc}, _From, #state{db=DB}=State) ->
    IDBinary = list_to_binary(ID),
    {ok, Doc} = couchbeam:open_doc(DB, IDBinary),
    NewDoc2 = couchbeam_doc:set_value(<<"_id">>, IDBinary, {NewDoc}),
    NewDoc3 = couchbeam_doc:set_value(<<"_rev">>, couchbeam_doc:get_rev(Doc), NewDoc2),
    {ok, {Doc1}} = couchbeam:save_doc(DB, NewDoc3),
    {reply, mochijson2:encode({struct, Doc1}), State};
handle_call({add_tags, ID, Tags}, _From, #state{db=DB}=State) ->
    IDBinary = list_to_binary(ID),
    {ok, Doc} = couchbeam:open_doc(DB, IDBinary),
    OldTags = couchbeam_doc:get_value(<<"tags">>, Doc),
    NewTags = list_to_binary(Tags),
    NewTags2 = merge_tags(OldTags, NewTags),
    io:format("NewTags2 ~p~n", [NewTags2]),
    NewDoc = couchbeam_doc:set_value(<<"tags">>, NewTags2, Doc),
    {ok, _} = couchbeam:save_doc(DB, NewDoc),
    {reply, ok, State};
handle_call({remove_tag, ID, Tag}, _From, #state{db=DB}=State) ->
    IDBinary = list_to_binary(ID),
    {ok, Doc} = couchbeam:open_doc(DB, IDBinary),
    Tags = couchbeam_doc:get_value(<<"tags">>, Doc),
    TagBin = list_to_binary(Tag),
    NewTags2 = remove_tag(Tags, TagBin),
    io:format("NewTags2 ~p~n", [NewTags2]),
    NewDoc = couchbeam_doc:set_value(<<"tags">>, NewTags2, Doc),
    {ok, _} = couchbeam:save_doc(DB, NewDoc),
    {reply, ok, State};
handle_call(terminate, _From, State) ->
    {stop, normal, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

merge_tags(Tags, NewTags) ->
    TagsList = lists:sort(re:split(Tags, ",")),
    NewTagsList = lists:sort(re:split(NewTags, ",")),
    bjoin(lists:umerge(TagsList, NewTagsList)).

remove_tag(Tags, Tag) ->
    TagsList = re:split(Tags, ","),
    bjoin(lists:remove(Tag, TagsList)).

bjoin(List) ->
    F = fun(A, B) -> <<A/binary, ",", B/binary>> end,
    lists:foldr(F, <<"">>, List).

get_docs(_Host, DB, Options) ->
    {ok, AllDocs} = couchbeam:view(DB, {"all", "find"}, Options),
    {ok, Results} = couchbeam_view:fetch(AllDocs),

    {[{<<"total_rows">>, _Total},
      {<<"offset">>, _Offset},
      {<<"rows">>, Rows}]} = Results,

    lists:map(fun({Row}) ->
                      {<<"value">>, {Value}} = lists:keyfind(<<"value">>, 1, Row),
                      %ID = couchbeam_doc:get_value(<<"id">>, {Value}),
                      %URL = list_to_binary(couchbeam:doc_url(DB, ID)),
                      %Where = couchbeam_doc:get_value(<<"where">>, {Value}),
                      %Where2 = <<Host/binary, "/", URL/binary, "/", Where/binary>>,
                      %{Doc} = couchbeam_doc:set_value(<<"where">>, Where2, {Value}),
                      {struct, Value}
              end, Rows).


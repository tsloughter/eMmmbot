-module(emb_resource_images).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         finish_request/2,
         from_json/2,
         to_json/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {db}).

init([]) ->
    {ok, PID} = emb_db_sup:start_child(),
    {ok, #ctx{db=PID}}.

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'PUT'], ReqData, Ctx}.

content_types_accepted(ReqData, Ctx) ->
    {[{"application/json", from_json}], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    emb_db_sup:terminate_child(Ctx#ctx.db),
    {true, ReqData, Ctx}.

process_post(ReqData, Ctx) ->
    [{JsonDoc, _}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    {struct, Doc} = mochijson2:decode(JsonDoc),
    NewDoc = emb_db:create(Ctx#ctx.db, {Doc}),
    ReqData2 = wrq:set_resp_body(NewDoc, ReqData),
    {true, ReqData2, Ctx}.

to_json(ReqData, Ctx) ->
    Results = case wrq:raw_path(ReqData) of
                  "/last" ->
                      emb_db:last(Ctx#ctx.db);
                  "/first" ->
                      emb_db:first(Ctx#ctx.db);
                  _ ->
                      case wrq:path_info(tag, ReqData) of
                          undefined ->
                              case wrq:path_info(id, ReqData) of
                                  undefined ->
                                      case wrq:path_info(next, ReqData) of
                                          undefined ->
                                              case wrq:path_info(prev, ReqData) of
                                                  undefined ->
                                                      emb_db:all(Ctx#ctx.db);
                                                  Prev ->
                                                      emb_db:prev(Ctx#ctx.db, Prev)
                                              end;
                                          Next ->
                                              emb_db:next(Ctx#ctx.db, Next)
                                      end;
                                  ID ->
                                      emb_db:find(Ctx#ctx.db, ID)
                              end;
                          Tag ->
                              [{DecodedTag,[]}] = mochiweb_util:parse_qs(Tag),
                              emb_db:images_for_tag(Ctx#ctx.db, DecodedTag)
                      end
              end,

    {Results, ReqData, Ctx}.

from_json(ReqData, Ctx) ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {false, ReqData, Ctx};
        ID ->
            JsonDoc = wrq:req_body(ReqData),
            {struct, Doc} = mochijson2:decode(JsonDoc),
            NewDoc = emb_db:update(Ctx#ctx.db, ID, Doc),
            ReqData2 = wrq:set_resp_body(NewDoc, ReqData),
            {true, ReqData2, Ctx}
    end.

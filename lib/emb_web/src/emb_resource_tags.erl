-module(emb_resource_tags).

-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         finish_request/2,
         to_json/2,
         delete_resource/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {db}).

init([]) ->
    {ok, PID} = emb_db_sup:start_child(),
    {ok, #ctx{db=PID}}.

allowed_methods(ReqData, Ctx) ->
    {['HEAD', 'GET', 'POST', 'DELETE'], ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    {[{"application/json", to_json}], ReqData, Ctx}.

finish_request(ReqData, Ctx) ->
    emb_db_sup:terminate_child(Ctx#ctx.db),
    {true, ReqData, Ctx}.

delete_resource(ReqData, Ctx) ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {false, ReqData, Ctx};
        ID ->
            case wrq:get_qs_value("tag", ReqData) of
                undefined ->
                    {false, ReqData, Ctx};
                Tag ->
                    emb_db:remove_tag(Ctx#ctx.db, ID, Tag),
                    {true, ReqData, Ctx}
            end
    end.

process_post(ReqData, Ctx) ->
    case wrq:path_info(id, ReqData) of
        undefined ->
            {false, ReqData, Ctx};
        ID ->
            case wrq:get_qs_value("tags", ReqData) of
                undefined ->
                    {false, ReqData, Ctx};
                Tags ->
                    emb_db:add_tags(Ctx#ctx.db, ID, Tags),
                    {true, ReqData, Ctx}
            end
    end.

to_json(ReqData, Ctx) ->
    Results = emb_db:tag_cloud(Ctx#ctx.db),
    {Results, ReqData, Ctx}.

%% This is the application resource file (.app file) for the emb_web,
%% application.
{application, emb_web,
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [emb_web_app,
              emb_web_sup]},
   {registered,[emb_web_sup]},
   {applications, [kernel, stdlib]},
   {mod, {emb_web_app,[]}},
   {start_phases, []}]}.


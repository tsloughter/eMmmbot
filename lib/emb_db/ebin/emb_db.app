%% This is the application resource file (.app file) for the emb_db,
%% application.
{application, emb_db,
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [emb_db_app,
              emb_db_sup]},
   {registered,[emb_db_sup]},
   {applications, [kernel, stdlib]},
   {mod, {emb_db_app,[]}},
   {start_phases, []}]}.


%% This is the application resource file (.app file) for the emb_db,
%% application.
{application, emb_db,
  [{description, "mmmbot database interface"},
   {vsn, "0.1.0"},
   {modules, [emb_db_app,
              emb_db_sup,
              emb_db]},
   {registered,[emb_db_sup]},
   {applications, [kernel, stdlib, couchbeam]},
   {mod, {emb_db_app,[]}},
   {start_phases, []}]}.


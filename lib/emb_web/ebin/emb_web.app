%% This is the application resource file (.app file) for the emb_web,
%% application.
{application, emb_web,
  [{description, "Web frontend"},
   {vsn, "0.1.0"},
   {modules, [emb_web_app,
              emb_web_sup,
              emb_resource_images,
              emb_resource_tags,
              emb_resource_static]},
   {registered,[emb_web_sup]},
   {applications, [kernel, stdlib, webmachine]},
   {mod, {emb_web_app,[]}},
   {start_phases, []}]}.


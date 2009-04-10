{application, impex,
 [{description, "Erlang Term Importer/Exporter"},
  {vsn, "0.1"},
  {modules, [
  	impex,
  	impex_parser
  ]},
  % {mod, {impex_app, []}},
  {registered, []},
  {env, [
  	{resources, ["priv", "priv/lib"]}
  ]},
  {applications, [kernel, stdlib]}
]}.

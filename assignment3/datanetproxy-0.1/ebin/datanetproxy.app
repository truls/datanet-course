%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, datanetproxy,
 [{description, "A HTTP proxy for the datanet course"},
  {vsn, "0.1.0"},
  {modules, [pr_app,
	     pr_sup,
	     pr_server]},
  {registered, [pr_sup]},
  {applications, [kernel, stdlib, sasl, ibrowse]},
  {mod, {pr_app, []}}
]}.

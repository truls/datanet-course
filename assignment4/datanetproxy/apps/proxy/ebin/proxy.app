%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-

{application, proxy,
 [{description, "A HTTP proxy for the datanet course"},
  {vsn, "0.2.0"},
  {modules, [pr_app,
	     pr_sup,
	     pr_server,
	     pr_nodeparser,
	     pr_root_sup,
	     pr_tracker]},
  {registered, [pr_sup]},
  {applications, [kernel, stdlib, sasl, ibrowse]},
  {mod, {pr_app, []}}
]}.

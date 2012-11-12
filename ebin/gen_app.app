{application, gen_app,
 [
  {description, "Library Server"},
  {vsn, "0.1"},
  {registered, []},
  {modules, [gen_app,supervisor_library]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { gen_app, []}}
 ]}.

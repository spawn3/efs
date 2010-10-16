-module (efs_fuse).
-behaviour (application).
-export ([ start/0,
           start/2,
           stop/0,
           stop/1 ]).

%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

%% @hidden

start () ->
  application:start (fuserl),
  application:start (efs_fuse).

%% @hidden

start (_Type, _Args) ->
  { ok, LinkedIn } = application:get_env (efs_fuse, linked_in),
  { ok, MountPoint } = application:get_env (efs_fuse, mount_point),
  { ok, MountOpts } = application:get_env (efs_fuse, mount_opts),
  case application:get_env (efs_fuse, make_mount_point) of
    { ok, false } -> 
      ok;
    _ ->
      case file:make_dir (MountPoint) of
        ok ->
          ok;
        { error, eexist } ->
          ok
      end
  end,

  efs_fuse_sup:start_link (LinkedIn, MountPoint, MountOpts).

%% @hidden

stop () ->
  application:stop (efs_fuse).

%% @hidden

stop (_State) ->
  ok.

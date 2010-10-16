{ application, efs_fuse,
  [ 
    { description, "Proc filesystem for Erlang." }, 
    { vsn, "0.4.1" },
    { modules, [ efs_fuse, efs_fuse_sup, efs_fuse_srv ] },
    { registered, [  ] },
    { applications, [ kernel, stdlib  , fuserl ] },
    { mod, { efs_fuse, [] } },
    { env, [ { linked_in, false }, { make_mount_point, true }, { mount_point, "/home/gj/git/efs/fuse" }, { mount_opts, "allow_other,default_permissions" }] }
  ] 
}.

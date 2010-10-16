{application, efs_cds, [
    {description, "EFS Distributed File System"},
    {vsn, "0.0.1"},
    {modules, [
        efs_app, 
        efs_sup, 
        libefs,
        efs, 
        efs_mds,
            efs_mds_model,
            efs_mds_chkpool,
            efs_mds_diskpool,
        efs_cds,
            efs_cds_model,
        efs_shell
        ]},
    {registered, [efs_sup, efs, efs_mds]},
    {applications, [kernel, stdlib]},
    {mod, {efs_app, [{cds,1,4}]}},
    {start_phases, []}
]}.

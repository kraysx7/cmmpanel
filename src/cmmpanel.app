{application, cmmpanel,
 [{description, "Панель управления CMM"},
    {vsn, "1"},
    {applications, [kernel, stdlib]},
    {mod,{main_app,[]}},
	{env,
		[
            %%{cmmcontroller_node, 'cmmcontroller@192.168.10.100'},
            {cmmcontroller_node, 'cmmcontroller@192.168.10.100'},
            {sensors,
                          [
                    {volt_io_cpu_ports, " IO_CPU V", 1.8,  1.44, 2.16},
                    {volt_io_dimm,      "IO_DIMM V", 1.5,  1.2,  1.8},
                    {volt_cores,        "  CORES V", 0.9,  0.72, 1.08},
                    {volt_cpu,          "    CPU V", 1.15, 0.92, 1.38}
                          ]
            },
            {tests_desc, 
                        [
                    {0,"    Reboot"},
                    {1,"TEST START"},
                    {2,"      tMEM"},
                    {3,"      tCPU"},
                    {4,"      eth0"},
                    {5,"      eth1"},
                    {6,"       MEM"},
                    {7,"  HDD(R/W)"},
                    {8,"    KERNEL"}
                        ]
            }
	]}
 ]}.

x = c4a_sysdata_export()
c4a_sysdata_import(x)
y = c4a_sysdata_export()
identical(x, y)

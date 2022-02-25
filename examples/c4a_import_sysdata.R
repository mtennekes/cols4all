x = c4a_export_sysdata()
c4a_import_sysdata(x)
y = c4a_export_sysdata()
identical(x, y)

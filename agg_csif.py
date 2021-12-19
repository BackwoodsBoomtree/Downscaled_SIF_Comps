import os
from glob import glob

out_res   = '0.20'
var_name  = 'csif_clear'
in_path   = '/mnt/g/CSIF/8-day/clear/orig'
out_path  = '/mnt/g/CSIF/8-day/clear/0.20'
grid_file = 'gridfile_0.20.txt'
remap_met = 'remapcon'


# Create output dirs
if not os.path.exists(out_path):
    os.makedirs(out_path)

# Create input and output filename lists
in_files  = [y for x in os.walk(in_path) for y in glob(os.path.join(x[0], '*.nc'))]
out_files = [x.replace(in_path, out_path).replace('.nc', ''.join(['.', out_res, '.nc'])) for x in in_files]           

# NC files created by R (terra) need to have the proj and grid_mapping paramaters renamed
beg_cmd_rename_crs = ''.join(['ncrename -a crs@proj4,proj_params '])
for i in range(len(in_files)):
    cmd_rename_crs = ' '.join([beg_cmd_rename_crs, in_files[i]])
    os.system(cmd_rename_crs)
    
beg_cmd_rename_grid = ''.join(['ncrename -a ', var_name, '@grid_mapping,grid_mapping_name '])
for i in range(len(in_files)):
    cmd_rename_grid = ' '.join([beg_cmd_rename_grid, in_files[i]])
    os.system(cmd_rename_grid)

# Create and call aggregate command
beg_cmd_agg = ''.join(['cdo ', remap_met, ',', grid_file]) # Beginning of command

for i in range(len(in_files)):
    cmd_agg = ' '.join([beg_cmd_agg, in_files[i], out_files[i]])
    os.system(cmd_agg)
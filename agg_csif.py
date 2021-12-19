import os
from glob import glob

in_res    = '0.05'
out_res   = '0.20'
in_path   = '/mnt/g/CSIF/8-day/inst'
out_path  = '/mnt/g/CSIF/8-day/1deg/inst'
grid_file = 'gridfile_0.20.txt'
remap_met = 'remapcon'

# Create output dirs
in_dirs   = [f.path for f in os.scandir(in_path) if f.is_dir()] # Get subdirectories of input dir
out_dirs  = [x.replace(in_path, out_path) for x in in_dirs]    # List of output dirs

for d in out_dirs:
    if not os.path.exists(d):
        os.makedirs(d)

# Create output filenames    
in_files  = [y for x in os.walk(in_path) for y in glob(os.path.join(x[0], '*.nc'))]
out_files = [x.replace(in_path, out_path).replace(in_res, out_res) for x in in_files]

beg_cmd = ''.join(['cdo ', remap_met, ',', grid_file]) # Beginning of command

for i in range(len(in_files)):
    cmd = ' '.join([beg_cmd, in_files[i], out_files[i]])
    os.system(cmd)

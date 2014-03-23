#!/usr/bin/python

##
#  This file converts a NetCDF file into R or matlab file formats.The output 
#  type is chosen based on the file extension. Do not use this script with a
#  large NetCDF file, your computer will crash when the entire file is read 
#  into memory.
#   {*.mat: MATLAB,
#    *.Rda: R} # Rdata is in development
#   
#  Usage: ./convert_format.py input_file.nc output_file.*
#
#  Please notify btjones@mit.edu with any issues.

## Checks that this required packages are available and aborts if not.
#  Please be careful adding dependencies, exec() does not perform any safety
#  checking before executing.
def check_deps(func):
    # Dictionary that list the package dependencies for each 
    deps = {'all': ['netCDF4', 'os', 'sys'],
            'mat': ['scipy.io']}
    if(not func in deps):
        print('Functionality %s is not supported.' % str(func))
        exit(-1)
    for pkg in deps[func]:
        try:
            exec('import %s' % (pkg, ), globals())
        except:
            print('Functionality %s requires package %s.' % (str(func), pkg,))
            exit(-1)

## Read the NetCDF file and return a dictionary with it's contents.
def read_ncdf(input_file):
    try:
        ncid = netCDF4.Dataset(input_file)
    except:
        print('Could not open input file %s' % input_file)
        exit(-1)
    data = {}
    for var in ncid.variables:
        data[var] = ncid.variables[var][:]
    ncid.close()
    return(data)

## Wrapper for format specific write functions
def write_output(data, output_file):
    fns = {'.mat': 'write_mat',
           '.Rda': 'write_rda'}
    fname, fext = os.path.splitext(output_file)
    check_deps(fext[1:])
    eval('%s(data, output_file)' % (fns[fext]))

## Write the data to the output MATLAB file.
def write_mat(data, output_file):
    scipy.io.savemat(output_file, data)

## Write the data to the output Rdata file.
def write_rda(data, output_file):
    print("I'm sorry, this is still in development\n")

## Main routine
def main():
    check_deps('all')
    if(len(sys.argv) != 3):
        print('Wrong usage. Try ./convert_format.py input output')
        exit(-1)
    data = read_ncdf(sys.argv[1])
    write_output(data, sys.argv[2])

if __name__ == '__main__':
    main()

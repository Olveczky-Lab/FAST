%% load Data
f = hdf5read('Z:\\badlands\\bintprog.h5','/f');
A = hdf5read('Z:\\badlands\\bintprog.h5','/A');
b = hdf5read('Z:\\badlands\\bintprog.h5','/b');
%% LP
x = bintprog(-f,A',b);
%% Save output
hdf5write('Z:\\badlands\\bintprogresult.h5','/x',uint8(x));
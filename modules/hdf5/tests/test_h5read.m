%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('h5read'), 2);
assert_isequal(nargout('h5read'), 1);
%=============================================================================
h5_directory = [modulepath('hdf5', 'tests'), '/h5'];
%=============================================================================
double_data = [h5_directory, '/h5ex_t_float.h5'];
R = h5read(double_data,'/DS1');
assert_isequal(class(R), 'double');
assert_isequal(size(R), [4 7]);
REF = [ 0    1.0000    2.0000    3.0000    4.0000    5.0000    6.0000;
2.0000    1.6667    2.4000    3.2857    4.2222    5.1818    6.1538;
4.0000    2.3333    2.8000    3.5714    4.4444    5.3636    6.3077;
6.0000    3.0000    3.2000    3.8571    4.6667    5.5455    6.4615];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
integer_data = [h5_directory, '/h5ex_t_int.h5'];
R = h5read(integer_data,'/DS1');
REF = int64([0    -1    -2    -3    -4    -5    -6;
0     0     0     0     0     0     0;
0     1     2     3     4     5     6;
0     2     4     6     8    10    12]);
assert_isequal(R, REF);
%=============================================================================
opaque_data = [h5_directory, '/h5ex_t_opaque.h5'];
R = h5read(opaque_data, '/DS1');
assert_isequal(class(R), 'cell');
assert_isequal(size(R), [4 1]);
REF_1 = uint8([79 ; 80 ; 65 ; 81 ; 85 ; 69 ; 49 ]);
REF_2 = uint8([79 ; 80 ; 65 ; 81 ; 85 ; 69 ; 50 ]);
REF_3 = uint8([79 ; 80 ; 65 ; 81 ; 85 ; 69 ; 51 ]);
REF_4 = uint8([79 ; 80 ; 65 ; 81 ; 85 ; 69 ; 52 ]);
REF = {REF_1 ; REF_2 ; REF_3 ; REF_4};
assert_isequal(R, REF);
%=============================================================================
bitfield_data = [h5_directory, '/h5ex_t_bit.h5'];
R = h5read(bitfield_data, '/DS1');
REF = uint8([     0    80   160   240     0    80   160;
68   148   228    52    68   148   228;
136   217    42   123   136   217    42;
204    30   108   190   204    30   108]);
assert_isequal(R, REF);
%=============================================================================
string_data = [h5_directory, '/h5ex_t_string.h5'];
R = h5read(string_data, '/DS1');
REF = {'Parting'; 'is such'; 'sweet  '; 'sorrow.'};
assert_isequal(R, REF);
%=============================================================================
vlstring_data = [h5_directory, '/h5ex_t_vlstring.h5'];
R = h5read(vlstring_data, '/DS1');
REF = {'Parting'; 'is such'; 'sweet  '; 'sorrow.'};
assert_isequal(R, REF);
%=============================================================================
enum_data = [h5_directory, '/h5ex_t_enum.h5'];
R = h5read(enum_data, '/DS1');
REF = {'SOLID', 'SOLID' , 'SOLID', 'SOLID' , 'SOLID', 'SOLID' , 'SOLID';
'SOLID', 'LIQUID', 'GAS'  , 'PLASMA', 'SOLID', 'LIQUID', 'GAS';
'SOLID', 'GAS'   , 'SOLID', 'GAS'   , 'SOLID', 'GAS'   , 'SOLID';
'SOLID', 'PLASMA', 'GAS'  , 'LIQUID', 'SOLID', 'PLASMA', 'GAS'};
assert_isequal(R, REF);
%=============================================================================
compound_data = [h5_directory, '/h5ex_t_cmpd.h5'];
R = h5read(compound_data, '/DS1');
assert_istrue(isstruct(R));
assert_isequal(fieldnames(R), {'serial_no'; 'location'; 'temperature'; 'pressure'});
REF_serial_no = int32([ 1153;   1184;   1027;   1313]);
assert_isequal(R.serial_no, REF_serial_no);
REF_location = {'Exterior (static)';
'Intake';
'Intake manifold';
'Exhaust manifold' };
assert_isequal(R.location, REF_location);
REF_temperature = double([53.2300;
55.1200;
130.5500;
1252.8900]);
assert_isequal(R.temperature, REF_temperature);
REF_pressure = double([24.5700;
22.9500;
31.2300;
84.1100]);
assert_isequal(R.pressure, REF_pressure);
%=============================================================================
vlen_data = [h5_directory, '/h5ex_t_vlen.h5'];
R = h5read(vlen_data, '/DS1');
REF_1 = int32([3;2;1]);
REF_2 = int32([ 1; 1;  2; 3;  5;  8;  13;  21;  34;  55;  89;  144]);
REF = {REF_1; REF_2};
assert_isequal(R, REF);
%=============================================================================
array_data = [h5_directory, '/h5ex_t_array.h5'];
R = h5read(array_data,'/DS1');
assert_isequal(size(R), [3 5 4]);
assert_isequal(class(R), 'int64');
%=============================================================================
REF_1 = int64([
0  0  0  0  0  ;
0  -1  -2  -3  -4;
0  -2  -4  -6  -8]);
assert_isequal(R(:,:,1), REF_1);
%=============================================================================
REF_2 = int64([
0  1  2  3  4  ;
1  1  1  1  1  ;
2  1  0  -1  -2]);
assert_isequal(R(:,:,2), REF_2);
%=============================================================================
REF_3 = int64([
0  2  4  6  8;
2  3  4  5  6;
4  4  4  4  4]);
assert_isequal(R(:,:,3), REF_3);
%=============================================================================
REF_4 = int64([
0  3  6  9  12;
3  5  7  9  11;
6  7  8  9  10]);
assert_isequal(R(:,:,4), REF_4);
%=============================================================================
reference_data = [h5_directory, '/h5ex_t_regref.h5'];
R = h5read(reference_data, '/DS1');
assert_istrue(iscell(R));
REF_1 = int8([104; 100; 102; 53]);
REF_2 = int8([84;  116;  104;  104;  101;  101;  114;  100;  111;  111;  119;  103]);
REF = {REF_1; REF_2};
assert_isequal(R, REF);
%=============================================================================

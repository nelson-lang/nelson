%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
fscanf_1 = [modulepath('stream_manager', 'tests'), '/fscanf_1.txt'];
FD = fopen(fscanf_1, 'r');
R = fscanf(FD, '%d');
fclose(FD);
REF = (1:9)';
assert_isequal(R, REF);
%=============================================================================
fscanf_1 = [modulepath('stream_manager', 'tests'), '/fscanf_1.txt'];
FD = fopen(fscanf_1, 'r');
R = fscanf(FD, '%s');
fclose(FD);
REF = '123456789';
assert_isequal(R, REF);
%=============================================================================
fscanf_1 = [modulepath('stream_manager', 'tests'), '/fscanf_1.txt'];
FD = fopen(fscanf_1, 'r');
R = fscanf(FD, '%o');
fclose(FD);
REF = (1:7)';
assert_isequal(R, REF);
%=============================================================================
fscanf_1 = [modulepath('stream_manager', 'tests'), '/fscanf_1.txt'];
FD = fopen(fscanf_1, 'r');
R = fscanf(FD, '%ld');
fclose(FD);
REF = int64((1:9)');
assert_isequal(R, REF);
%=============================================================================
fscanf_1 = [modulepath('stream_manager','tests'), '/fscanf_1.txt'];
FD = fopen(fscanf_1, 'r');
R = fscanf(FD, '%lu');
fclose(FD);
REF = uint64((1:9)');
assert_isequal(R, REF);
%=============================================================================
fscanf_2 = [modulepath('stream_manager', 'tests'), '/fscanf_2.txt'];
FD = fopen(fscanf_2, 'r');
R = fscanf(FD, '%g %g');
fclose(FD);
REF = [0;
1.0000;
0.1000;
1.1052;
0.2000;
2.1052;
1.0000;
3.7183];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
fscanf_2 = [modulepath('stream_manager', 'tests'), '/fscanf_2.txt'];
FD = fopen(fscanf_2, 'r');
R = fscanf(FD, '%g %g');
fclose(FD);
REF = [0;
1.0000;
0.1000;
1.1052;
0.2000;
2.1052;
1.0000;
3.7183];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
fscanf_2 = [modulepath('stream_manager', 'tests'), '/fscanf_2.txt'];
FD = fopen(fscanf_2, 'r');
R = fscanf(FD,'%g %g',[2 inf]);
fclose(FD);
REF = [0.0000      0.1000      0.2000      1.0000;
1.0000      1.1052      2.1052      3.7183];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
fscanf_2 = [modulepath('stream_manager', 'tests'), '/fscanf_2.txt'];
FD = fopen(fscanf_2, 'r');
[R, C] = fscanf(FD,'%g %g',[2 inf]);
fclose(FD);
REF = [0.0000      0.1000      0.2000      1.0000;
1.0000      1.1052      2.1052      3.7183];
assert_isapprox(R, REF, 1e-4);
assert_isapprox(C, 8);
%=============================================================================
fscanf_3 = [modulepath('stream_manager', 'tests'), '/fscanf_3.txt'];
FD = fopen(fscanf_3, 'r');
R = fscanf(FD,'%s');
fclose(FD);
REF = '1titi34tata67tutu9';
assert_isequal(R, REF);
%=============================================================================
fscanf_3 = [modulepath('stream_manager', 'tests'), '/fscanf_3.txt'];
FD = fopen(fscanf_3, 'r');
R = fscanf(FD,'%d %s %d');
fclose(FD);
REF = [1; 116; 105; 116; 105; 3; 4; 116; 97; 116; 97; 6; 7; 116; 117; 116; 117; 9];
assert_isequal(R, REF);
%=============================================================================
fscanf_3 = [modulepath('stream_manager', 'tests'), '/fscanf_3.txt'];
FD = fopen(fscanf_3, 'r');
[R, C] = fscanf(FD,'%d %s %d');
fclose(FD);
REF = [1; 116; 105; 116; 105; 3; 4; 116; 97; 116; 97; 6; 7; 116; 117; 116; 117; 9];
assert_isequal(R, REF);
assert_isequal(C, 9);
%=============================================================================
fscanf_4 = [modulepath('stream_manager', 'tests'), '/fscanf_4.txt'];
FD = fopen(fscanf_4, 'r');
R = fscanf(FD,'%s');
fclose(FD);
REF = 'tititatatutu';
assert_isequal(R, REF);
%=============================================================================
fscanf_5 = [modulepath('stream_manager', 'tests'), '/fscanf_5.txt'];
FD = fopen(fscanf_5, 'r');
R = fscanf(FD,'%f');
fclose(FD);
REF = [1; NaN; 3; Inf];
assert_isequal(R, REF);
%=============================================================================
fscanf_6 = [modulepath('stream_manager', 'tests'), '/fscanf_6.txt'];
FD = fopen(fscanf_6, 'r');
R = fscanf(FD,'%c');
fclose(FD);
R = replace(R, char([13 10]), char(10));
REF = ['4i', char(10)];
assert_isequal(R, REF);
%=============================================================================
fscanf_6 = [modulepath('stream_manager', 'tests'), '/fscanf_6.txt'];
FD = fopen(fscanf_6, 'r');
R = fscanf(FD,'%s');
fclose(FD);
REF = '4i';
assert_isequal(R, REF);
%=============================================================================
fscanf_6 = [modulepath('stream_manager', 'tests'), '/fscanf_6.txt'];
FD = fopen(fscanf_6, 'r');
[R, C] = fscanf(FD,'%s');
fclose(FD);
REF = '4i';
assert_isequal(R, REF);
assert_isequal(C, 1);
%=============================================================================
fscanf_7 = [modulepath('stream_manager', 'tests'), '/fscanf_7.txt'];
FD = fopen(fscanf_7, 'r', 'n', 'windows-1251');
[R, C] = fscanf(FD,'%s %s %s');
fclose(FD);
assert_isequal(R, 'Тойисамнезнаекогаероденно');
assert_isequal(C, 9);
%=============================================================================
M = [1 2 3; 4 5 6];
fw = fopen([tempdir, 'test_fscanf.txt'], 'wt');
fprintf(fw, "%d", M);
fclose(fw);
fd = fopen([tempdir, 'test_fscanf.txt'], 'r');
R = fscanf(fd, "%d");
fclose(fd);
assert_isequal(R, 142536);
%=============================================================================
M = [1 2 3; 4 5 6];
dst = [tempdir, 'test_fscanf.txt'];
fw = fopen(dst, 'wt');
fprintf(fw, "%d %d %d\n", M);
fclose(fw);
fd = fopen(dst, 'r');
R = fscanf(fd, "%d");
fclose(fd);
REF = [1; 4; 2; 5; 3; 6];
assert_isequal(R, REF);
%=============================================================================
M = [pi 2*pi 3*pi; 4*pi 5*pi 6*pi];
dst = [tempdir, 'test_fscanf.txt'];
fw = fopen(dst, 'wt');
fprintf(fw, "%f %f %f\n", M);
fclose(fw);
fd = fopen(dst, 'r');
R = fscanf(fd, "%f");
fclose(fd);
REF = [pi; 4*pi; 2*pi; 5*pi; 3*pi; 6*pi];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
fscanf_2 = [modulepath('stream_manager', 'tests'), '/fscanf_2.txt'];
FD = fopen(fscanf_2, 'r');
[R, C] = fscanf(FD,'%g %g',[20 30]);
fclose(FD);
REF = [0.0000;
1.0000;
0.1000;
1.1052;
0.2000;
2.1052;
1.0000;
3.7183];
assert_isapprox(R, REF, 1e-4);
assert_isapprox(C, 8);
%=============================================================================

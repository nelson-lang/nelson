%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [8     1     6;3     5     7;4     9     2];
B = [15; 15; 15];
R1 = A \ B;
R2 = mldivide(A, B);
assert_isequal(R1, R2);
REF = [ 1.0000;1.0000;1.0000];
assert_isapprox(R1, REF, 1e-4);
%=============================================================================
A = [8     1     6;3     5     7;4     9     2];
B = [15; 15; 15] * i;
R = A \ B;
REF = [complex(0,1); complex(0,1); complex(0,1)];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [ 16     2     3    13;5    11    10     8;9     7     6    12;4    14    15     1];
B = [34; 34; 34; 34] * i;
R = A \ B;
REF = [complex(0,1); complex(0,1); complex(0,1); complex(0,1)];
assert_isapprox(R, REF, 1e-4);
W = lastwarn();
assert_istrue(startsWith(W, _('Matrix is singular to working precision:')));
%=============================================================================
A = [1 0; 0 0];
B = [1; 1];
R = A \ B;
REF = [1;0];
assert_isapprox(R, REF, 1e-4);
W = lastwarn();
assert_istrue(startsWith(W, _('Matrix is singular to working precision:')));
%=============================================================================
A = [1 2 0; 0 4 3];
B = [8; 18];
R = A \ B;
REF = [0.0000;4.0000;0.6667];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [1 2 0; 0 4 3];
B = single([8; 18]);
R = A \ B;
REF = single([0.0000;4.0000;0.6667]);
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = single([1 2 0; 0 4 3]);
B = [8; 18];
R = A \ B;
REF = single([0.0000;4.0000;0.6667]);
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [17    24     1     8    15;
23     5     7    14    16;
4     6    13    20    22;
10    12    19    21     3;
11    18    25     2     9];
A(:,1) = zeros(1,5);
B = [1;2;5;7;7];
R = A \ B;
REF = [0.0000;0.0209;0.2717;0.0808;-0.0321];
assert_isapprox(R, REF, 1e-3);
W = lastwarn();
assert_istrue(startsWith(W, _('Matrix is singular to working precision:')));
%=============================================================================
A = [1 0 0;1 0 0];
B = [1; 2];
R = A \ B;
REF = [1.5; 0; 0];
assert_isapprox(R, REF, 1e-4);
W = lastwarn();
assert_istrue(startsWith(W, _('Matrix is rank deficient to machine precision:')));
%=============================================================================
A = [0.68  0.597;
-0.211  0.823;
0.566 -0.605];
B = [ -0.33; 0.536;-0.444];
R = A \ B;
REF = [-0.669988554736702;0.313593694351637];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [1. 83.0 234289 2356 1590 107608 1947;
1. 88.5 259426 2325 1456 108632 1948;
1. 88.2 258054 3682 1616 109773 1949;
1. 89.5 284599 3351 1650 110929 1950;
1. 96.2 328975 2099 3099 112075 1951;
1. 98.1 346999 1932 3594 113270 1952;
1. 99.0 365385 1870 3547 115094 1953;
1. 100.0 363112 3578 3350 116219 1954;
1. 101.2 397469 2904 3048 117388 1955;
1. 104.6 419180 2822 2857 118734 1956;
1. 108.4 442769 2936 2798 120445 1957;
1. 110.8 444546 4681 2637 121950 1958;
1. 112.6 482704 3813 2552 123366 1959;
1. 114.2 502601 3931 2514 125368 1960;
1. 115.7 518173 4806 2572 127852 1961;
1. 116.9 554894 4007 2827 130081 1962];
B = [60323;
61122;
60171;
61187;
63221;
63639;
64989;
63761;
66019;
67857;
68169;
66513;
68655;
69564;
69331;
70551];
R = A\B;
REF = [-3482258.634598;
15.061872;
-0.035819;
-2.020230;
-1.033227;
-0.051104;
1829.151465];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = [81.       -3240.       41580.     -249480.     810810.    -1513512.    1621620.   -926640.     218790. ;
-3240.      172800.    -2494800.    15966720.  -54054000.   1.038D+08  -1.135D+08   65894400.  -15752880.;
41580.    -2494800.    38419920.  -2.561D+08   8.919D+08  -1.748D+09   1.942D+09  -1.142D+09   2.757D+08;
-249480.    15966720.  -2.561D+08   1.756D+09  -6.243D+09   1.243D+10  -1.398D+10   8.303D+09  -2.022D+09;
810810.   -54054000.   8.919D+08  -6.243D+09   2.255D+10  -4.545D+10   5.165D+10  -3.092D+10   7.581D+09;
-1513512.   1.038D+08  -1.748D+09   1.243D+10  -4.545D+10   9.255D+10  -1.061D+11   6.393D+10  -1.577D+10;
1621620.  -1.135D+08   1.942D+09  -1.398D+10   5.165D+10  -1.061D+11   1.224D+11  -7.421D+10   1.840D+10;
-926640.    65894400.  -1.142D+09   8.303D+09  -3.092D+10   6.393D+10  -7.421D+10   4.523D+10  -1.126D+10;
218790.   -15752880.   2.757D+08  -2.022D+09   7.581D+09  -1.577D+10   1.840D+10  -1.126D+10   2.816D+09];
B = ones(9, 1);
R = A\B;
REF = [ 0.287317116918036;
0.035547470961149;
0.004862734316149;
0.000489140981213;
0.000003207227934;
0.000006804956596;
0.000033320519171;
0.000071700905960;
0.000150126407777];
assert_isapprox(R, REF, 1e-8);
%=============================================================================
A = complex(1, 2);
B = [2, 4, 6];
R = A \ B;
REF  = [complex(0.4, -0.8), complex(0.8, -1.6), complex(1.2, -2.4)];
assert_isapprox(R, REF, 1e-8);
%=============================================================================
B = 2;
A = [1;2;3];
R = (A'\B')';
REF = [0.0000      0.0000      0.6667];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
A = ones(3, 0);
B = [15; 15; 15];
R = A \ B;
REF = zeros(0, 1);
assert_isequal(R, REF);
%=============================================================================
A = [8     1     6;3     5     7; 4     9     2];
B = [1;2;3];
R=A \ B;
REF = [0.050000000000000;
0.300000000000000;
0.050000000000000];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = [1; 4] \ [2; 4];
REF = 1.058823529411765;
assert_isapprox(R, REF, 1e-8);
%=============================================================================
R = [6 2; 4 1] \ [4 7;8 5];
REF = [  6.0000      1.5000
-16.0000     -1.0000];
assert_isapprox(R, REF, 1e-5);
%=============================================================================
R =  [6 2; 4 1] \ [4 7;8 5];
REF = [6.0000      1.5000; -16.0000     -1.0000];
assert_isapprox(R, REF, 1e-5);
%=============================================================================
R = [3i 4i 6i+4] \ [5 9i-4 8];
REF = [ 0.0000 + 0.0000i     0.0000 + 0.0000i     0.0000 + 0.0000i;
0.0000 + 0.0000i     0.0000 + 0.0000i     0.0000 + 0.0000i;
0.3846 - 0.5769i     0.7308 + 1.1538i     0.6154 - 0.9231i];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
assert_checkerror('R = [1; 4] \ [2 4];', _('Requested divide operation requires arguments to have correct dimensions.'));
%=============================================================================
R = 2 \ [1 2 3];
REF = [0.5   1.   1.5];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = 2 \ [1; 2; 3];
REF = [0.5;   1.;   1.5];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = 2 \ [1 2; 6 3];
REF = [0.5000      1.0000;    3.0000      1.5000];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = [1 2 5 3] \ 2;
REF = [0;0;0.400000000000000;0];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = [1 2 5 3]\[7 4 2 9];
REF = [ 0                   0                   0                   0;
0                   0                   0                   0;
1.400000000000000   0.800000000000000   0.400000000000000   1.800000000000000;
0                   0                   0                   0];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = [1; 4] \ [2; 4];
REF =  1.058823529411765;
assert_isapprox(R, REF, 1e-8);
%=============================================================================
R = [6; 2] \ [7 5; 8 3];
REF = [1.450000000000000   0.900000000000000];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = [6 2; 4 1] \ [4; 9];
REF = [7 ; -19];
assert_isapprox(R, REF, 1e-4);
%=============================================================================
R = [6 2; 4 1] \ [4 7;8 5];
REF = [6 1.5; -16 -1];
assert_isapprox(R, REF, 1e-4);
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('readmatrix'), 1);
assert_isequal(nargout('readmatrix'), 1);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readmatrix_1.csv'];
R = readmatrix(csv_filename);
REF = [     6     8     3     1;
5     4     7     3;
1     6     7    10;
4     2     8     2;
2     7     5     9];
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readmatrix_2.csv'];
R = readmatrix(csv_filename);
REF = [   6.0000 + 0.0000i   8.0000 + 0.0000i   3.0000 + 0.0000i   1.0000 + 0.0000i;
5.0000 + 0.0000i   4.0000 + 0.0000i   7.0000 + 0.0000i   3.0000 + 0.0000i;
1.0000 + 0.0000i   6.0000 + 0.0000i   7.0000 + 0.0000i  10.0000 + 0.0000i;
4.0000 + 0.0000i   2.4000 + 5.6000i   8.0000 + 0.0000i   2.0000 + 0.0000i;
2.0000 + 0.0000i   7.0000 + 0.0000i   5.0000 + 0.0000i   9.0000 + 0.0000i];
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readmatrix_3.csv'];
R = readmatrix(csv_filename);
REF = [NaN    38    71   176   124    93;
  NaN    43    69   163   109    77;
  NaN    38    64   131   125    83;
  NaN    40    67   133   117    75;
  NaN    49    64   119   122    80];
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readmatrix_4.csv'];
R = readmatrix(csv_filename);
REF =  [complex(NaN, 0)   0.3800 + 0.0000i   0.7100 + 0.0000i   1.7600 + 0.0000i   1.2400 + 0.0000i   0.9300 + 0.0000i;
complex(NaN, 0)   0.4300 + 0.0000i   0.6900 + 0.0000i   1.6300 + 0.0000i   1.0900 + 0.0000i   0.7700 + 0.0000i;
complex(NaN, 0)   0.3800 + 0.0000i   0.6400 + 0.0000i   1.3100 + 0.0000i   1.2500 + 0.0000i   0.8300 + 0.0000i;
complex(NaN, 0)   0.4000 + 0.0000i   0.6700 + 0.0000i   1.3300 + 0.0000i   1.1700 + 0.0000i   0.7500 + 0.0000i;
complex(NaN, 0)   0.4900 + 0.0000i   0.6400 + 0.0000i   0.0000 + 1.1900i   1.2200 + 0.0000i   0.8000 + 0.0000i];
assert_isequal(real(R)/ 1.0e+02 , real(REF));
assert_isequal(imag(R)/ 1.0e+02 , imag(REF));
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readmatrix_5.csv'];
R = readmatrix(csv_filename);
REF = [NaN    38    71   176   124    93;
NaN    43    69   163   109    77;
NaN    38    64   131   125    83;
NaN    40    67   133   117    75;
NaN    49    64   119   122    80];
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readmatrix_6.csv'];
R = readmatrix(csv_filename);
REF = [ NaN     1   NaN;
-Inf   Inf     4];
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readmatrix_7.csv'];
R = readmatrix(csv_filename);
REF = [    complex(NaN,0)   0.0000 + 0.0013i      complex(NaN,0)
complex(-Inf, 0)      complex(Inf,0)  -4.0000 + 0.0000i] * 1e3;
assert_isequal(real(R), real(REF));
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readmatrix_3.csv'];
options = detectImportOptions(csv_filename);
options.DataLines = [3 5];
R = readmatrix(csv_filename, options);
REF = [NaN    43    69   163   109    77;
NaN    38    64   131   125    83;
NaN    40    67   133   117    75];
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readmatrix_3.csv'];
options = detectImportOptions(csv_filename);
options.DataLines = [3 5];
R = readmatrix(csv_filename, options, 'OutputType', 'char');
REF = {'Johnson', '43', '69', '163', '109', '77';
'Williams', '38', '64', '131', '125', '83';
'Jones', '40', '67', '133', '117', '75'};
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readmatrix_3.csv'];
options = detectImportOptions(csv_filename);
options.DataLines = [3 5];
R = readmatrix(csv_filename, options, 'OutputType', 'string');
REF = ["Johnson"    "43"    "69"    "163"    "109"    "77";
"Williams"    "38"    "64"    "131"    "125"    "83";
"Jones"    "40"    "67"    "133"    "117"    "75"];
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readmatrix_3.csv'];
options = detectImportOptions(csv_filename);
options.DataLines = [3 5];
R = readmatrix(csv_filename, options, 'OutputType', 'int8');
REF = [   0   43   69   127   109   77;
0   38   64   127   125   83;
0   40   67   127   117   75];
REF = int8(REF);
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readmatrix_6.csv'];
R = readmatrix(csv_filename, 'OutputType', 'int8');
REF =  [ 0      1      0;
-128    127      4];
REF = int8(REF);
assert_isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/readmatrix_6.csv'];
R = readmatrix(csv_filename, 'OutputType', 'uint8');
REF = [0     1     0;
0   255     4];
REF = uint8(REF);
assert_isequal(R, REF);
%=============================================================================

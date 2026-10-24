%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('csvread'), -1);
asserts.isequal(nargout('csvread'), -1);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/dlmread_comma_1.csv'];
R = csvread(csv_filename);
REF = [     1     2     3     4     5;
6     7     8     9    10;
11    -12    13    14    15];
asserts.isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/dlmread_comma_1.csv'];
R = csvread(csv_filename, 1, 0);
REF = [   6     7     8     9    10;
11   -12    13    14    15];
asserts.isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/dlmread_comma_1.csv'];
R = csvread(csv_filename, 1,0,[1,0,2,2]);
REF = [6     7     8;
11   -12    13];
asserts.isequal(R, REF);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/dlmread_precision_space.csv'];
msg = _('Unable to parse numeric value at row %d, column %d.');
msg = msg(1:strfind(msg, 'row %d') - 1);
try
  R = csvread(csv_filename);
catch e
    asserts.istrue(startsWith(e.message, msg));
end
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempname(), '.csv'];
M = magic(5);
writematrix(M, filename);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writematrix_1.csv'];
REF =  fileread(csv_ref_filename);
asserts.isequal(R, REF);
%=============================================================================
filename = [tempname(), '.csv'];
M = magic(5);
writematrix(M, filename, 'Delimiter','tab');
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writematrix_2.csv'];
REF =  fileread(csv_ref_filename);
asserts.isequal(R, REF);
%=============================================================================
filename = [tempname(), '.csv'];
M = [1, 2+pi*i, 3; 4-2*pi*i, 5, 6];
writematrix(M, filename, 'QuoteStrings', 'all');
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writematrix_3.csv'];
REF =  fileread(csv_ref_filename);
asserts.isequal(R, REF);
%=============================================================================
filename = [tempname(), '.csv'];
M = [Inf, -Inf, NaN; eps, realmax, realmin];
writematrix(M, filename);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writematrix_4.csv'];
REF =  fileread(csv_ref_filename);
asserts.isequal(R, REF);
%=============================================================================
filename = [tempname(), '.csv'];
M = [];
writematrix(M, filename);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writematrix_5.csv'];
REF =  fileread(csv_ref_filename);
asserts.isequal(R, REF);
%=============================================================================
filename = [tempname(), '.csv'];
M = magic(6);
writematrix(M, filename, 'QuoteStrings', 'all');
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writematrix_6.csv'];
REF =  fileread(csv_ref_filename);
asserts.isequal(R, REF);
%=============================================================================

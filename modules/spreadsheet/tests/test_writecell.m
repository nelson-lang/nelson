%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
C = {'Name', 'Age', 'City'; 'Alice', 25, 'New York'; 'Bob', 30, 'Los Angeles'};
filename = [tempdir(), 'test_writecell_1.csv'];
writecell(C, filename);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writecell_1.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
C = {'ID', 'Product', 'Price'; 1, 'Laptop', 799.99; 2, 'Phone', 699.49; 3, 'Tablet', 499.00};
filename = [tempdir(), 'test_writecell_2.csv'];
writecell(C, filename);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writecell_2.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
C = {'A', 'B', 'C'; 1, 2, 3; 4, 5, 6};
filename = [tempdir(), 'test_writecell_3.csv'];
writecell(C, filename, 'Delimiter', '\t');
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writecell_3.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
C = {'Name', 'Score', 'Remarks'; 'Alice', 85, 'Good'; 'Bob', [], 'Pending'; 'Charlie', 95, ''};
filename = [tempdir(), 'test_writecell_4.csv'];
writecell(C, filename);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writecell_4.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
C = {
    'ID', 'Task', 'Completed', 'Notes';
    101, 'Task A', true, 'All good';
    102, 'Task B', false, NaN;
    103, 'Task C', true, Inf;
    104, 'Task D', false, -Inf
};
filename = [tempdir(), 'test_writecell_5.csv'];
writecell(C, filename);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writecell_5.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
C = {
    'ID', 'Task', 'Completed', 'Notes', 'Value';
    101, 'Task A', true, 'All good', 3 + 4i;
    102, 'Task B', false, NaN, -2 - 5i;
    103, 'Task C', true, Inf, 1 + 0i;
    104, 'Task D', false, -Inf, NaN;
    105, 'Task E', false, complex(-Inf, NaN), complex(NaN, -Inf);
};
filename = [tempdir(), 'test_writecell_6.csv'];
writecell(C, filename);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writecell_6.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R, REF);
%=============================================================================
C = {
    'Name', 'Symbol', 'Unicode';
    'José', '©', '🌍';
    'Übung', '®', '🚀';
    'こんにちは', '§', '🌈'
};
filename = [tempdir(), 'test_writecell_7.csv'];
writecell(C, filename);
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writecell_7.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R,REF);
%=============================================================================
C = {
    'Name', 'Quote Test', 'Delimiter Test';
    'John "The Great"', 'with, comma', 'semicolon;test';
    'Jane', 'another, quote', 'another; semicolon'
};
filename = [tempdir(), 'test_writecell_8.csv'];
writecell(C, filename, 'Delimiter', ';', 'QuoteStrings', 'all');
R = fileread(filename);
csv_ref_filename = [modulepath('spreadsheet'), '/tests/writecell_8.csv'];
REF =  fileread(csv_ref_filename);
assert_isequal(R,REF);
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite((strcmp(getenv('NELSON_PORTABLE_TEST_RUN'), '1') || iswasm()), ...
  'Delimited text table variables import is not stable in the portable runtime yet.');
%=============================================================================
asserts.isequal(nargin('readvars'), -1);
asserts.isequal(nargout('readvars'), -1);
%=============================================================================
csv_filename = [modulepath('spreadsheet'), '/tests/test_readtable_1.csv'];
%=============================================================================
% Each column is returned as a separate variable.
[LastName, Age, Smoker, Height, Weight, BP1, BP2] = readvars(csv_filename);
asserts.isequal(class(LastName), 'cell');
asserts.isequal(LastName, {'Sanchez'; 'Johnson'; 'Li'; 'Diaz'; 'Brown'});
asserts.isequal(class(Age), 'double');
asserts.isequal(Age, [38; 43; 38; 40; 49]);
asserts.isequal(Smoker, [1; 0; 1; 0; 1]);
asserts.isequal(Height, [71; 69; 64; 67; 64]);
asserts.isequal(Weight, [176; 163; 131; 133; 119]);
asserts.isequal(BP1, [124; 109; 125; 117; 122]);
asserts.isequal(BP2, [93; 77; 83; 75; 80]);
%=============================================================================
% A single output returns only the first column.
first = readvars(csv_filename);
asserts.isequal(first, {'Sanchez'; 'Johnson'; 'Li'; 'Diaz'; 'Brown'});
%=============================================================================
% Fewer outputs than columns returns only the first columns.
[c1, c2] = readvars(csv_filename);
asserts.isequal(c1, {'Sanchez'; 'Johnson'; 'Li'; 'Diaz'; 'Brown'});
asserts.isequal(c2, [38; 43; 38; 40; 49]);
%=============================================================================
% Requesting more outputs than columns raises an error.
asserts.checkerror('[a, b, c, d, e, f, g, h] = readvars(csv_filename);', _('Too many output arguments.'));
%=============================================================================
% Import options object is forwarded to readtable.
opts = detectImportOptions(csv_filename);
[o1, o2] = readvars(csv_filename, opts);
asserts.isequal(o1, {'Sanchez'; 'Johnson'; 'Li'; 'Diaz'; 'Brown'});
asserts.isequal(o2, [38; 43; 38; 40; 49]);
%=============================================================================
% TextType 'string' returns text columns as a string array.
names = readvars(csv_filename, 'TextType', 'string');
asserts.isequal(class(names), 'string');
asserts.isequal(names, ["Sanchez"; "Johnson"; "Li"; "Diaz"; "Brown"]);
%=============================================================================
% TextType 'char' keeps text columns as a cell array of character vectors.
namesChar = readvars(csv_filename, 'TextType', 'char');
asserts.isequal(class(namesChar), 'cell');
asserts.isequal(namesChar, {'Sanchez'; 'Johnson'; 'Li'; 'Diaz'; 'Brown'});
%=============================================================================
% 'Range' restricts the columns and rows returned as variables.
[rn, ra] = readvars(csv_filename, 'Range', 'A1:B3');
asserts.isequal(rn, {'Sanchez'; 'Johnson'});
asserts.isequal(ra, [38; 43]);
%=============================================================================
% 'Range' starting below the header reads every selected row as data.
[c1, c2, c3] = readvars(csv_filename, 'Range', 'A2:C4');
asserts.isequal(c1, {'Sanchez'; 'Johnson'; 'Li'});
asserts.isequal(c2, [38; 43; 38]);
asserts.isequal(c3, [1; 0; 1]);
%=============================================================================
% 'Sheet' is not supported for a delimited text file.
cmd = sprintf('readvars(''%s'', ''Sheet'', 1)', csv_filename);
asserts.checkerror(cmd, _('Unsupported parameter ''Sheet'' for FileType ''text''.'));
%=============================================================================

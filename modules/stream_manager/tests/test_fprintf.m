%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
fprintf('hello');
%=============================================================================
R = fprintf('hello');
REF = 5;
asserts.isequal(R, REF);
%=============================================================================
test_fprintf_file = [tempname(), '.txt'];
fileID = fopen(test_fprintf_file, 'wt');
fprintf(fileID, '%s', 'hello');
fclose(fileID);
R = fileread(test_fprintf_file);
REF = 'hello';
asserts.isequal(R, REF);
%=============================================================================
fileID = fopen(test_fprintf_file, 'wt');
R = fprintf(fileID, '%s %s', 'hello', 'Nelson');
fclose(fileID);
R = fileread(test_fprintf_file);
REF = 'hello Nelson';
asserts.isequal(R, REF);
%=============================================================================
fileID = fopen(test_fprintf_file, 'wt');
R = fprintf(fileID, '%f', -53);
fclose(fileID);
R = fileread(test_fprintf_file);
REF = '-53.000000';
asserts.isequal(R, REF);
%=============================================================================
R = fprintf("hello");
REF = 5;
asserts.isequal(R, REF);
%=============================================================================
fprintf(2, "an value %g.", pi);
%=============================================================================
test_fprintf_encoding_file = [tempname(), '.txt'];
skip_testsuite(iswasm(), ...
  'WebAssembly reduced characters_encoding backend does not provide Windows-1251 conversion.');
fileID = fopen(test_fprintf_encoding_file, 'wt', 'n', 'Windows-1251');
fprintf(fileID, '%s', 'Виртуальная');
fclose(fileID);
F = fopen(test_fprintf_encoding_file, 'rt');
R = fread(F, 'uint8');
fclose(F);
ce = nativecharset(R');
asserts.istrue(iscell(ce));
asserts.istrue(iscellstr(ce));
asserts.istrue(any(contains(ce, 'windows-1251')));
%=============================================================================
R = evalc('fprintf(1,''%d%%.'',50)')
REF = '50%.';
asserts.isequal(R, REF);
rmfile(test_fprintf_file);
rmfile(test_fprintf_encoding_file);
%=============================================================================

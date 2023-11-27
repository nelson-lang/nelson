%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
fprintf('hello');
%=============================================================================
R = fprintf('hello');
REF = 5;
assert_isequal(R, REF);
%=============================================================================
fileID = fopen([tempdir(), 'test_fprintf.txt'], 'wt');
fprintf(fileID, '%s','hello');
fclose(fileID);
R = fileread([tempdir(), 'test_fprintf.txt']);
REF = 'hello';
assert_isequal(R, REF);
%=============================================================================
fileID = fopen([tempdir(), 'test_fprintf.txt'], 'wt');
R = fprintf(fileID, '%s %s', 'hello', 'Nelson');
fclose(fileID);
R = fileread([tempdir(), 'test_fprintf.txt']);
REF = 'hello Nelson';
assert_isequal(R, REF);
%=============================================================================
fileID = fopen([tempdir(), 'test_fprintf.txt'], 'wt');
R = fprintf(fileID, '%f', -53);
fclose(fileID);
R = fileread([tempdir(), 'test_fprintf.txt']);
REF = '-53.000000';
assert_isequal(R, REF);
%=============================================================================
R = fprintf("hello");
REF = 5;
assert_isequal(R, REF);
%=============================================================================
fprintf(2, "an value %g.", pi);
%=============================================================================
fileID = fopen([tempdir(), 'test_fprintf_encoding.txt'], 'wt', 'n', 'Windows-1251');
fprintf(fileID, '%s','Виртуальная');
fclose(fileID);
F = fopen([tempdir(), 'test_fprintf_encoding.txt'], 'rt');
R = fread(F, 'uint8');
fclose(F);
ce = nativecharset(R');
assert_istrue(iscell(ce));
assert_istrue(iscellstr(ce));
assert_istrue(any(contains(ce, 'windows-1251')));
%=============================================================================
R = evalc('fprintf(1,''%d%%.'',50)')
REF = '50%.';
assert_isequal(R, REF);
%=============================================================================
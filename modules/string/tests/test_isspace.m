%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = '123 Main St.';
R = isspace(A);
REF = logical([0 0 0 1 0 0 0 0 1 0 0 0]);
asserts.isequal(R, REF)
%=============================================================================
A = '123 Main St.';
B = [A; A];
R = isspace(B);
REF = logical([0 0 0 1 0 0 0 0 1 0 0 0; 0 0 0 1 0 0 0 0 1 0 0 0]);
asserts.isequal(R, REF)
%=============================================================================
A = "123 Main St.";
R = isspace(A);
REF = logical([0 0 0 1 0 0 0 0 1 0 0 0]);
asserts.isequal(R, REF)
%=============================================================================
A = '可口可乐; 可口可樂';
R = isspace(A);
REF = logical([0 0 0 0 0 1 0 0 0 0]);
asserts.isequal(R, REF)
%=============================================================================
% full whitespace set: tab (9), line feed (10), vertical tab (11),
% form feed (12), carriage return (13) and space (32) are whitespace
A = char([97 9 98 10 32 99 13 11 12 100]);
R = isspace(A);
REF = logical([0 1 0 1 1 0 1 1 1 0]);
asserts.isequal(R, REF)
%=============================================================================
A = sprintf('a\tb c');
R = isspace(A);
REF = logical([0 1 0 1 0]);
asserts.isequal(R, REF)
%=============================================================================
% Unicode space separators (but not the no-break ones), other types are never blanks.
asserts.isequal(isspace(char([9 10 11 12 13 28 29 30 31 32])), [true true true true true true true true true true]);
asserts.isequal(isspace(char([160 5760 8192 8199 8202 8203 8232 8233 8239 8287 12288])), [false true true false true false true true false true true]);
asserts.isequal(isspace(5), false);
asserts.isequal(isspace([1 2; 3 4]), [false false;false false]);
asserts.isequal(isspace(true), false);
asserts.isequal(isspace({'a'}), false);
asserts.isequal(isspace("a b"), [false true false]);
asserts.isequal(isspace(''), false(0, 0));
asserts.isequal(isspace(char(zeros(0, 3))), false(0, 3));
%=============================================================================

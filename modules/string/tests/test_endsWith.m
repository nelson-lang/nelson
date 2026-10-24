%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
asserts.isequal(nargin('endsWith'), -3);
asserts.isequal(nargout('endsWith'), 1);
%=============================================================================
c1 = 'time';
c2 = {'Once', 'upon'; 'a', 'time'};
res = endsWith(c1, c2);
ref = true;
asserts.isequal(res, ref);
%=============================================================================
res = endsWith(c2, c1);
ref = [false, false; false, true];
asserts.isequal(res, ref);
%=============================================================================
c1 = {'abs.docx', 'data.gz', 'code.m', 'results.nlf'};
c2 = {'.nlf', '.gz'};
res = endsWith(c1, c2);
ref = [false, true, false, true];
asserts.isequal(res, ref);
%=============================================================================
c1 = {'abs.docx', 'data.gz', 'code.nLf', 'results.nlf'};
c2 = {'.nlf', '.gz'};
res = endsWith(c1, c2, 'IgnoreCase', true);
ref = [false, true, true, true];
asserts.isequal(res, ref);
%=============================================================================
c1 = {'abs.docx', 'data.gz', 'code.m', 'results.nlf'};
c2 = {1};
asserts.checkerror('res = endsWith(c1, c2);', _('character vector or cell of strings expected.'));
%=============================================================================
c1 = "time";
c2 = ["Once", "upon"; "a", "time"];
res = endsWith(c1, c2);
ref = true;
asserts.isequal(res, ref);
%=============================================================================
c1 = ["abs.docx", "data.gz"; "code.nLf", "results.nlf"];
c2 = [".nlf"; ".gz"];
res = endsWith(c1, c2, 'IgnoreCase', true);
ref = [false, true; true, true];
asserts.isequal(res, ref);
%=============================================================================
%=============================================================================
% endsWith accepts pattern objects (anchored at the end).
asserts.istrue(endsWith("abc123", digitsPattern));
asserts.istrue(endsWith("123abc", lettersPattern));
asserts.isfalse(endsWith("123abc", digitsPattern));
asserts.istrue(endsWith("catdog", "dog" | "cat"));
asserts.istrue(endsWith("abc", alphanumericsPattern));
%=============================================================================
s = ["abc", missing, "cba"];
res = endsWith(s, "c");
ref = [true, false, false];
asserts.isequal(res, ref);
%=============================================================================
res = endsWith(s, ["c", "a"]);
ref = [true, false, true];
asserts.isequal(res, ref);
%=============================================================================
res = endsWith(string(missing), "a");
asserts.isequal(res, false);
%=============================================================================
res = endsWith({'a'; 'Fire'}, ["WINTER", "Fire"]);
asserts.isequal(res, [false; true]);
%=============================================================================
res = endsWith(["a"; "Fire"], {'WINTER', 'Fire'});
asserts.isequal(res, [false; true]);
%=============================================================================
res = endsWith({'a'; 'FIRE'}, ["winter", "fire"], 'IgnoreCase', true);
asserts.isequal(res, [false; true]);
%=============================================================================
asserts.checkerror('endsWith(''Fire'', 3)', _('character vector or cell of strings expected.'));
%=============================================================================
asserts.checkerror('endsWith(''Fire'', {1})', _('character vector or cell of strings expected.'));
%=============================================================================
% IgnoreCase: any case for the name, numeric values, Unicode case folding.
asserts.isequal(endsWith('Hello', 'LO', 'ignorecase', 1), true);
asserts.isequal(endsWith('Hello', 'LO', 'IgnoreCase', int8(1)), true);
asserts.isequal(endsWith('Hello', ''), true);
asserts.isequal(endsWith(['a' char(233)], char(201), 'IgnoreCase', true), true);
%=============================================================================

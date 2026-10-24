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
asserts.isequal(nargin('startsWith'), -3);
asserts.isequal(nargout('startsWith'), 1);
%=============================================================================
c1 = 'time';
c2 = {'Once', 'upon'; 'a', 'time'};
res = startsWith(c1, c2);
ref = true;
asserts.isequal(res, ref);
%=============================================================================
res = startsWith(c2, c1);
ref = [false, false; false, true];
asserts.isequal(res, ref);
%=============================================================================
c1 = {'abs.docx', 'data.gz', 'code.m', 'results.nlf'};
c2 = {'abs', 'data'};
res = startsWith(c1, c2);
ref = [true, true, false, false];
asserts.isequal(res, ref);
%=============================================================================
c1 = {'Data.docx', 'data.gz', 'code.m', 'AbS.nlf'};
c2 = {'abs', 'data'};
res = startsWith(c1, c2, 'IgnoreCase', true);
ref = [true, true, false, true];
asserts.isequal(res, ref);
%=============================================================================
c1 = {'abs.docx', 'data.gz', 'code.m', 'results.nlf'};
c2 = {1};
asserts.checkerror('res = startsWith(c1, c2);', _('character vector or cell of strings expected.'));
%=============================================================================
c1 = "time";
c2 = ["Once", "upon"; "a", "time"];
res = startsWith(c1, c2);
ref = true;
asserts.isequal(res, ref);
%=============================================================================
c1 = ["Data.docx", "data.gz"; "code.m", "AbS.nlf"];
c2 = ["abs", "data"];
res = startsWith(c1, c2, 'IgnoreCase', true);
ref = [true, true; false, true];
asserts.isequal(res, ref);
%=============================================================================
s = ["abc", missing, "cba"];
res = startsWith(s, "a");
ref = [true, false, false];
asserts.isequal(res, ref);
%=============================================================================
res = startsWith(s, ["a", "c"]);
ref = [true, false, true];
asserts.isequal(res, ref);
%=============================================================================
res = startsWith(string(missing), "a");
asserts.isequal(res, false);
%=============================================================================
res = startsWith({'a'; 'Fire'}, ["WINTER", "Fire"]);
asserts.isequal(res, [false; true]);
%=============================================================================
res = startsWith(["a"; "Fire"], {'WINTER', 'Fire'});
asserts.isequal(res, [false; true]);
%=============================================================================
res = startsWith({'a'; 'FIRE'}, ["winter", "fire"], 'IgnoreCase', true);
asserts.isequal(res, [false; true]);
%=============================================================================
asserts.checkerror('startsWith(''Fire'', 3)', _('character vector or cell of strings expected.'));
%=============================================================================
asserts.checkerror('startsWith(''Fire'', {1})', _('character vector or cell of strings expected.'));
%=============================================================================
% IgnoreCase: any case for the name, numeric values, Unicode case folding.
asserts.isequal(startsWith('Hello', 'HE', 'ignorecase', 1), true);
asserts.isequal(startsWith('Hello', 'HE', 'IgnoreCase', int8(1)), true);
asserts.isequal(startsWith('Hello', ''), true);
asserts.isequal(startsWith(['a' char(233)], ['A' char(201)], 'IgnoreCase', true), true);
%=============================================================================

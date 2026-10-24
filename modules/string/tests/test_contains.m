%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('contains'), -3);
asserts.isequal(nargout('contains'), 1);
%=============================================================================
c1 = 'time';
c2 = {'Once', 'upon'; 'a', 'time'};
res = contains(c1, c2);
ref = true;
asserts.isequal(res, ref);
%=============================================================================
res = contains(c2, c1);
ref = [false, false; false, true];
asserts.isequal(res, ref);
%=============================================================================
c1 = {'abs.docx', 'data.gz', 'code.m', 'results.nlf'};
c2 = {'abs', 'data'};
res = contains(c1, c2);
ref = [true, true, false, false];
asserts.isequal(res, ref);
%=============================================================================
c1 = {'abs.docx', 'data.gz', 'ABS.m', 'DAta.nlf'};
c2 = {'abs', 'data'};
res = contains(c1, c2, 'IgnoreCase', true);
ref = [true, true, true, true];
asserts.isequal(res, ref);
%=============================================================================
c1 = {'abs.docx', 'data.gz', 'code.m', 'results.nlf'};
c2 = {1};
asserts.checkerror('res = contains(c1, c2);', _('character vector or cell of strings expected.'));
%=============================================================================
c1 = "time";
c2 = ["Once", "upon"; "a", "time"];
res = contains(c1, c2);
ref = true;
asserts.isequal(res, ref);
%=============================================================================
c1 = ["abs.docx", "data.gz"; "ABS.m", "DAta.nlf"];
c2 = ["abs"; "data"];
res = contains(c1, c2, 'IgnoreCase', true);
ref = [true, true; true, true];
asserts.isequal(res, ref);
%=============================================================================
s = ["a", missing, "c"];
res = contains(s, "a");
ref = [true, false, false];
asserts.isequal(res, ref);
%=============================================================================
res = contains(s, "c");
ref = [false, false, true];
asserts.isequal(res, ref);
%=============================================================================
res = contains(s, ["a", "c"]);
ref = [true, false, true];
asserts.isequal(res, ref);
%=============================================================================
res = contains(string(missing), "a");
asserts.isequal(res, false);
%=============================================================================
% IgnoreCase: any case for the name, numeric values, Unicode case folding.
asserts.isequal(contains('Hello', 'ELL', 'IgnoreCase', true), true);
asserts.isequal(contains('Hello', 'ELL', 'ignorecase', true), true);
asserts.isequal(contains('Hello', 'ELL', 'IGNORECASE', 1), true);
asserts.isequal(contains('Hello', 'ELL', 'IgnoreCase', 0), false);
asserts.isequal(contains('Hello', 'ELL', 'IgnoreCase', int8(1)), true);
asserts.isequal(contains('Hello', ''), true);
asserts.isequal(contains('', ''), true);
asserts.isequal(contains({'Hello', 'x'}, ''), [true true]);
asserts.isequal(contains(['a' char(233)], char(201), 'IgnoreCase', true), true);
asserts.isequal(contains(['a' char(233)], char(201)), false);
%=============================================================================

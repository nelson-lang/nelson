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
asserts.isequal(str2num('[1 2 3]'), [1 2 3]);
asserts.isequal(str2num('1 2 3'), [1 2 3]);
asserts.isequal(str2num('2+3'), 5);
asserts.isequal(str2num('[1;2]'), [1; 2]);
asserts.isequal(class(str2num('int8(5)')), 'int8');
[x, ok] = str2num('abc');
asserts.isequal(x, []);
asserts.isequal(ok, false);
[y, ok2] = str2num('[4 5 6]');
asserts.isequal(y, [4 5 6]);
asserts.istrue(ok2);
asserts.isequal(str2num('pi'), pi);
[z, ok3] = str2num('');
asserts.isequal(z, []);
asserts.isequal(ok3, false);
asserts.isequal(str2num("[7 8]"), [7 8]);
%=============================================================================
% character matrices give rows, text and cell results are not numbers.
asserts.isequal(str2num(['1 2'; '3 4']), [1 2;3 4]);
asserts.isequal(str2num(['1 ';'2 ';'3 ']), [1;2;3]);
asserts.isequal(str2num('''abc'''), zeros(0, 0));
asserts.isequal(str2num('{1}'), zeros(0, 0));
asserts.isequal(str2num('true'), true);
asserts.isequal(str2num('1 2 3;'), [1 2 3]);
asserts.isequal(str2num('[]'), zeros(0, 0));
asserts.isequal(str2num(' '), zeros(0, 0));
asserts.isequal(str2num('abc'), zeros(0, 0));
asserts.isequal(str2num('1+'), zeros(0, 0));
asserts.isequal(str2num('1:3'), [1 2 3]);
asserts.isequal(str2num('Inf NaN'), [Inf NaN]);
asserts.isequal(str2num(sprintf('1\r2')), [1;2]);
asserts.isequal(str2num(sprintf('1\n2')), [1;2]);
asserts.isequal(str2num('int8(5)'), int8(5));
asserts.isequal(str2num('1 + 2i'), 1+2i);
%=============================================================================

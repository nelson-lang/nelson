%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
C = {1, [2 3], "four"};
[a, b] = C{[1 2]};
asserts.isequal(a, 1);
asserts.isequal(b, [2 3]);
%=============================================================================
[a, b, c] = C{:};
asserts.isequal(a, 1);
asserts.isequal(b, [2 3]);
asserts.isequal(c, "four");
%=============================================================================
S(1).value = 10;
S(2).value = 20;
[a, b] = S.value;
asserts.isequal(a, 10);
asserts.isequal(b, 20);
%=============================================================================
field = 'value';
[a, b] = S.(field);
asserts.isequal(a, 10);
asserts.isequal(b, 20);
%=============================================================================
asserts.checkerror('[a, b] = C{1};', _('Insufficient number of outputs from right hand side of equal sign to satisfy assignment.'));
%=============================================================================
handles = {@size};
[a, b] = handles{1}([1 2 3]);
asserts.isequal(a, 1);
asserts.isequal(b, 3);
%=============================================================================
asserts.checkerror('[a, b] = C{2}(1);', _('Insufficient number of outputs from right hand side of equal sign to satisfy assignment.'));
%=============================================================================

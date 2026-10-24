%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% NaN operands of element-wise logical operators must raise an error
%=============================================================================
msg = _('Conversion to logical with NaN is not possible.');
asserts.checkerror('R = and(NaN, 1);', msg);
asserts.checkerror('R = and(1, NaN);', msg);
asserts.checkerror('R = or(NaN, 0);', msg);
asserts.checkerror('R = or(0, NaN);', msg);
asserts.checkerror('R = not(NaN);', msg);
%=============================================================================
% matrix paths
asserts.checkerror('R = [1 NaN] & [1 1];', msg);
asserts.checkerror('R = [1 1] | [NaN 1];', msg);
asserts.checkerror('R = ~[0 NaN];', msg);
%=============================================================================
% single paths
asserts.checkerror('R = single(NaN) & 1;', msg);
asserts.checkerror('R = single(NaN) | 0;', msg);
asserts.checkerror('R = ~single(NaN);', msg);
%=============================================================================
% NOTE: 'NaN & 1', 'NaN | 0', '~NaN' with scalar double literals and
% 'NaN && 1' are evaluated through scalar fast paths of the interpreter
% (BytecodeVM binaryOperator/unaryOperator fusion and
% checkShortcutScalarCondition) and are not covered by this fix.
%=============================================================================
% non-NaN paths unchanged
asserts.isequal(1 & 1, true);
asserts.isequal(0 | 0, false);
asserts.isequal(~0, true);
asserts.isequal([1 0] & [1 1], logical([1 0]));
asserts.isequal(and(1, 0), false);
asserts.isequal(or(1, 0), true);
asserts.isequal(not(1), false);
%=============================================================================

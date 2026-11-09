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
% scalar paths of the interpreter
asserts.checkerror('R = NaN & 1;', msg);
asserts.checkerror('R = 1 & NaN;', msg);
asserts.checkerror('R = NaN | 0;', msg);
asserts.checkerror('R = ~NaN;', msg);
asserts.checkerror('R = NaN & true;', msg);
%=============================================================================
% && and || operands and if / while conditions
asserts.checkerror('R = NaN && true;', msg);
asserts.checkerror('R = true && NaN;', msg);
asserts.checkerror('R = NaN || true;', msg);
asserts.checkerror('R = false || NaN;', msg);
asserts.isequal(false && NaN, false);
asserts.isequal(true || NaN, true);
asserts.checkerror('if NaN, R = 1; end', msg);
asserts.checkerror('if [0 NaN], R = 1; end', msg);
asserts.checkerror('if single(NaN), R = 1; end', msg);
asserts.checkerror('if sparse(NaN), R = 1; end', msg);
asserts.checkerror('while NaN, break; end', msg);
asserts.checkerror('if 1 && NaN, R = 1; end', msg);
asserts.checkerror('if 0 || NaN, R = 1; end', msg);
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

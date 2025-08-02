%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_istrue(true && true);
assert_isfalse(true && false);
assert_isfalse(false && false);
assert_isfalse(false && true);
%=============================================================================
A = [true, false, true];
B = [true, true, true];
assert_checkerror('A && B', _('Operand to && operator must be convertible to logical scalar values.'));
%=============================================================================
A = [true, false, true];
B = [true, false, true, true];
assert_checkerror('A && B', _('Operand to && operator must be convertible to logical scalar values.'));
%=============================================================================
A = [true, false, true];
B = [true, false, true, true];
assert_checkerror('B && A', _('Operand to && operator must be convertible to logical scalar values.'));
%=============================================================================
assert_isfalse(ischar({'f'}) && strcmp({'f'}, 'f'));
%=============================================================================

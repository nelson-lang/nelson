%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.istrue(true && true);
asserts.isfalse(true && false);
asserts.isfalse(false && false);
asserts.isfalse(false && true);
%=============================================================================
A = [true, false, true];
B = [true, true, true];
asserts.checkerror('A && B', _('Operand to && operator must be convertible to logical scalar values.'));
%=============================================================================
A = [true, false, true];
B = [true, false, true, true];
asserts.checkerror('A && B', _('Operand to && operator must be convertible to logical scalar values.'));
%=============================================================================
A = [true, false, true];
B = [true, false, true, true];
asserts.checkerror('B && A', _('Operand to && operator must be convertible to logical scalar values.'));
%=============================================================================
asserts.isfalse(ischar({'f'}) && strcmp({'f'}, 'f'));
%=============================================================================

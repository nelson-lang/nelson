%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = 3.3 + int16(3);
REF = int16(6);
assert_isequal(R, REF)
%=============================================================================
R = int16(4) + 4;
REF = int16(8);
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = single(3.3) + int16(3);', _('Integers can only be combined with integers of the same class, or scalar doubles.'));
%=============================================================================
assert_checkerror('R = int16(4) + single(4);', _('Integers can only be combined with integers of the same class, or scalar doubles.'));
%=============================================================================
R = int16(4) + int16(4);
REF = int16(8);
assert_isequal(R, REF);
%=============================================================================
assert_checkerror('R = int16(4) + int8(4);', _('Integers can only be combined with integers of the same class, or scalar doubles.'));
%=============================================================================
assert_checkerror('int32(ones(3, 3)) + int32(ones(4, 4))', [_('Size mismatch on arguments to arithmetic operator'), ' +']);
%=============================================================================

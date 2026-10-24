%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('lcm(0,3)', _('Input arguments must contain positive integers.'));
asserts.checkerror('lcm(-4,6)', _('Input arguments must contain positive integers.'));
asserts.isequal(lcm(true,3), 3);
asserts.isequal(lcm('a','b'), 9506);
asserts.checkerror('lcm(int8(-4),int8(6))', _('Input arguments must contain positive integers.'));
asserts.checkerror('lcm(0,0)', _('Input arguments must contain positive integers.'));
asserts.checkerror('lcm(4,0)', _('Input arguments must contain positive integers.'));
asserts.checkerror('lcm(0,4)', _('Input arguments must contain positive integers.'));
asserts.checkerror('lcm(uint8(0),uint8(4))', _('Input arguments must contain positive integers.'));
asserts.checkerror('lcm(int8(0),int8(4))', _('Input arguments must contain positive integers.'));
asserts.checkerror('lcm(int8(-4),int8(-6))', _('Input arguments must contain positive integers.'));
asserts.isequal(lcm(true,true), 1);
asserts.checkerror('lcm(-4,-6)', _('Input arguments must contain positive integers.'));
asserts.checkerror('lcm(int32(-7),int32(3))', _('Input arguments must contain positive integers.'));
asserts.checkerror('lcm(0,5)', _('Input arguments must contain positive integers.'));
%=============================================================================

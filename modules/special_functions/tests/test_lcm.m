%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('lcm'), 2);
asserts.isequal(nargout('lcm'), 1);
%=============================================================================
asserts.isequal(lcm(4, 6), 12);
asserts.isequal(lcm(12, 18), 36);
asserts.isequal(lcm([2 3 4], [5 6 8]), [10 6 8]);
asserts.checkerror('lcm(0, 5)', _('Input arguments must contain positive integers.'));
asserts.checkerror('lcm(0, 0)', _('Input arguments must contain positive integers.'));
asserts.checkerror('lcm(-4, 6)', _('Input arguments must contain positive integers.'));
asserts.isequal(lcm(7, 1), 7);
asserts.isequal(lcm(uint16(6), uint16(8)), uint16(24));
%=============================================================================
asserts.checkerror('lcm(2.5, 3)', _('Inputs must be real integers.'), 'Nelson:gcd:mustBeRealIntegers');
%=============================================================================

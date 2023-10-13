%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
numerator1 = [1];
denominator1 = [1, 2, 1];
numerator2 = [3];
denominator2 = [1, 4, 4];
sys1 = tf(numerator1, denominator1);
sys2 = tf(numerator2, denominator2);
sys = [sys1; sys2];
%=============================================================================
sys3 = -sys1;
assert_isequal(sys3.Numerator{1}{1}, - sys1.Numerator{1}{1})
%=============================================================================
sys4 = -sys;
assert_isequal(sys4.Numerator{1}{1}, - sys.Numerator{1}{1})
assert_isequal(sys4.Numerator{1}{2}, - sys.Numerator{1}{2})
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(num2hex(1), '3ff0000000000000');
assert_isequal(num2hex(0), '0000000000000000');
assert_isequal(num2hex(Inf), '7ff0000000000000');
assert_isequal(num2hex(single(1)), '3f800000');
assert_isequal(hex2num('3ff0000000000000'), 1);
assert_isapprox(hex2num('400921fb54442d18'), pi, 0);
assert_isequal(hex2num(num2hex(exp(1))), exp(1));
%=============================================================================

%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sys1 = tf(1);
sys2 = tf(2);
assert_istrue(isequal(sys1, sys1))
assert_isfalse(isequal(sys1, sys2))
assert_isfalse(isequal(sys1, 1))
assert_isfalse(isequal(1, sys1))
%=============================================================================

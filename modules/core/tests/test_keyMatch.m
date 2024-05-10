%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('keyMatch'), 2);
assert_isequal(nargout('keyMatch'), 1);
%=============================================================================
assert_istrue(keyMatch(1, 1))
assert_istrue(keyMatch(NaN, NaN))
%=============================================================================
assert_isfalse(keyMatch(1, 0))
assert_isfalse(keyMatch(NaN, Inf))
%=============================================================================
X = struct("field1",1,"field2",2);
Y = struct("field1",1,"field2",2);
assert_istrue(keyMatch(X, Y));
%=============================================================================
Y.field1 = 3;
assert_isfalse(keyMatch(X, Y));
%=============================================================================

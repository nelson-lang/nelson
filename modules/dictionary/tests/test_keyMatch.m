%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('keyMatch'), 2);
asserts.isequal(nargout('keyMatch'), 1);
%=============================================================================
asserts.istrue(keyMatch(1, 1))
asserts.istrue(keyMatch(NaN, NaN))
%=============================================================================
asserts.isfalse(keyMatch(1, 0))
asserts.isfalse(keyMatch(NaN, Inf))
%=============================================================================
X = struct("field1", 1, "field2", 2);
Y = struct("field1", 1, "field2", 2);
asserts.istrue(keyMatch(X, Y));
%=============================================================================
Y.field1 = 3;
asserts.isfalse(keyMatch(X, Y));
%=============================================================================

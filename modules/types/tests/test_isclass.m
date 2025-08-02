%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('isclass'), 1);
assert_isequal(nargout('isclass'), 1);
%=============================================================================
A = 3;
res = isclass(A);
assert_isfalse(res);
%=============================================================================
addpath([nelsonroot(), '/modules/overload/examples/complex']);
c = complexObj(3,4);
res = isclass(c);
assert_istrue(res);
%=============================================================================

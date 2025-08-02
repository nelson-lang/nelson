%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('isvector'), 1)
assert_isequal(nargout('isvector'), 1)
%=============================================================================
assert_isfalse(isvector([]));
assert_isfalse(isvector(ones(0, 3)));
assert_istrue(isvector('hello'));
assert_isfalse(isvector(zeros(3,1,3)));
%=============================================================================

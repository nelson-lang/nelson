%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('isvarname'), 1);
assert_isequal(nargout('isvarname'), 1);
%=============================================================================
assert_isfalse(isvarname(''))
assert_isfalse(isvarname([]))
assert_isfalse(isvarname('8t'))
assert_isfalse(isvarname("8t"))
%=============================================================================
assert_istrue(isvarname('t9'))
assert_istrue(isvarname("t9"))
%=============================================================================

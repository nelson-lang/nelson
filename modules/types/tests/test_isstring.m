%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('isstring'), 1);
assert_isequal(nargout('isstring'), 1);
%=============================================================================
assert_istrue(isstring("yeah"));
assert_isfalse(isstring('yeah'));
assert_isfalse(isstring(1));
assert_istrue(isstring(["1", "go"]));
assert_isfalse(isstring({"1", "go"}));
%=============================================================================

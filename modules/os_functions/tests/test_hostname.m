%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('hostname'), 0);
assert_isequal(nargout('hostname'), 1);
%=============================================================================
R = hostname();
assert_istrue(ischar(R));
assert_istrue(isvector(R));
assert_isfalse(isempty(R));
%=============================================================================

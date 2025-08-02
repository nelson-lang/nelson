%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('license'), -1);
assert_isequal(nargout('license'), -1);
%=============================================================================
license();
%=============================================================================
a = license();
assert_istrue(strcmp(a, 'GNU Lesser General Public License v3.0') || strcmp(a, 'GNU General Public License v3.0'))
%=============================================================================
[a, b] = license();
assert_istrue(strcmp(a, 'GNU Lesser General Public License v3.0') || strcmp(a, 'GNU General Public License v3.0'))
assert_istrue(ischar(b));
assert_istrue(length(b) > 100);
%=============================================================================
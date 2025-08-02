%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('@dllib/isprop'), 2);
assert_isequal(nargout('@dllib/isprop'), 1);
%=============================================================================
path_ref = modulepath('dynamic_link', 'builtin');
lib = dlopen(path_ref);
assert_isfalse(isprop(lib, 'path'));
assert_istrue(isprop(lib, 'Path'));
%=============================================================================

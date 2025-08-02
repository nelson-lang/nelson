%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- NO USER MODULES -->
%=============================================================================
assert_isequal(nargin('@dllib/delete'), 1);
assert_isequal(nargout('@dllib/delete'), 0);
%=============================================================================
path_ref = [modulepath('nelson', 'builtin'), '/libnlsDynamic_link', getdynlibext()];
lib = dlopen(path_ref);
assert_istrue(isvalid(lib));
delete(lib);
assert_isfalse(isvalid(lib));
assert_isequal(size(dllib_used()), [0 0]);
%=============================================================================

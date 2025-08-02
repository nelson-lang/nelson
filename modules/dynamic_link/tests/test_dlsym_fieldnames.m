%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('@dlsym/isprop'), 2);
assert_isequal(nargout('@dlsym/isprop'), 1);
%=============================================================================
path_ref = [modulepath('nelson', 'builtin'), '/libnlsDynamic_link', getdynlibext()];
lib = dlopen(path_ref);
f = dlsym(lib, 'dynlibTestMultiplyDoubleArrayWithReturn', 'int32', {'doublePtr', 'int32'});
assert_isequal(fieldnames(f), {'Prototype'; 'Input'; 'Output'});
%=============================================================================

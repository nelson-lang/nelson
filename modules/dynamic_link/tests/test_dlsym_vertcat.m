%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
path_ref = [modulepath('nelson', 'builtin'), '/libnlsDynamic_link', getdynlibext()];
lib = dlopen(path_ref);
f1 = dlsym(lib, 'dynlibTestMultiplyDoubleArrayWithReturn', 'int32', {'doublePtr', 'int32'});
f2 = dlsym(lib, 'dynlibTestInputDoublePtr', 'void', {'doublePtr', 'int32'});
R = [f1, f2];
assert_isequal(size(R),  [1 2]);
assert_isequal(class(R), 'dlsym');
assert_isequal(R(1), f1);
assert_isequal(R(2), f2);
%=============================================================================

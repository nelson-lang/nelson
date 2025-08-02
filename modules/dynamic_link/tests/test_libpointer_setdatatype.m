%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('@libpointer/reshape'), 3);
assert_isequal(nargout('@libpointer/reshape'), 0);
%=============================================================================
a = libpointer('double', 3);
assert_checkerror('a.setdatatype(''int32'')', _('Incompatible types double --> int32'));
%=============================================================================
a = libpointer('doublePtr', 3);
assert_checkerror('a.setdatatype(''int32Ptr'')', _('Incompatible types doublePtr --> int32Ptr'));
%=============================================================================
a = libpointer();
a.setdatatype('doublePtr');
a.reshape(1, 1);
assert_checkerror('a.Value', _('The datatype and size of the value must be defined.'));
%=============================================================================
x = 133.3;
xPtr = libpointer('doublePtr', x);
path_ref = [modulepath('nelson', 'builtin'), '/libnlsDynamic_link', getdynlibext()];
lib = dlopen(path_ref);
f = dlsym(lib, 'multiplicationDoubleByReference', 'libpointer', {'doublePtr'});
[r1, r2] = dlcall(f, xPtr);
r1.setdatatype('doublePtr');
r1.reshape(1, 1);
assert_isequal(r1.Value, 266.6);
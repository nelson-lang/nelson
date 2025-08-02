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
lib = dlopen([modulepath('nelson', 'builtin'), '/libnlsDynamic_link', getdynlibext()]);
f = dlsym(lib, 'dynlibTestMultiplyDoubleArrayWithReturn', 'int32', {'doublePtr', 'int32'});
%=============================================================================
R = evalc('display(f)');
REF = '
f =

  1×1 handle [dlsym] 

  Prototype: string 1x79
  Input:     {doublePtr, int32}
  Output:    {int32, doublePtr}

';
assert_isequal(R, REF);
%=============================================================================
R = evalc('disp(f)');
REF = '  1×1 handle [dlsym] 

  Prototype: string 1x79
  Input:     {doublePtr, int32}
  Output:    {int32, doublePtr}
';
assert_isequal(R, REF);
%=============================================================================

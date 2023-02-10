%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ispc()
  nelsonLibrariesPath = modulepath('nelson', 'builtin');
  blas_library_name = [nelsonLibrariesPath, '/', 'libnlsblaslapack', getdynlibext()];
  lib = dlopen(blas_library_name)
else
  blas_library_name = ['libblas', getdynlibext()];
  try
    lib = dlopen(blas_library_name);
  catch
    if isfile('/usr/lib/x86_64-linux-gnu/libblas.so')
      blas_library_name = '/usr/lib/x86_64-linux-gnu/libblas.so';
    else
      if isfile('/usr/lib64/libblas.so')
        blas_library_name = '/usr/lib64/libblas.so';
      else
        blas_library_name = ['libblas', getdynlibext(), '.3'];
      end
    end
    try
      lib = dlopen(blas_library_name)
    catch
      error('libblas not found.');
    end
  end
end

DASUM_SYMBOL = "dasum";
V = [ -2, 1, 3, -5, 4, 0, -1, -3]
N = length(V)
% load DASUM blas symbol (L1 Norm)
f = dlsym(lib, DASUM_SYMBOL,'double',{'int32Ptr','doublePtr','int32Ptr'})
% call DASUM blas symbol with arguments
r = dlcall(f, int32(N), V, int32(1))
assert(r == 19)
% release handles
delete(f);
delete(lib);
% clear variables
clear f
clear lib

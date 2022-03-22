%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
if ispc()
  blas_library_name = ['libnlsblaslapack', getdynlibext()];
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

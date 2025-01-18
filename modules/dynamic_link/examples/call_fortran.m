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
    % For non-Windows systems, start with the generic BLAS library name
    blas_library_name = ['libblas', getdynlibext()];
    
    % Attempt to locate the library in common system paths
    try
      lib = dlopen(blas_library_name);
    catch
      % Check specific paths for BLAS/OpenBLAS libraries
      library_candidates = { '/usr/lib/aarch64-linux-gnu/libopenblas.so', ...
       '/usr/lib/aarch64-linux-gnu/libblas.so', ...
       '/usr/lib/x86_64-linux-gnu/libblas.so', ...
       '/usr/lib64/libblas.so'};
      
      for candidate = library_candidates
        if isfile(candidate{1})
          blas_library_name = candidate{1};
          break;
        end
      end
      
      % Fallback to versioned BLAS library name if no candidate matched
      if ~exist('blas_library_name', 'var')
        blas_library_name = ['libblas', getdynlibext(), '.3'];
      end
      
      % Attempt to load the library again
      try
        lib = dlopen(blas_library_name);
      catch
        error('Unable to locate or load the BLAS library.');
      end
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

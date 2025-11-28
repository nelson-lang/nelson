%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function call_fortran()
  % Example of calling a Fortran BLAS library (dasum function)
  blas_library_full_filename = detect_blas();
  if isempty(blas_library_full_filename)
    error('BLAS library not found');
  end
  % load BLAS library
  lib = dlopen(blas_library_full_filename);
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
end
%=============================================================================
function blas_library_full_filename = detect_blas()
  blas_library_full_filename = '';
  library_candidates = {};
  if ispc()
    nelsonLibrariesPath = modulepath('nelson', 'builtin');
    library_candidates{end+1} = [nelsonLibrariesPath, '/', 'libnlsblaslapack', getdynlibext()];
  else
    if isunix()
      library_candidates{end+1} = 'libopenblas.so.3';
      library_candidates{end+1} = 'libopenblas.so.0';
      library_candidates{end+1} = '/usr/lib/aarch64-linux-gnu/libopenblas.so';
      library_candidates{end+1} = '/usr/lib/x86_64-linux-gnu/libopenblas.so';
      library_candidates{end+1} = '/usr/lib/aarch64-linux-gnu/libblas.so';
      library_candidates{end+1} = '/usr/lib/x86_64-linux-gnu/libblas.so';
      library_candidates{end+1} = '/usr/lib64/libblas.so';
      library_candidates{end+1} = '/usr/lib64/libopenblas.so';
      library_candidates{end+1} = '/app/lib64/libopenblas.so';
    end
    if ismac()
      library_candidates{end+1} = '/opt/homebrew/lib/libblas.dylib';
      library_candidates{end+1} = 'libopenblas.dylib';
    end 
    library_candidates{end+1} = ['libblas', getdynlibext(), '.3'];
    library_candidates{end+1} = ['libblas', getdynlibext()];
  end

  for k = 1:length(library_candidates)
    blas_library_name = library_candidates{k};
    try
      lib = dlopen(blas_library_name);
      blas_library_full_filename = lib.Path;
      delete(lib);
      return
    catch
      blas_library_full_filename = '';
    end
  end
end  
%=============================================================================

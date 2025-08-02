%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
getpid_symbol = 'getpid';
if ispc()
  lib_c_name = ['msvcrt', getdynlibext()];
  libc = dlopen(lib_c_name)
else
  if isunix()
    % arch vs debian vs others
    lib_c_names = [string(['/usr/lib/x86_64-linux-gnu/libc', getdynlibext()]), ...
      string(['/usr/lib/aarch64-linux-gnu/libc', getdynlibext()]), ...
      string(['libc', getdynlibext()]), ...
      string(['/usr/lib64/libc', getdynlibext(), '.6']), ...
      string(['libc', getdynlibext(), '.6']), ...
      string(['/lib/x86_64-linux-gnu/libc', getdynlibext(), '.6']), ...
      string(['/lib/aarch64-linux-gnu/libc', getdynlibext(), '.6'])];
    for l = lib_c_names
      lib_c_name = char(l);
      try
        libc = dlopen(lib_c_name);
        break
      catch
        libc = [];
      end
    end
    if isempty(libc)
      libc = dlopen(['libc', getdynlibext()]);
    end
  else
    % macos
    lib_c_name = ['libc', getdynlibext()];
    libc = dlopen(lib_c_name)
  end
end
% getpid C function from standard libc library
f = dlsym(libc, getpid_symbol, 'int32', {})
% call getpid
pid = dlcall(f)
% release handles
delete(f);
delete(libc);
% clear variables
clear f
clear libc

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function native_entry()
  library = fullfile(fileparts(mfilename('fullpath')), 'native', ['nc_native_bridge', getdynlibext()]);
  loaded = dlopen(library);
  finish = onCleanup(@() dlclose(loaded));
  compute = dlsym(loaded, 'nc_native_bridge', 'double', {'double'});
  value = dlcall(compute, 7);
  if value ~= 17
    error('Incorrect native dependency result.');
  end
  fprintf('NATIVE_VALUE=%d\n', value);
  pause(2);
  clear compute;
  clear finish;
end

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function files = __nelsonc_runtime_backends__(runtime, directory)
  files = {};
  if ~any(strcmp(runtime.modules, 'fftw'))
    return;
  end
  % Keep each double/single pair in the default FFTW loader's search order.
  ext = getdynlibext();
  pairs = {['libfftw3-3', ext], ['libfftw3f-3', ext]; ...
    ['libfftw3', ext], ['libfftw3f', ext]; ...
    ['libfftw3', ext, '.3'], ['libfftw3f', ext, '.3']; ...
    ['libfftw3.3', ext], ['libfftw3f', ext]};
  for k = 1:size(pairs, 1)
    first = fullfile(directory, pairs{k, 1});
    second = fullfile(directory, pairs{k, 2});
    if isfile(first) && isfile(second)
      files = {first; second};
      return;
    end
  end
  error('Nelson:nelsonc:runtimeBackend', 'Missing default FFTW backend pair in the selected runtime.');
end

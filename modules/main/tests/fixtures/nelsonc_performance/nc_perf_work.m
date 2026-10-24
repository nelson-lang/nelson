%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = nc_perf_work(kind, count)
  result = 0;
  if strcmp(kind, 'scalar')
    for k = 1:count
      result = result + k * 0.25;
    end
  elseif strcmp(kind, 'crossfile')
    for k = 1:count
      result = result + nc_perf_leaf(k);
    end
  elseif strcmp(kind, 'builtin')
    for k = 1:count
      result = result + sin(k * 0.001);
    end
  elseif strcmp(kind, 'matrix') || strcmp(kind, 'matrixwide')
    width = 256;
    if strcmp(kind, 'matrixwide')
      width = 1024;
    end
    values = reshape(mod(1:width ^ 2, 31) / 31, width, width);
    transposed = values.';
    for k = 1:count
      product = values * transposed;
      result = result + product(1, 1) + product(width, width);
    end
  else
    error('Unknown benchmark workload.');
  end
end

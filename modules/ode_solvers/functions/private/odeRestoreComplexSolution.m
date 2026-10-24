%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sol = odeRestoreComplexSolution(sol, restore)
  sol = odeRestoreComplexSolutionStruct(sol, restore.StateSize, restore.Options);
end
%=============================================================================
function sol = odeRestoreComplexSolutionStruct(sol, n, originalOptions)
  sol.y = odeRealToComplexRows(sol.y, n);
  if isfield(sol, 'ye')
    sol.ye = odeRealToComplexRows(sol.ye, n);
  end
  if isfield(sol, 'extdata')
    if isfield(sol.extdata, 'slopes')
      sol.extdata.slopes = odeRealToComplexRows(sol.extdata.slopes, n);
    end
    if isfield(sol.extdata, 'internal')
      sol.extdata.internal = odeRestoreComplexSolutionStruct(sol.extdata.internal, n, originalOptions);
    end
    sol.extdata.options = originalOptions;
  end
  if isfield(sol, 'idata')
    sol.idata.idxNonNegative = originalOptions.NonNegative;
  end
end
%=============================================================================
function y = odeRealToComplexRows(z, n)
  if isempty(z)
    y = zeros(n, 0);
  else
    y = z(1:n, :) + 1i * z((n + 1):(2 * n), :);
  end
end
%=============================================================================

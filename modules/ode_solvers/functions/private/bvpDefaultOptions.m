%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function options = bvpDefaultOptions()
  % All fields default to empty so that bvpget(options, name, callerDefault)
  % honours the caller-supplied default when a property was never set. The
  % numeric defaults (AbsTol = 1e-6, RelTol = 1e-3, NMax = 1000, Stats = 'off',
  % Vectorized = 'off') are applied at consumption time inside bvpSolve.
  options = struct('AbsTol', [], 'RelTol', [], ...
    'NMax', [], 'Stats', [], 'Vectorized', [], ...
    'FJacobian', [], 'BCJacobian', [], 'SingularTerm', []);
end
%=============================================================================

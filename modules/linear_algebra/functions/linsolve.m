%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [X, r] = linsolve(A, B, opts)
  narginchk(2, 3);
  transA = false;
  if nargin == 3
    if ~isstruct(opts)
      error('Nelson:linsolve:invalidOptions', 'Third argument must be a structure of options.');
    end
    if isfield(opts, 'TRANSA') && ~isempty(opts.TRANSA) && opts.TRANSA
      transA = true;
    end
  end
  if transA
    X = A' \ B;
  else
    X = A \ B;
  end
  if nargout > 1
    r = rcond(A);
  end
end
%=============================================================================

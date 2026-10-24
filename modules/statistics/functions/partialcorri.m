%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [rho, pval] = partialcorri(y, x, varargin)
  narginchk(2, Inf);
  nargoutchk(0, 2);
  [z, args] = parseInputsLocal(y, x, convertStringToCharArgs(varargin));
  [y, x, z] = validateInputsLocal(y, x, z);
  rho = NaN(size(y, 2), size(x, 2));
  pval = NaN(size(y, 2), size(x, 2));
  for k = 1:size(x, 2)
    controls = [x(:, setdiff(1:size(x, 2), k, 'stable')), z];
    [rk, pk] = partialcorr(y, x(:, k), controls, args{:});
    rho(:, k) = rk;
    pval(:, k) = pk;
  end
end
%=============================================================================
function [z, args] = parseInputsLocal(y, x, args)
  z = zeros(size(y, 1), 0);
  if ~isempty(args) && ~isTextScalarLocal(args{1})
    z = args{1};
    args = args(2:end);
  end
  if rem(length(args), 2) ~= 0
    error('Nelson:statistics:nameValueNotPaired', 'Name-value arguments must be paired.');
  end
  for k = 1:2:length(args)
    if ~isTextScalarLocal(args{k})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    name = lower(char(args{k}));
    switch name
      case {'type', 'rows', 'tail'}
      otherwise
        error('Nelson:statistics:unknownNameValue', 'Invalid name-value argument name.');
    end
  end
end
%=============================================================================
function [y, x, z] = validateInputsLocal(y, x, z)
  if ~(isnumeric(y) && isreal(y) && ismatrix(y) && ~issparse(y))
    error('Nelson:statistics:yRealNumericMatrix', '%s must be a real numeric matrix.', 'Y');
  end
  if ~(isnumeric(x) && isreal(x) && ismatrix(x) && ~issparse(x))
    error('Nelson:statistics:xRealNumericMatrix', '%s must be a real numeric matrix.', 'X');
  end
  if ~(isnumeric(z) && isreal(z) && ismatrix(z) && ~issparse(z))
    error('Nelson:statistics:zRealNumericMatrix', '%s must be a real numeric matrix.', 'Z');
  end
  if size(y, 1) ~= size(x, 1) || size(y, 1) ~= size(z, 1)
    error('Nelson:statistics:xYZSameNumber', '%s must have the same number of rows.', 'Y, X, and Z');
  end
  y = double(y);
  x = double(x);
  z = double(z);
end
%=============================================================================
function tf = isTextScalarLocal(value)
  tf = ischar(value) || (isstring(value) && isscalar(value));
end
%=============================================================================

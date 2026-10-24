%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = uniquetol(A, varargin)
  narginchk(1, inf);
  nargoutchk(0, 3);
  if ~isreal(A) || issparse(A) || ~(isa(A, 'double') || isa(A, 'single'))
    error('Nelson:data_analysis:firstArgumentRealFullMatrix', 'First argument must be a real full matrix of type single or double.');
  end
  args = varargin;
  tol = [];
  if ~isempty(args) && isnumeric(args{1})
    tol = args{1};
    args(1) = [];
  end
  byRows = false;
  outputAllIndices = false;
  dataScale = [];
  if mod(numel(args), 2) ~= 0
    error('Nelson:data_analysis:nameValueArgumentsComePairs', 'Name-value arguments must come in pairs.');
  end
  for k = 1:2:numel(args)
    name = args{k};
    if ~(ischar(name) || (isstring(name) && isscalar(name)))
      error('Nelson:data_analysis:expectingValidPropertyName', 'Expecting a valid property name.');
    end
    value = args{k + 1};
    switch matchOptionName(char(name))
      case 'ByRows'
        byRows = logical(value);
      case 'OutputAllIndices'
        outputAllIndices = logical(value);
      case 'DataScale'
        dataScale = double(value);
      otherwise
        error('Nelson:data_analysis:expectingValidPropertyName', 'Expecting a valid property name.');
    end
  end
  if isempty(tol)
    if isa(A, 'single')
      tol = 1e-6;
    else
      tol = 1e-12;
    end
  else
    if ~isscalar(tol) || ~isreal(tol) || ~(tol >= 0)
      error('Nelson:data_analysis:TOLNonnegativeNumericScalar', 'TOL must be a nonnegative numeric scalar.');
    end
    tol = double(tol);
  end
  if byRows
    [C, ia, ic] = uniquetolByRows(A, tol, dataScale, outputAllIndices);
  else
    [C, ia, ic] = uniquetolVector(A, tol, dataScale, outputAllIndices);
  end
  varargout{1} = C;
  if nargout >= 2
    varargout{2} = ia;
  end
  if nargout >= 3
    varargout{3} = ic;
  end
end
%=============================================================================
function [C, ia, ic] = uniquetolVector(A, tol, dataScale, outputAllIndices)
  x = A(:);
  n = numel(x);
  if isempty(dataScale)
    scale = max(abs(x(~isnan(x))));
    if isempty(scale)
      scale = 0;
    end
  else
    scale = dataScale(1);
  end
  threshold = tol * scale;
  [xs, ord] = sort(x);
  groupOf = zeros(n, 1);
  g = 0;
  lastKept = [];
  for k = 1:n
    v = xs(k);
    if isnan(v)
      g = g + 1;
      groupOf(k) = g;
      continue;
    end
    if g == 0 || isempty(lastKept) || abs(v - lastKept) > threshold
      g = g + 1;
      groupOf(k) = g;
      lastKept = v;
    else
      groupOf(k) = g;
    end
  end
  firstOfGroup = [true; diff(groupOf) ~= 0];
  if n == 0
    firstOfGroup = false(0, 1);
  end
  C = xs(firstOfGroup);
  ic = zeros(n, 1);
  ic(ord) = groupOf;
  if outputAllIndices
    ia = cell(g, 1);
    for gid = 1:g
      ia{gid} = sort(ord(groupOf == gid));
    end
  else
    ia = ord(firstOfGroup);
  end
  if (ndims(A) == 2) && (size(A, 1) == 1) && (size(A, 2) ~= 0)
    C = C.';
  end
end
%=============================================================================
function [C, ia, ic] = uniquetolByRows(A, tol, dataScale, outputAllIndices)
  ncols = size(A, 2);
  nr = size(A, 1);
  if isempty(dataScale)
    colScale = max(abs(A), [], 1, 'omitnan');
    if isempty(colScale)
      colScale = zeros(1, ncols);
    end
  elseif isscalar(dataScale)
    colScale = repmat(dataScale, 1, ncols);
  else
    colScale = dataScale(:).';
  end
  threshold = tol * colScale;
  [As, ord] = sortrows(A);
  groupOf = zeros(nr, 1);
  g = 0;
  lastKept = [];
  for k = 1:nr
    r = As(k, :);
    if g == 0
      g = 1;
      groupOf(k) = 1;
      lastKept = r;
    elseif all(abs(r - lastKept) <= threshold)
      groupOf(k) = g;
    else
      g = g + 1;
      groupOf(k) = g;
      lastKept = r;
    end
  end
  if nr == 0
    firstOfGroup = false(0, 1);
  else
    firstOfGroup = [true; diff(groupOf) ~= 0];
  end
  C = As(firstOfGroup, :);
  ic = zeros(nr, 1);
  ic(ord) = groupOf;
  if outputAllIndices
    ia = cell(g, 1);
    for gid = 1:g
      ia{gid} = sort(ord(groupOf == gid));
    end
  else
    ia = ord(firstOfGroup);
  end
end
%=============================================================================
function name = matchOptionName(candidate)
  options = {'ByRows', 'OutputAllIndices', 'DataScale'};
  name = '';
  lc = lower(candidate);
  for k = 1:numel(options)
    if strcmp(lc, lower(options{k})) || (~isempty(lc) && startsWith(lower(options{k}), lc))
      name = options{k};
      return;
    end
  end
end
%=============================================================================

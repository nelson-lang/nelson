%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [gridVectors, values, method, extrap] = griddedInterpolantParse(varargin)
  args = varargin;
  method = '';
  extrap = '';
  if numel(args) >= 2 && griddedInterpolantIsText(args{end}) && griddedInterpolantIsText(args{end - 1})
    method = convertStringsToChars(args{end - 1});
    extrap = convertStringsToChars(args{end});
    args(end - 1:end) = [];
  elseif numel(args) >= 1 && griddedInterpolantIsText(args{end})
    method = convertStringsToChars(args{end});
    args(end) = [];
  end

  if isempty(method)
    method = 'linear';
  else
    method = griddedInterpolantValidateMethod(method);
  end
  if isempty(extrap)
    extrap = method;
  else
    extrap = griddedInterpolantValidateExtrap(extrap);
  end

  if numel(args) == 0
    error('Nelson:notEnoughInputs', 'Not enough input arguments.');
  elseif numel(args) == 1
    values = args{1};
    gridVectors = griddedInterpolantDefaultGrid(values);
  elseif numel(args) == 2
    first = args{1};
    values = args{2};
    if iscell(first)
      gridVectors = reshape(first, 1, numel(first));
    else
      gridVectors = {first};
    end
  else
    values = args{end};
    gridVectors = griddedInterpolantGridFromArrays(args(1:end - 1));
  end

  [gridVectors, values] = griddedInterpolantValidateGrid(gridVectors, values);
end
%=============================================================================
function gridVectors = griddedInterpolantDefaultGrid(values)
  if isvector(values)
    gridVectors = {1:numel(values)};
  else
    nd = ndims(values);
    gridVectors = cell(1, nd);
    for dim = 1:nd
      gridVectors{dim} = 1:size(values, dim);
    end
  end
end
%=============================================================================
function gridVectors = griddedInterpolantGridFromArrays(gridArrays)
  nd = numel(gridArrays);
  gridVectors = cell(1, nd);
  for dim = 1:nd
    g = gridArrays{dim};
    if isvector(g)
      gridVectors{dim} = g;
    else
      subs = repmat({1}, 1, nd);
      subs{dim} = ':';
      slice = g(subs{:});
      gridVectors{dim} = slice(:);
    end
  end
end
%=============================================================================

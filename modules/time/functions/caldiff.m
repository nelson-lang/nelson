%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function c = caldiff(t, components, varargin)
  % Calendar differences between consecutive elements of t (see between); a
  % trailing 'ArithmeticMethod' name-value pair is passed to between.
  narginchk(1, 5);
  if nargin < 2
    components = 'months';
  end
  [dim, options] = splitDimension(varargin);
  serial = datenum(t);
  if isempty(dim)
    dim = find(size(serial) ~= 1, 1);
    if isempty(dim)
      dim = 1;
    end
  end
  idx1 = repmat({':'}, 1, ndims(serial));
  idx2 = idx1;
  idx1{dim} = 1:(size(serial, dim) - 1);
  idx2{dim} = 2:size(serial, dim);
  c = between(t(idx1{:}), t(idx2{:}), components, options{:});
end
%=============================================================================
function [dim, options] = splitDimension(args)
  % optional dimension first, then name-value options
  dim = [];
  options = args;
  if ~isempty(args) && isnumeric(args{1})
    dim = args{1};
    options = args(2:end);
  end
end
%=============================================================================

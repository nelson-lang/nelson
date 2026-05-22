%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Vq = interp2(varargin)
  narginchk(1, 7);
  nargoutchk(0, 1);
  [args, method, extrapMode, extrapValue] = interp_parse_tail(varargin, false);

  switch numel(args)
    case 1
      V = args{1};
      k = 1;
      [x, y, xq, yq] = interp2_default_refined_grid(V, k);
    case 2
      V = args{1};
      k = args{2};
      [x, y, xq, yq] = interp2_default_refined_grid(V, k);
    case 3
      V = args{1};
      xq = args{2};
      yq = args{3};
      x = 1:size(V, 2);
      y = 1:size(V, 1);
    case 5
      [x, y] = interp2_grid_vectors(args{1}, args{2});
      V = args{3};
      xq = args{4};
      yq = args{5};
    otherwise
      error(_('Wrong number of input arguments.'));
  end
  Vq = interpn_core({y, x}, V, {yq, xq}, method, extrapMode, extrapValue);
end
%=============================================================================
function [x, y, xq, yq] = interp2_default_refined_grid(V, k)
  if ~isscalar(k) || k < 0 || fix(k) ~= k
    error(_('Refinement factor must be a nonnegative integer scalar.'));
  end
  step = 1 / (2^k);
  x = 1:size(V, 2);
  y = 1:size(V, 1);
  xq = 1:step:size(V, 2);
  yq = (1:step:size(V, 1)).';
end
%=============================================================================
function [x, y] = interp2_grid_vectors(X, Y)
  if isvector(X) && isvector(Y)
    x = X(:).';
    y = Y(:);
  else
    if ~isequal(size(X), size(Y))
      error(_('Grid arrays must have the same size.'));
    end
    x = X(1, :);
    y = Y(:, 1);
  end
end
%=============================================================================

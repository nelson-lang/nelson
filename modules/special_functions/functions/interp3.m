%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function Vq = interp3(varargin)
  narginchk(1, 9);
  nargoutchk(0, 1);
  [args, method, extrapMode, extrapValue] = interp_parse_tail(varargin, false);

  switch numel(args)
    case 1
      V = args{1};
      k = 1;
      [x, y, z, xq, yq, zq] = interp3_default_refined_grid(V, k);
    case 2
      V = args{1};
      k = args{2};
      [x, y, z, xq, yq, zq] = interp3_default_refined_grid(V, k);
    case 4
      V = args{1};
      xq = args{2};
      yq = args{3};
      zq = args{4};
      x = 1:size(V, 2);
      y = 1:size(V, 1);
      z = 1:size(V, 3);
    case 7
      [x, y, z] = interp3_grid_vectors(args{1}, args{2}, args{3});
      V = args{4};
      xq = args{5};
      yq = args{6};
      zq = args{7};
    otherwise
      error(_('Wrong number of input arguments.'));
  end
  Vq = interpn_core({y, x, z}, V, {yq, xq, zq}, method, extrapMode, extrapValue);
end
%=============================================================================
function [x, y, z, xq, yq, zq] = interp3_default_refined_grid(V, k)
  if ~isscalar(k) || k < 0 || fix(k) ~= k
    error(_('Refinement factor must be a nonnegative integer scalar.'));
  end
  step = 1 / (2^k);
  x = 1:size(V, 2);
  y = 1:size(V, 1);
  z = 1:size(V, 3);
  xq = 1:step:size(V, 2);
  yq = (1:step:size(V, 1)).';
  zq = reshape(1:step:size(V, 3), 1, 1, []);
end
%=============================================================================
function [x, y, z] = interp3_grid_vectors(X, Y, Z)
  if isvector(X) && isvector(Y) && isvector(Z)
    x = X(:).';
    y = Y(:);
    z = Z(:).';
  else
    if ~isequal(size(X), size(Y)) || ~isequal(size(X), size(Z))
      error(_('Grid arrays must have the same size.'));
    end
    x = X(1, :, 1);
    y = Y(:, 1, 1);
    z = squeeze(Z(1, 1, :)).';
  end
end
%=============================================================================

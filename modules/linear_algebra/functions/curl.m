%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = curl(varargin)
  % curl of a 2-D or 3-D vector field.
  %   curlz = curl(X, Y, U, V)                     2-D field on grid X, Y
  %   curlz = curl(U, V)                           2-D field on a default grid
  %   [curlz, cav] = curl(...)                     also the angular velocity
  %   [cx, cy, cz, cav] = curl(X, Y, Z, U, V, W)   3-D field on grid X, Y, Z
  %   [cx, cy, cz, cav] = curl(U, V, W)            3-D field on a default grid
  narginchk(2, 6);
  switch nargin
    case 2
      [U, V] = deal(varargin{:});
      [X, Y] = defaultGrid2D(U);
      [varargout{1:max(nargout, 1)}] = curl2D(X, Y, U, V, nargout);
    case 4
      [X, Y, U, V] = deal(varargin{:});
      [varargout{1:max(nargout, 1)}] = curl2D(X, Y, U, V, nargout);
    case 3
      [U, V, W] = deal(varargin{:});
      [X, Y, Z] = defaultGrid3D(U);
      [varargout{1:max(nargout, 1)}] = curl3D(X, Y, Z, U, V, W, nargout);
    case 6
      [X, Y, Z, U, V, W] = deal(varargin{:});
      [varargout{1:max(nargout, 1)}] = curl3D(X, Y, Z, U, V, W, nargout);
    otherwise
      error('Nelson:curl:invalidNumberOfInputs', 'Wrong number of input arguments.');
  end
end
%=============================================================================
function varargout = curl2D(X, Y, U, V, nOut)
  if ~isequal(size(U), size(V)) || ~isequal(size(X), size(U)) || ~isequal(size(Y), size(U))
    error('Nelson:curl:inputSizeMismatch', 'X, Y, U and V must have the same size.');
  end
  xv = X(1, :);
  yv = Y(:, 1);
  [~, dUdy] = gradient(U, xv, yv);
  [dVdx, ~] = gradient(V, xv, yv);
  curlz = dVdx - dUdy;
  cav = curlz / 2;
  if nOut <= 1
    varargout = {curlz};
  else
    varargout = {curlz, cav};
  end
end
%=============================================================================
function varargout = curl3D(X, Y, Z, U, V, W, nOut)
  if ~isequal(size(U), size(V)) || ~isequal(size(U), size(W)) ...
      || ~isequal(size(X), size(U)) || ~isequal(size(Y), size(U)) || ~isequal(size(Z), size(U))
    error('Nelson:curl:inputSizeMismatch', 'X, Y, Z, U, V and W must have the same size.');
  end
  xv = squeeze(X(1, :, 1));
  yv = squeeze(Y(:, 1, 1));
  zv = squeeze(Z(1, 1, :));
  [~, dUdy, dUdz] = gradient(U, xv, yv, zv);
  [dVdx, ~, dVdz] = gradient(V, xv, yv, zv);
  [dWdx, dWdy, ~] = gradient(W, xv, yv, zv);
  curlx = dWdy - dVdz;
  curly = dUdz - dWdx;
  curlz = dVdx - dUdy;
  cav = 0.5 * sqrt(curlx .^ 2 + curly .^ 2 + curlz .^ 2);
  if nOut <= 1
    varargout = {cav};
  else
    varargout = {curlx, curly, curlz, cav};
    varargout = varargout(1:min(nOut, 4));
  end
end
%=============================================================================
function [X, Y] = defaultGrid2D(U)
  [X, Y] = meshgrid(1:size(U, 2), 1:size(U, 1));
end
%=============================================================================
function [X, Y, Z] = defaultGrid3D(U)
  [X, Y, Z] = meshgrid(1:size(U, 2), 1:size(U, 1), 1:size(U, 3));
end
%=============================================================================

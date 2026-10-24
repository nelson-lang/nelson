%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = ellipsoid(varargin)
  nargoutchk(0, 3);
  args = varargin;
  ax = [];
  if ~isempty(args) && isscalar(args{1}) && isgraphics(args{1}, 'axes')
    ax = args{1};
    args(1) = [];
  end
  if numel(args) < 6 || numel(args) > 7
    error('Nelson:ellipsoid:invalidNumInputs', ...
      'ELLIPSOID requires xc, yc, zc, xr, yr, zr and an optional N.');
  end
  xc = args{1};
  yc = args{2};
  zc = args{3};
  xr = args{4};
  yr = args{5};
  zr = args{6};
  if numel(args) == 7
    n = args{7};
  else
    n = 20;
  end
  if ~(isscalar(n) && isreal(n) && n > 0 && n == floor(n))
    error('Nelson:ellipsoid:invalidN', 'N must be a positive integer.');
  end
  theta = (-n:2:n) / n * pi;
  phi = (-n:2:n)' / n * pi / 2;
  cosphi = cos(phi);
  cosphi(1) = 0;
  cosphi(n + 1) = 0;
  sintheta = sin(theta);
  sintheta(1) = 0;
  sintheta(n + 1) = 0;
  x = xc + xr * (cosphi * cos(theta));
  y = yc + yr * (cosphi * sintheta);
  z = zc + zr * (sin(phi) * ones(1, n + 1));
  if nargout == 0
    ax = newplot(ax);
    surf(x, y, z, 'Parent', ax);
  else
    varargout{1} = x;
    varargout{2} = y;
    varargout{3} = z;
  end
end
%=============================================================================

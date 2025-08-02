%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = meshgrid(x, y, z)
  nargoutchk(0, 3);
  % Cartesian rectangular grid in 2-D or 3-D    
  if nargin == 0 || (nargin > 1 && nargout > nargin)
    error('Nelson:meshgrid:NotEnoughInputs', _('Wrong number of input arguments.'));
  end
  isCartesianRectangularGrid2D = (nargin == 1 && nargout < 3) || (nargin == 2);
  if ~isCartesianRectangularGrid2D
    if nargin == 1
      y = x;
      z = x;
    end
    allEmpty = isempty(x) || isempty(y) || isempty(z);
    if ~allEmpty
      [XX, YY, ZZ] = meshgrid3d(x, y, z);
    else
      [XX, YY, ZZ] = meshgrid3dAllEmpty(x, y, z);
    end
    varargout{1} = XX;
    varargout{2} = YY;
    varargout{3} = ZZ;
  else
    if nargin == 1
      y = x;
    end
    allEmpty = isempty(x) || isempty(y);
    if ~allEmpty
      [XX, YY] = meshgrid2d(x, y);
    else
      [XX, YY] = meshgrid2dAllEmpty(x, y);
    end
    varargout{1} = XX;
    varargout{2} = YY;
  end
end
%=============================================================================
function [XX, YY, ZZ] = meshgrid3d(x, y, z)
  numel_x = numel(x);
  numel_y = numel(y);
  numel_z = numel(z);
  XX = reshape(full(x), [1, numel_x, 1]);
  YY = reshape(full(y), [numel_y, 1, 1]);
  ZZ = reshape(full(z), [1, 1, numel_z]);
  XX = repmat(XX, numel_y, 1, numel_z);
  YY = repmat(YY, 1, numel_x, numel_z);
  ZZ = repmat(ZZ, numel_y, numel_x, 1);
end
%=============================================================================
function [XX, YY, ZZ] = meshgrid3dAllEmpty(x, y, z)
  XX = zeros(0, class(x));
  YY = zeros(0, class(y));
  ZZ = zeros(0, class(z));
end
%=============================================================================
function [XX, YY] = meshgrid2d(x, y)
  xrow = full(x(:)).';
  ycol = full(y(:));
  XX = repmat(xrow, size(ycol));
  YY = repmat(ycol, size(xrow));
end
%=============================================================================
function [XX, YY] = meshgrid2dAllEmpty(x, y)
  XX = zeros(0, class(x));
  YY = zeros(0, class(y));
end
%=============================================================================

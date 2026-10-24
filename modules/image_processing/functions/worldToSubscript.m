%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = worldToSubscript(varargin)
  narginchk(3, 4);
  ref = varargin{1};
  [dim, ref] = ip_spatial_ref_dimension(ref);
  if dim == 2
    nargoutchk(0, 2);
    if nargin ~= 3
      error('Nelson:image_processing:dSpatialReferencesRequireXY', '2-D spatial references require X and Y coordinates.');
    end
    [xIntrinsic, yIntrinsic] = worldToIntrinsic(ref, varargin{2}, varargin{3});
    row = round(yIntrinsic);
    column = round(xIntrinsic);
    outside = xIntrinsic < ref.XIntrinsicLimits(1) | xIntrinsic > ref.XIntrinsicLimits(2) ...
      | yIntrinsic < ref.YIntrinsicLimits(1) | yIntrinsic > ref.YIntrinsicLimits(2);
    row(outside) = NaN;
    column(outside) = NaN;
    varargout{1} = row;
    varargout{2} = column;
  else
    nargoutchk(0, 3);
    if nargin ~= 4
      error('Nelson:image_processing:dSpatialReferencesRequireXYZ', '3-D spatial references require X, Y and Z coordinates.');
    end
    [xIntrinsic, yIntrinsic, zIntrinsic] = worldToIntrinsic(ref, varargin{2}, varargin{3}, ...
      varargin{4});
    row = round(yIntrinsic);
    column = round(xIntrinsic);
    plane = round(zIntrinsic);
    outside = xIntrinsic < ref.XIntrinsicLimits(1) | xIntrinsic > ref.XIntrinsicLimits(2) ...
      | yIntrinsic < ref.YIntrinsicLimits(1) | yIntrinsic > ref.YIntrinsicLimits(2) ...
      | zIntrinsic < ref.ZIntrinsicLimits(1) | zIntrinsic > ref.ZIntrinsicLimits(2);
    row(outside) = NaN;
    column(outside) = NaN;
    plane(outside) = NaN;
    varargout{1} = row;
    varargout{2} = column;
    varargout{3} = plane;
  end
end
%=============================================================================

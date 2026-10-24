%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = worldToIntrinsic(varargin)
  narginchk(3, 4);
  ref = varargin{1};
  [dim, ref] = ip_spatial_ref_dimension(ref);
  if dim == 2
    nargoutchk(0, 2);
    if nargin ~= 3
      error('Nelson:image_processing:dSpatialReferencesRequireXY', '2-D spatial references require X and Y coordinates.');
    end
    varargout{1} = (double(varargin{2}) - ref.XWorldLimits(1)) ...
      / ref.PixelExtentInWorldX + 0.5;
    varargout{2} = (double(varargin{3}) - ref.YWorldLimits(1)) ...
      / ref.PixelExtentInWorldY + 0.5;
  else
    nargoutchk(0, 3);
    if nargin ~= 4
      error('Nelson:image_processing:dSpatialReferencesRequireXYZ', '3-D spatial references require X, Y and Z coordinates.');
    end
    varargout{1} = (double(varargin{2}) - ref.XWorldLimits(1)) ...
      / ref.PixelExtentInWorldX + 0.5;
    varargout{2} = (double(varargin{3}) - ref.YWorldLimits(1)) ...
      / ref.PixelExtentInWorldY + 0.5;
    varargout{3} = (double(varargin{4}) - ref.ZWorldLimits(1)) ...
      / ref.PixelExtentInWorldZ + 0.5;
  end
end
%=============================================================================

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = intrinsicToWorld(varargin)
  narginchk(3, 4);
  ref = varargin{1};
  [dim, ref] = ip_spatial_ref_dimension(ref);
  if dim == 2
    nargoutchk(0, 2);
    if nargin ~= 3
      error('Nelson:image_processing:dSpatialReferencesRequireXY', '2-D spatial references require X and Y coordinates.');
    end
    varargout{1} = ref.XWorldLimits(1) + (double(varargin{2}) - 0.5) ...
      * ref.PixelExtentInWorldX;
    varargout{2} = ref.YWorldLimits(1) + (double(varargin{3}) - 0.5) ...
      * ref.PixelExtentInWorldY;
  else
    nargoutchk(0, 3);
    if nargin ~= 4
      error('Nelson:image_processing:dSpatialReferencesRequireXYZ', '3-D spatial references require X, Y and Z coordinates.');
    end
    varargout{1} = ref.XWorldLimits(1) + (double(varargin{2}) - 0.5) ...
      * ref.PixelExtentInWorldX;
    varargout{2} = ref.YWorldLimits(1) + (double(varargin{3}) - 0.5) ...
      * ref.PixelExtentInWorldY;
    varargout{3} = ref.ZWorldLimits(1) + (double(varargin{4}) - 0.5) ...
      * ref.PixelExtentInWorldZ;
  end
end
%=============================================================================

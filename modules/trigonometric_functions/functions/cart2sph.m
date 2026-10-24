%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = cart2sph(varargin)
  narginchk(3, 3);
  nargoutchk(0, 3);
  xCoordinate = varargin{1};
  yCoordinate = varargin{2};
  zCoordinate = varargin{3};

  xyRadius = hypot(xCoordinate, yCoordinate);
  sphericalValues = cell(1, 3);
  sphericalValues{1} = atan2(yCoordinate, xCoordinate);
  sphericalValues{2} = atan2(zCoordinate, xyRadius);
  sphericalValues{3} = hypot(xyRadius, zCoordinate);

  varargout = sphericalValues(1:nargout);
end
%=============================================================================



%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = imreconstruct(varargin)
  narginchk(2, 3);
  nargoutchk(0, 1);
  marker = varargin{1};
  mask = varargin{2};
  [markerValues, maskValues] = ip_validate_reconstruction_pair(marker, mask);
  conn = 8;
  if nargin == 3
    conn = ip_validate_watershed_connectivity(varargin{3});
  end
  reconstructed = ip_reconstruct_by_dilation_2d(markerValues, maskValues, conn);
  out = ip_cast_like(reconstructed, mask);
end
%=============================================================================

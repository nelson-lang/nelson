%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = imclearborder(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  bw = ip_validate_binary_image_2d(varargin{1});
  conn = 8;
  if nargin == 2
    conn = ip_validate_watershed_connectivity(varargin{2});
  end
  cc = bwconncomp(bw, conn);
  out = bw;
  border = false(size(bw));
  border(1, :) = true;
  border(end, :) = true;
  border(:, 1) = true;
  border(:, end) = true;
  borderIdx = find(border);
  for k = 1:cc.NumObjects
    pixels = cc.PixelIdxList{k};
    if any(ismember(pixels, borderIdx))
      out(pixels) = false;
    end
  end
end
%=============================================================================

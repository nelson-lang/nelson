%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bw2 = imfill(varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);
  bw = varargin{1};
  ip_assert_full_image(bw, false);
  bw = ip_to_logical_mask(bw);
  conn = 8;
  mode = 'holes';
  for k = 2:nargin
    arg = varargin{k};
    if ischar(arg) || isstring(arg)
      mode = lower(convertStringsToChars(arg));
    else
      conn = double(arg);
    end
  end
  if ~(conn == 4 || conn == 8)
    error('Nelson:image_processing:unsupportedConnectivity', 'Unsupported connectivity.');
  end
  if ~strcmp(mode, 'holes')
    error('Nelson:image_processing:unsupportedFillOperation', 'Unsupported "%s" option.', 'fill operation');
  end
  background = ~bw;
  cc = bwconncomp(background, conn);
  border = false(size(bw));
  border(1, :) = true;
  border(end, :) = true;
  border(:, 1) = true;
  border(:, end) = true;
  keepBackground = false(size(bw));
  for k = 1:cc.NumObjects
    idx = cc.PixelIdxList{k};
    if any(border(idx))
      keepBackground(idx) = true;
    end
  end
  bw2 = ~keepBackground;
end
%=============================================================================

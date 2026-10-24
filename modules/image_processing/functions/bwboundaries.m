%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [boundaries, labels, numObjects] = bwboundaries(varargin)
  narginchk(1, 3);
  nargoutchk(0, 3);
  bw = varargin{1};
  ip_assert_full_image(bw, false);
  bw = ip_to_logical_mask(bw);
  conn = 8;
  includeHoles = false;
  for k = 2:nargin
    arg = varargin{k};
    if ischar(arg) || isstring(arg)
      txt = lower(convertStringsToChars(arg));
      if strcmp(txt, 'holes')
        includeHoles = true;
      elseif strcmp(txt, 'noholes')
        includeHoles = false;
      else
        error('Nelson:image_processing:unsupportedBoundaryOption', 'Unsupported "%s" option.', 'boundary');
      end
    else
      conn = double(arg);
    end
  end
  if ~(conn == 4 || conn == 8)
    error('Nelson:image_processing:unsupportedConnectivity', 'Unsupported connectivity.');
  end
  cc = bwconncomp(bw, conn);
  labels = labelmatrix(cc);
  perim = bwperim(bw, conn);
  boundaries = ip_boundaries_from_perimeter(perim, labels, cc.NumObjects);
  if includeHoles
    holes = imfill(bw, conn, 'holes') & ~bw;
    if any(holes(:))
      hcc = bwconncomp(holes, conn);
      hlabels = labelmatrix(hcc);
      hperim = bwperim(holes, conn);
      holeBoundaries = ip_boundaries_from_perimeter(hperim, hlabels, hcc.NumObjects);
      boundaries = [boundaries; holeBoundaries];
    end
  end
  numObjects = cc.NumObjects;
end
%=============================================================================

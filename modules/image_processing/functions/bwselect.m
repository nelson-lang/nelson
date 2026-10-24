%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [bw2, idx] = bwselect(varargin)
  narginchk(3, 4);
  nargoutchk(0, 2);
  bw = varargin{1};
  ip_assert_full_image(bw, false);
  bw = ip_to_logical_mask(bw);
  cols = double(varargin{2});
  rows = double(varargin{3});
  if ~isnumeric(varargin{2}) || ~isnumeric(varargin{3}) || ~isreal(varargin{2}) ...
      || ~isreal(varargin{3}) || ~isequal(size(cols), size(rows))
    error('Nelson:image_processing:selectionCoordinatesNumericArraysSameSize', 'Selection coordinates must be numeric arrays of the same size.');
  end
  conn = 8;
  if nargin == 4
    conn = double(varargin{4});
  end
  if ~(conn == 4 || conn == 8)
    error('Nelson:image_processing:unsupportedConnectivity', 'Unsupported connectivity.');
  end
  cc = bwconncomp(bw, conn);
  labels = labelmatrix(cc);
  selectedLabels = [];
  for k = 1:numel(rows)
    r = round(rows(k));
    c = round(cols(k));
    if isfinite(r) && isfinite(c) && r >= 1 && c >= 1 && r <= size(bw, 1) && c <= size(bw, 2)
      label = labels(r, c);
      if label > 0
        selectedLabels = [selectedLabels label];
      end
    end
  end
  selectedLabels = unique(selectedLabels);
  bw2 = false(size(bw));
  for k = 1:numel(selectedLabels)
    bw2(cc.PixelIdxList{selectedLabels(k)}) = true;
  end
  idx = find(bw2);
end
%=============================================================================

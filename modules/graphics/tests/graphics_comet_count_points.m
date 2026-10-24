%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function counts = graphics_comet_count_points(handles)
  counts = zeros(1, numel(handles));
  for k = 1:numel(handles)
    [x, y] = getpoints(handles(k));
    asserts.isequal(numel(x), numel(y));
    counts(k) = numel(x);
  end
  counts = sort(counts);
end
%=============================================================================

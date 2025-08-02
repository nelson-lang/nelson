%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function rgbplot(map)
  if (size(map, 2) ~= 3)
    error(_('Must be a 3-column colormap matrix.'));
  end
  m = 1:size(map, 1);
  plot(m, map(:, 1), 'r-', m, map(:, 2), 'g-', m, map(:,3), 'b-');
end
%=============================================================================

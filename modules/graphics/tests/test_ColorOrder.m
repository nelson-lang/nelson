%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure();
ax = gca();
assert_isequal(ax.ColorOrderIndex, 1);
hold on
assert_isequal(ax.ColorOrderIndex, 1);
REF = [2, 3, 4, 5, 6, 7, 1, 2, 3];
for r = 1:9
  x = linspace(0,r,500);
  y = sqrt(r.^2-x.^2);
  plot(x, y, 'LineWidth', 2);
  assert_isequal(ax.ColorOrderIndex, REF(r));
end
%=============================================================================

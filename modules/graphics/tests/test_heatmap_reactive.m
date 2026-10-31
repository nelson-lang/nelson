%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% A dot-set of a heatmap property rebuilds the chart in place (reactive), like
% the reference. Verified headless through the web display list.
%=============================================================================
function c = kindCount(f, kind)
  s = jsondecode(__web_display_list__(f));
  p = s.primitives;
  c = 0;
  for k = 1:numel(p)
    if strcmp(p{k}.kind, kind)
      c = c + 1;
    end
  end
end
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
data = [1 2 3; 4 NaN 6; 7 8 9];
h = heatmap(ax, data);
drawnow();
%=============================================================================
% changing the cell-label format rebuilds the labels (render changes)
dl0 = char(__web_display_list__(f));
h.CellLabelFormat = '%0.2f';
drawnow();
dl1 = char(__web_display_list__(f));
assert_istrue(~isequal(dl0, dl1));
%=============================================================================
% GridVisible 'off' removes the grid lines
nBefore = kindCount(f, 'polyline') + kindCount(f, 'segments');
h.GridVisible = 'off';
drawnow();
nAfter = kindCount(f, 'polyline') + kindCount(f, 'segments');
assert_istrue(nAfter < nBefore);
%=============================================================================
% ColorbarVisible 'off' changes the render
dlb = char(__web_display_list__(f));
h.ColorbarVisible = 'off';
drawnow();
dlc = char(__web_display_list__(f));
assert_istrue(~isequal(dlb, dlc));
%=============================================================================
% ColorLimits change remaps the render
dld = char(__web_display_list__(f));
h.ColorLimits = [0 20];
drawnow();
dle = char(__web_display_list__(f));
assert_istrue(~isequal(dld, dle));
%=============================================================================
% the nine cell labels are still drawn
assert_istrue(kindCount(f, 'text') >= 9);
%=============================================================================
close(f);
%=============================================================================

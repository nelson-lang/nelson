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
f = figure('Visible', 'off');
ax = axes('Parent', f);
view(ax, 3);
box(ax, 'on');
plot3(ax, [0 1], [0 1], [0 1]);
%=============================================================================
% default BoxStyle is 'back'
assert_isequal(char(ax.BoxStyle), 'back');
%=============================================================================
function n = sumSegPoints(f)
  s = jsondecode(__web_display_list__(f));
  p = s.primitives;
  n = 0;
  for k = 1:numel(p)
    e = p{k};
    if strcmp(e.kind, 'segments') && isfield(e, 'count')
      n = n + e.count;
    end
  end
end
%=============================================================================
drawnow();
nBack = sumSegPoints(f);
%=============================================================================
% BoxStyle 'full' completes the cube: three extra edges (six segment points)
ax.BoxStyle = 'full';
drawnow();
nFull = sumSegPoints(f);
assert_isequal(nFull - nBack, 6);
%=============================================================================
close(f);
%=============================================================================

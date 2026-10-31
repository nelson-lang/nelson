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
% ClippingStyle 'rectangle' clips to the projected x/y rectangle only (no depth
% clip); '3dbox' (default) clips to the full 3D axes volume.
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
view(ax, 3);
plot3(ax, [0 1], [0 1], [-5 5]);
ax.ZLim = [0 1];
%=============================================================================
% default ClippingStyle is '3dbox'
assert_isequal(char(ax.ClippingStyle), '3dbox');
%=============================================================================
function n = redPolylinePoints(f)
  s = jsondecode(__web_display_list__(f));
  p = s.primitives;
  n = 0;
  for k = 1:numel(p)
    e = p{k};
    if strcmp(e.kind, 'polyline') && isfield(e, 'count')
      n = n + e.count;
    end
  end
end
%=============================================================================
ax.ClippingStyle = '3dbox';
drawnow();
n3 = redPolylinePoints(f);
%=============================================================================
% 'rectangle' drops the depth clip, so it never keeps fewer points than '3dbox'
ax.ClippingStyle = 'rectangle';
assert_isequal(char(ax.ClippingStyle), 'rectangle');
drawnow();
nr = redPolylinePoints(f);
assert_istrue(nr >= n3);
%=============================================================================
close(f);
%=============================================================================

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
f= figure();
f.DrawLater = 'on';
colormaps = {'autumn';
'bone';
'cool';
'copper';
'gray';
'hot';
'jet';
'parula';
'pink';
'spring';
'summer';
'turbo';
'viridis';
'white';
'winter'};

k = 1;
for map = colormaps'
  subplot(4, 4, k);
  surf(peaks);
  ax = gca();
  colormap(ax, map{1});
  title(map{1});
  k = k + 1;
  grid off
  view(-30, 10)
end
f.DrawLater = 'off';
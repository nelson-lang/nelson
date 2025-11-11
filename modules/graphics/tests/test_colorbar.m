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
surf(peaks);
colormap('summer');
colorbar()
%=============================================================================
f = figure();
surf(peaks);
colormap('gray');
cb = colorbar(gca);
%=============================================================================
f = figure();
cba = colorbar();
colorbar('off');
%=============================================================================
locations = { 'north';
'south';
'east';
'west';
'northoutside';
'southoutside';
'eastoutside';
'westoutside'};
for k = 1 : length(locations)
    f = figure();
    surf(peaks);
    colormap('jet');
    colorbar(locations{k});
    title(['Location: ', locations{k}]);
end
%=============================================================================

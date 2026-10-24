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
close all
%=============================================================================
reef = [0.866 0.329 0; 0.329 0.713 1; 0.066 0.443 0.745; ...
  0.996 0.564 0.262; 0.454 0.921 0.854; 0 0.639 0.639];
earth = [0.062 0.258 0.501; 0.717 0.192 0.172; 0.611 0.466 0.125; ...
  0.007 0.345 0.054; 0.862 0.6 0.423; 0.372 0.105 0.031; 1 0.819 0.619];
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
colororder(ax, 'reef');
asserts.isequal(ax.ColorOrder, reef);
asserts.isequal(ax.ColorOrderIndex, 1);
asserts.isequal(colororder(ax), reef);
colororder(ax, [1 0 0; 0 1 0]);
asserts.isequal(ax.ColorOrder, [1 0 0; 0 1 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
b = bar(ones(3, 5));
colororder("earth");
for k = 1:numel(b)
  asserts.isequal(b(k).FaceColorMode, 'auto');
  asserts.isequal(b(k).FaceColor, earth(k, :));
end
close(f);
%=============================================================================
f = figure('Visible', 'off');
b = bar(ones(3, 1), 'r');
asserts.isequal(b.FaceColorMode, 'manual');
colororder("reef");
asserts.isequal(b.FaceColor, [1 0 0]);
asserts.isequal(b.FaceColorMode, 'manual');
close(f);
%=============================================================================
asserts.checkerror('colororder("unknown");', getString(message('Nelson:colororder:unknownColorOrder')));
asserts.checkerror('colororder([1 0]);', getString(message('Nelson:graphics:invalidInputArguments')));
%=============================================================================
close all
%=============================================================================

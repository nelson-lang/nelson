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
% The adv-cli rasterizer must draw the uitree node hierarchy (indented rows with
% their text), not a blank container.
%=============================================================================
fig = uifigure('Visible', 'off', 'Position', [100 100 300 200]);
tr = uitree(fig, 'Position', [40 30 200 140]);
n1 = uitreenode(tr, 'Text', 'Fruits');
uitreenode(n1, 'Text', 'Apple');
uitreenode(n1, 'Text', 'Banana');
expand(tr);
drawnow();
pngFull = [tempname(), '.png'];
exportapp(fig, pngFull);
assert_istrue(isfile(pngFull));
delete(tr);
%=============================================================================
% An empty tree draws no node rows: the populated tree must differ from it.
%=============================================================================
tr2 = uitree(fig, 'Position', [40 30 200 140]);
drawnow();
pngEmpty = [tempname(), '.png'];
exportapp(fig, pngEmpty);
assert_istrue(isfile(pngEmpty));
imgFull = imread(pngFull);
imgEmpty = imread(pngEmpty);
assert_isfalse(isequal(imgFull, imgEmpty));
%=============================================================================
% The node text is actually painted (dark pixels inside the box).
%=============================================================================
R = imgFull(:, :, 1);
G = imgFull(:, :, 2);
B = imgFull(:, :, 3);
darkText = (R < 100) & (G < 100) & (B < 100);
assert_istrue(sum(darkText(:)) > 20);
rmfile(pngFull);
rmfile(pngEmpty);
delete(fig);
close all
%=============================================================================

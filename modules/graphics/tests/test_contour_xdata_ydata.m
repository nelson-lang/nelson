%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
% contour(Z): XData/YData are the implicit index vectors 1:columns and 1:rows.
figure();
Z = peaks(10);
[~, h] = contour(Z);
asserts.istrue(~isempty(get(h, 'XData')));
asserts.istrue(~isempty(get(h, 'YData')));
asserts.isequal(get(h, 'XData'), 1:10);
asserts.isequal(get(h, 'YData'), 1:10);
asserts.isequal(get(h, 'XDataMode'), 'auto');
asserts.isequal(get(h, 'YDataMode'), 'auto');
%=============================================================================
% Non-square matrix: XData follows columns, YData follows rows.
figure();
Zr = Z(1:6, 1:8);
[~, h] = contour(Zr);
asserts.isequal(get(h, 'XData'), 1:8);
asserts.isequal(get(h, 'YData'), 1:6);
%=============================================================================
% contour(x, y, Z) with vectors keeps the supplied coordinates (manual mode).
figure();
x = 11:20;
y = 31:40;
[~, h] = contour(x, y, Z);
asserts.isequal(get(h, 'XData'), x);
asserts.isequal(get(h, 'YData'), y);
asserts.isequal(get(h, 'XDataMode'), 'manual');
asserts.isequal(get(h, 'YDataMode'), 'manual');
%=============================================================================
% contour(X, Y, Z) with matrices keeps the full coordinate grids.
figure();
[XX, YY] = meshgrid(x, y);
[~, h] = contour(XX, YY, Z);
asserts.isequal(size(get(h, 'XData')), [10 10]);
asserts.isequal(get(h, 'XData'), XX);
asserts.isequal(get(h, 'YData'), YY);
%=============================================================================
% TextList tracks the labelled level values when automatic.
figure();
[~, h] = contour(Z);
asserts.isequal(get(h, 'TextList'), get(h, 'LevelList'));
asserts.isequal(get(h, 'TextListMode'), 'auto');
%=============================================================================
% Filled contour: TextList excludes the synthetic base band prepended to
% LevelList.
figure();
[~, h] = contourf(Z);
levelList = get(h, 'LevelList');
textList = get(h, 'TextList');
asserts.istrue(numel(levelList) == numel(textList) + 1);
asserts.isequal(textList, levelList(2:end));
%=============================================================================
% A user-supplied TextList stays manual and is preserved.
figure();
[~, h] = contour(Z);
set(h, 'TextList', [0 2]);
asserts.isequal(get(h, 'TextList'), [0 2]);
asserts.isequal(get(h, 'TextListMode'), 'manual');
%=============================================================================

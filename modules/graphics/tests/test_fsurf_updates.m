%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = fsurf(ax, @(x, y) x + y, [-1 1], 'ShowContours', 'on');
asserts.isequal(char(h.ShowContours), 'on');
asserts.isequal(numel(allchild(ax)), 1);
drawnow();
contourFrame = getframe(f).cdata;
h.ShowContours = 'off';
drawnow();
plainFrame = getframe(f).cdata;
asserts.istrue(sum(abs(double(contourFrame(:)) - double(plainFrame(:)))) > 0);
%=============================================================================
h.MeshDensity = 11;
drawnow();
asserts.isequal(size(h.XData), [11 11]);
asserts.isequal(size(h.YData), [11 11]);
asserts.isequal(size(h.ZData), [11 11]);
h.XRange = [0 2];
h.YRange = [-2 0];
h.Function = @(x, y) x - y;
drawnow();
asserts.isequal(h.XRangeMode, 'manual');
asserts.isequal(h.YRangeMode, 'manual');
asserts.isequal(size(h.ZData), [11 11]);
asserts.istrue(abs(min(h.XData(:))) < 1e-12);
asserts.isapprox(max(h.XData(:)), 2, 1e-12);
asserts.isapprox(min(h.YData(:)), -2, 1e-12);
asserts.istrue(abs(max(h.YData(:))) < 1e-12);
asserts.isapprox(h.ZData(1, 1), 2, 1e-12);
h.XRangeMode = 'auto';
h.YRangeMode = 'auto';
drawnow();
asserts.isequal(h.XRange, [0 2]);
asserts.isequal(h.YRange, [-2 0]);
asserts.isequal(h.XRangeMode, 'auto');
asserts.isequal(h.YRangeMode, 'auto');
%=============================================================================

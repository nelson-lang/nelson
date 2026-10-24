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
f = figure('Visible', 'off');
% rectangle is a primitive graphics object, not a patch.
h = rectangle('Position', [0 0 1 1]);
asserts.isequal(h.Type, 'rectangle');
asserts.isequal(class(h), 'nelson.graphics.primitive.Rectangle');
asserts.isequal(h.Position, [0 0 1 1]);
asserts.isequal(h.Curvature, [0 0]);
asserts.isequal(h.FaceColor, 'none');
asserts.isequal(h.EdgeColor, [0.129411764705882 0.129411764705882 0.129411764705882]);
asserts.isequal(h.EdgeColorMode, 'auto');
asserts.isequal(h.LineStyle, '-');
asserts.isequal(h.LineWidth, 0.5);
asserts.isequal(h.FaceAlpha, 1);
delete(h);
%=============================================================================
% Curvature is queryable and settable; [1 1] draws an ellipse.
h = rectangle('Position', [-1 -1 2 2], 'Curvature', 1, 'LineStyle', '--');
asserts.isequal(h.Type, 'rectangle');
asserts.isequal(h.Curvature, 1);
asserts.isequal(h.LineStyle, '--');
h.Curvature = [1 1];
asserts.isequal(h.Curvature, [1 1]);
h.Curvature = [0.2 0.4];
asserts.isequal(h.Curvature, [0.2 0.4]);
delete(h);
%=============================================================================
% A scalar Curvature is stored as a scalar (circular corners).
h = rectangle('Position', [0 0 2 1], 'Curvature', 0.3);
asserts.isequal(h.Curvature, 0.3);
delete(h);
%=============================================================================
% FaceColor / EdgeColor accept an explicit color spec and 'none'.
h = rectangle('Position', [0 0 1 1], 'FaceColor', [1 0 0], 'EdgeColor', 'none');
asserts.isequal(h.FaceColor, [1 0 0]);
asserts.isequal(h.EdgeColor, 'none');
asserts.isequal(h.EdgeColorMode, 'manual');
delete(h);
%=============================================================================
% Curvature out of range is rejected.
failed = false;
try
  r = rectangle('Curvature', 1.5);
catch
  failed = true;
end
asserts.istrue(failed);
failed = false;
try
  r = rectangle('Curvature', -0.2);
catch
  failed = true;
end
asserts.istrue(failed);
%=============================================================================
% An unrecognized property is rejected.
failed = false;
try
  r = rectangle('NotAProperty', 1);
catch
  failed = true;
end
asserts.istrue(failed);
%=============================================================================
% axes creation still works alongside rectangle.
ax = axes('position', [.1 .2 .3 .4]);
asserts.isequal(ax.Position, [.1 .2 .3 .4]);
close(f);
%=============================================================================

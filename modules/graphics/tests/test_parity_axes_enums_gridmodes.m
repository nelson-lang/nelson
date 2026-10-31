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
% Parity with the reference: enumerated axes properties reject unknown values,
% accept case-insensitive / abbreviated values; grid line widths are positive,
% reset to 0.5 when their mode goes back to auto (alpha and colors keep their
% value); a grid Mode given as auto after its value in one set() call wins.
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
%=============================================================================
enums = {'BoxStyle', 'back', 'full'; ...
  'SortMethod', 'childorder', 'depth'; ...
  'ClippingStyle', '3dbox', 'rectangle'; ...
  'PositionConstraint', 'outerposition', 'innerposition'};
for k = 1:size(enums, 1)
  name = enums{k, 1};
  assert_isequal(ax.(name), enums{k, 2});
  ax.(name) = enums{k, 3};
  assert_isequal(ax.(name), enums{k, 3});
  ax.(name) = upper(enums{k, 2});
  assert_isequal(ax.(name), enums{k, 2});
  assert_istrue(setFails(ax, name, 'bad'));
  assert_isequal(ax.(name), enums{k, 2});
end
ax.SortMethod = 'dep';
assert_isequal(ax.SortMethod, 'depth');
ax.SortMethod = 'childorder';
%=============================================================================
assert_istrue(setFails(ax, 'GridLineWidth', 0));
assert_istrue(setFails(ax, 'GridLineWidth', -1));
assert_istrue(setFails(ax, 'MinorGridLineWidth', 0));
%=============================================================================
% mode back to auto: widths reset to 0.5, alpha and color keep their value
ax.GridLineWidth = 2;
assert_isequal(ax.GridLineWidthMode, 'manual');
ax.GridLineWidthMode = 'auto';
assert_isequal(ax.GridLineWidth, 0.5);
ax.MinorGridLineWidth = 3;
ax.MinorGridLineWidthMode = 'auto';
assert_isequal(ax.MinorGridLineWidth, 0.5);
ax.GridAlpha = 0.6;
ax.GridAlphaMode = 'auto';
assert_isequal(ax.GridAlpha, 0.6);
ax.GridColor = [1 0 0];
ax.GridColorMode = 'auto';
assert_isequal(ax.GridColor, [1 0 0]);
%=============================================================================
% order inside one set() call: the last one wins
ax = axes('Parent', f);
set(ax, 'GridAlpha', 0.9, 'GridAlphaMode', 'auto');
assert_isequal(ax.GridAlphaMode, 'auto');
assert_isequal(ax.GridAlpha, 0.9);
set(ax, 'GridLineWidth', 3, 'GridLineWidthMode', 'auto');
assert_isequal(ax.GridLineWidthMode, 'auto');
assert_isequal(ax.GridLineWidth, 0.5);
ax = axes('Parent', f);
set(ax, 'GridAlphaMode', 'auto', 'GridAlpha', 0.9);
assert_isequal(ax.GridAlphaMode, 'manual');
set(ax, 'GridLineWidthMode', 'auto', 'GridLineWidth', 3);
assert_isequal(ax.GridLineWidthMode, 'manual');
assert_isequal(ax.GridLineWidth, 3);
%=============================================================================
close(f);
%=============================================================================
function failed = setFails(ax, name, value)
  failed = false;
  try
    ax.(name) = value;
  catch
    failed = true;
  end
end
%=============================================================================

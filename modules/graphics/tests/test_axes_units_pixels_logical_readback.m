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
% Converting the axes position to pixel units gives logical pixels, like the
% figure Position, whatever the display scale of a shown figure.
%=============================================================================
close all
%=============================================================================
for visible = {'off', 'on'}
  f = figure('Visible', visible{1}, 'Position', [100 100 400 300]);
  ax = axes('Parent', f, 'Position', [0.25 0.2 0.5 0.6]);
  drawnow();
  fw = f.Position(3);
  fh = f.Position(4);
  ax.Units = 'pixels';
  assert_position(ax.Position, [0.25 * fw, 0.2 * fh, 0.5 * fw, 0.6 * fh]);
  ax.Units = 'normalized';
  assert_position(ax.Position, [0.25 0.2 0.5 0.6]);
  ax.Units = 'pixels';
  ax.Position = [40 30 120 90];
  ax.Units = 'normalized';
  assert_position(ax.Position, [40 / fw, 30 / fh, 120 / fw, 90 / fh]);
  close(f);
end
%=============================================================================
function assert_position(value, expected)
  asserts.isapprox(value, expected, 1e-2);
end
%=============================================================================

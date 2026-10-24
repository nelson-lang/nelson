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
figure();
h = viscircles([0 0], 1);
assert_isequal(class(h), 'nelson.graphics.primitive.Group');
assert_isequal(numel(get(h, 'Children')), 1);
%=============================================================================
figure();
h = viscircles([0 0; 2 2; -1 3], [1; 0.5; 0.25]);
assert_isequal(numel(get(h, 'Children')), 3);
%=============================================================================
figure();
h = viscircles([0 0], 2, 'Color', 'b', 'LineWidth', 1.5, 'EnhanceVisibility', false);
line1 = get(h, 'Children');
assert_isequal(get(line1(1), 'Color'), [0 0 1]);
assert_isequal(get(line1(1), 'LineWidth'), 1.5);
%=============================================================================
figure();
ax = gca();
h = viscircles(ax, [1 1], 0.5);
assert_isequal(ancestor(h, 'axes'), ax);
%=============================================================================
% errors
figure();
assert_checkerror('viscircles([0 0 0], 1)', _('CENTERS must be an N-by-2 matrix.'));
assert_checkerror('viscircles([0 0; 1 1], [1 2 3])', _('RADII must be a scalar or have one value per circle.'));
%=============================================================================

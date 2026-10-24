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
% patch accepts a faces/vertices struct followed by Name/Value properties.
figure();
S = surf2patch(zeros(4, 4));
p = patch(S, 'FaceColor', [0.2 0.6 0.2], 'EdgeColor', 'none', 'Tag', 'ground');
assert_isequal(get(p, 'FaceColor'), [0.2 0.6 0.2]);
assert_isequal(get(p, 'EdgeColor'), 'none');
assert_isequal(get(p, 'Tag'), 'ground');
assert_isequal(size(get(p, 'Faces')), [9 4]);
assert_isequal(size(get(p, 'Vertices')), [16 3]);
%=============================================================================
% a lone struct argument still works (no trailing properties).
figure();
p2 = patch(S);
assert_isequal(size(get(p2, 'Faces')), [9 4]);
%=============================================================================
% struct after an explicit axes parent, with trailing properties.
figure();
ax = gca();
p3 = patch(ax, S, 'FaceColor', 'interp');
assert_isequal(get(p3, 'FaceColor'), 'interp');
assert_isequal(ancestor(p3, 'axes'), ax);
%=============================================================================

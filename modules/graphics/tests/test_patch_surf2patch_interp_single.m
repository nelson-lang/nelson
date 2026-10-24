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
% A surf2patch + patch(..., 'FaceColor', 'interp') pipeline must build a single
% patch with per-vertex interpolated colours, not one patch per face. The
% scalar-per-vertex FaceVertexCData is mapped through the colormap directly by
% the renderer.
f = figure();
S = surf2patch(peaks(20));
assert_isequal(size(S.vertices, 1), 400);
assert_isequal(size(S.faces, 1), 361);
assert_isequal(size(S.facevertexcdata), [400 1]);
p = patch(S, 'FaceColor', 'interp');
% One patch handle, not 361.
assert_isequal(numel(p), 1);
assert_istrue(isscalar(p));
assert_istrue(isgraphics(p, 'patch'));
assert_isequal(get(p, 'FaceColor'), 'interp');
assert_isequal(size(get(p, 'FaceVertexCData')), [400 1]);
% The axes holds a single patch child.
patches = findobj(gca(), 'Type', 'patch');
assert_isequal(numel(patches), 1);
close(f);
%=============================================================================
% A larger grid stays a single patch (the per-face fallback would create ~2500).
f = figure();
p = patch(surf2patch(peaks(50)), 'FaceColor', 'interp');
assert_isequal(numel(p), 1);
patches = findobj(gca(), 'Type', 'patch');
assert_isequal(numel(patches), 1);
close(f);
%=============================================================================

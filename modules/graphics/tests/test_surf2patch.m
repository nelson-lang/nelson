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
% struct output on a plain grid
Z = reshape(1:12, 3, 4);
S = surf2patch(Z);
assert_isequal(size(S.vertices), [12 3]);
assert_isequal(size(S.faces), [6 4]);
assert_isequal(S.facevertexcdata, Z(:));
%=============================================================================
% quad winding on the smallest grid
S2 = surf2patch(zeros(2, 2));
assert_isequal(S2.faces, [1 2 4 3]);
%=============================================================================
% multiple outputs with vector X and Y
[f, v, c] = surf2patch(-10:10, 0:30, zeros(31, 21));
assert_isequal(size(v), [31 * 21, 3]);
assert_isequal(size(f), [30 * 20, 4]);
assert_isequal(v(1, 1), -10);
assert_isequal(v(1, 2), 0);
assert_isequal(size(c), [31 * 21, 1]);
%=============================================================================
% explicit color data
R = surf2patch(zeros(3, 3), magic(3));
M = magic(3);
assert_isequal(R.facevertexcdata, M(:));
%=============================================================================
% triangles option
St = surf2patch(zeros(3, 3), 'triangles');
assert_isequal(size(St.faces), [8 3]);
%=============================================================================
% surface handle input
f1 = figure();
h = surf(peaks(8));
Sh = surf2patch(h);
assert_isequal(size(Sh.vertices), [64 3]);
assert_isequal(size(Sh.faces), [49 4]);
close(f1);
%=============================================================================

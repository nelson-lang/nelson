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
figure('Position', [10 50 600 400]);
surf(peaks);
ax = gca();
F = getframe(ax);
[X, map] = frame2im(F);
asserts.isequal(map, []);
asserts.isequal(ndims(X), 3);
asserts.istrue(size(X, 1) > 200);
asserts.istrue(size(X, 2) > 400);
asserts.isequal(size(X, 3), 3);
asserts.istrue(isa(X, 'uint8'));
%=============================================================================
F = struct('cdata', uint8(ones(4, 5)), 'colormap', [0 0 0; 1 1 1]);
[X, map] = frame2im(F);
asserts.isequal(size(X), [4 5]);
asserts.isequal(map, [0 0 0; 1 1 1]);
F = struct('cdata', uint8(zeros(4, 5, 3)), 'colormap', []);
[X, map] = frame2im(F);
asserts.isequal(size(X), [4 5 3]);
asserts.isequal(size(map), [0 0]);
%=============================================================================

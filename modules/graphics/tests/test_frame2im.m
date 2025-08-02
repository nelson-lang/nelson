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
assert_isequal(map, []);
assert_isequal(ndims(X), 3);
assert_istrue(size(X,1) > 200);
assert_istrue(size(X,2) > 400);
assert_isequal(size(X,3), 3);
assert_istrue(isa(X, 'uint8'));
%=============================================================================

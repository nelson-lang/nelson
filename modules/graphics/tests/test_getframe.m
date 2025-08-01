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
f = figure();
ax1 = subplot(2,1,1);
surf(peaks);
ax2 = subplot(2,1,2);
plot(rand(30));
%=============================================================================
F = getframe(ax1);
assert_istrue(isstruct(F));
f1 = figure();
imshow(F.cdata);
%=============================================================================
F = getframe(ax2);
assert_istrue(isstruct(F));
f1 = figure();
imshow(F.cdata);
%=============================================================================
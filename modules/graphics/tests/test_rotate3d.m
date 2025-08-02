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
ax = gca();
surf(peaks);
rotate3d();
rotate3d(f, 'on');
rotate3d(f, 'off');
rotate3d(ax, 'on');
rotate3d(ax, 'off');
rotate3d('on');
rotate3d('off');

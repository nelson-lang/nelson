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
f = figure('Visible', 'off');
ax = axes('Parent', f);
h = imagesc(ax, magic(3));
asserts.isequal(h.Parent, ax);
asserts.isequal(h.CDataMapping, 'scaled');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = imagesc(1:3, 1:3, magic(3), [-1 1]);
asserts.isequal(h.CDataMapping, 'scaled');
asserts.isequal(gca().CLim, [-1 1]);
close(f);
%=============================================================================

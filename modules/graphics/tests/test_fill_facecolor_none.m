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
x1 = [0 1 1 0];
y1 = [0 0 1 1];
x2 = x1 + 2;
y2 = y1;
h = fill(x1, y1, 'r', x2, y2, 'g', 'LineWidth', 2);
asserts.isequal(size(h), [1 2]);
asserts.istrue(isgraphics(h(1), 'patch'));
asserts.istrue(isgraphics(h(2), 'patch'));
asserts.isequal(h(1).LineWidth, 2);
asserts.isequal(h(2).LineWidth, 2);
%=============================================================================

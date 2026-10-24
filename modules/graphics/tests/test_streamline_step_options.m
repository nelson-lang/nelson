%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
[x, y, u, v] = graphics_streamline_fixture();
f = figure('Visible', 'off');
h = streamline(x, y, u, v, 1, 0, [0.1 100]);
asserts.isequal(numel(h), 1);
asserts.istrue(isgraphics(h(1), 'line'));
close(f);
%=============================================================================

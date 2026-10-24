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
f = figure('Visible', 'off');
verts = {[0 0; 1 1], [1 0; 2 1]};
h = streamline(verts, 'LineWidth', 2);
asserts.isequal(numel(h), 2);
asserts.istrue(isgraphics(h(1), 'line'));
asserts.isequal(h(1).LineWidth, 2);
close(f);
%=============================================================================

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
cleanupFigure = onCleanup(@() close(f)); %#ok<NLS0004>
ax = axes('Parent', f);
label = text(ax, 0.5, 0.5, {'first line'; 'second line'});
label.Visible = 'off';
label.Visible = 'on';
asserts.istrue(isgraphics(label, 'text'));
%=============================================================================

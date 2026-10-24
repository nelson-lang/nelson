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
% Non-regression tests for true super/subscript positioning of the TeX
% interpreter (raised / lowered runs at a smaller font), verified through the
% text Extent so no browser or reference image is required.
%=============================================================================
% A subscript is rendered at a smaller font, so 'H_2O' is narrower than the
% same three glyphs at full size ('H2O'), both under the TeX interpreter.
f = figure();
tfull = text(0.2, 0.5, 'H2O', 'Interpreter', 'tex', 'FontSize', 20);
tsub = text(0.6, 0.5, 'H_2O', 'Interpreter', 'tex', 'FontSize', 20);
drawnow();
asserts.istrue(tsub.Extent(3) < tfull.Extent(3));
close(f);
%=============================================================================

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
% Plain text (no marker) is untouched: identical Extent under 'tex' and 'none'.
f = figure();
tp1 = text(0.2, 0.5, 'plain label', 'Interpreter', 'tex', 'FontSize', 14);
tp2 = text(0.6, 0.5, 'plain label', 'Interpreter', 'none', 'FontSize', 14);
drawnow();
asserts.isequal(tp1.Extent(3), tp2.Extent(3));
asserts.isequal(tp1.Extent(4), tp2.Extent(4));
close(f);
%=============================================================================

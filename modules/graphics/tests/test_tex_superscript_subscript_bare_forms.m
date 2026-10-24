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
% Bare single-character forms are handled too (no braces required).
f = figure();
tbare = text(0.5, 0.5, 'a^2 b_3 c^n', 'Interpreter', 'tex', 'FontSize', 20);
drawnow();
asserts.istrue(tbare.Extent(3) > 0);
asserts.istrue(tbare.Extent(4) > 0);
close(f);
%=============================================================================

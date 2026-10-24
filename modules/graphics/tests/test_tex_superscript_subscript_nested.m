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
% Nested scripts inside braces do not crash and produce a positive extent.
f = figure();
tnest = text(0.5, 0.5, 'e^{-x^2}', 'Interpreter', 'tex', 'FontSize', 20);
drawnow();
asserts.istrue(tnest.Extent(3) > 0);
asserts.istrue(tnest.Extent(4) > 0);
close(f);
%=============================================================================

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
% Multi-line strings with a marker keep the fallback path (no crash).
f = figure();
tml = text(0.5, 0.5, sprintf('line1\nx^2'), 'Interpreter', 'tex', 'FontSize', 14);
drawnow();
asserts.istrue(tml.Extent(4) > 0);
close(f);
%=============================================================================

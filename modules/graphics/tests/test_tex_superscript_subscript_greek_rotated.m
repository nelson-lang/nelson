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
% Greek symbols combined with scripts still resolve, and a rotated label
% (as used for a y-axis label) lays out without error.
f = figure();
tgreek = text(0.5, 0.5, '\alpha^2 \beta_0', 'Interpreter', 'tex', 'FontSize', 20);
trot = text(0.5, 0.5, 'v^2', 'Interpreter', 'tex', 'FontSize', 20, 'Rotation', 90);
drawnow();
asserts.istrue(tgreek.Extent(3) > 0);
asserts.istrue(trot.Extent(4) > 0);
close(f);
%=============================================================================

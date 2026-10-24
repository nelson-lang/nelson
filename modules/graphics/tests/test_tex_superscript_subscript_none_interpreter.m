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
% With the 'none' interpreter the markers stay literal, so the string is wider
% than the TeX layout that shrinks the exponent.
f = figure();
tnone = text(0.2, 0.5, 'x^{222}', 'Interpreter', 'none', 'FontSize', 20);
ttex = text(0.6, 0.5, 'x^{222}', 'Interpreter', 'tex', 'FontSize', 20);
drawnow();
asserts.istrue(ttex.Extent(3) < tnone.Extent(3));
close(f);
%=============================================================================

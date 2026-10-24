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
figure();
Z = peaks(25);
Z(:, 13) = NaN;
[M, h] = contourf(Z, 4, 'LineWidth', 2, 'LabelColor', 'black', 'ZLocation', 'zmin');
asserts.isequal(M, h.ContourMatrix);
asserts.isequal(h.LineWidth, 2);
asserts.isequal(h.LabelColor, [0 0 0]);
asserts.isequal(h.ZLocation, 'zmin');
%=============================================================================

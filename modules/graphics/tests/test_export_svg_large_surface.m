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
svgPath = [tempname(), '.svg'];
f = figure('Visible', 'off', 'Position', [100 100 560 470]);
[X, Y, Z] = peaks(35);
C(:, :, 1) = zeros(35);
C(:, :, 2) = ones(35) .* 0.5;
C(:, :, 3) = ones(35);
surf(X, Y, Z, C);
saveas(f, svgPath);
close(f);
asserts.istrue(isfile(svgPath));
info = dir(svgPath);
asserts.istrue(info.bytes > 0);
svgText = fileread(svgPath);
asserts.istrue(contains(svgText, '<image '));
asserts.istrue(contains(svgText, 'data:image/png;base64,'));
rmfile(svgPath);
%=============================================================================

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
destination = [tempname(), '/'];
mkdir(destination);
f = figure('Visible', 'off', 'Position', [100 100 420 315], 'Color', [1 1 1]);
plot(1:5, [1, 4, 2, 5, 3], 'LineWidth', 2);
title('Vector export');
%=============================================================================
svgFile = [destination, 'figure.svg'];
pdfFile = [destination, 'figure.pdf'];
saveas(f, svgFile);
saveas(f, pdfFile);
close(f);
%=============================================================================
svg = fileread(svgFile);
asserts.istrue(contains(svg, '<svg'));
asserts.istrue(contains(svg, 'viewBox='));
asserts.istrue(contains(svg, '</svg>'));
fid = fopen(pdfFile, 'r');
asserts.istrue(fid > 0);
signature = fread(fid, 4, '*char');
fclose(fid);
asserts.isequal(signature, '%PDF');
%=============================================================================

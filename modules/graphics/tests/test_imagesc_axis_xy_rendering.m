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
f = figure('Visible', 'off');
set(f, 'Position', [100 100 240 240]);
C = zeros(6, 6);
C(1, 1) = 1;
C(6, 1) = 2;
C(1, 6) = 3;
C(6, 6) = 4;
imagesc(C, [0 4]);
axis xy;
axis off;
colormap([0 0.8 0; 1 0 0; 0 0 1; 1 0.5 0; 0 0 0]);
fr = getframe(f);
close(f);

redRows = findColorRows(fr.cdata, [255 0 0]);
blueRows = findColorRows(fr.cdata, [0 0 255]);
orangeRows = findColorRows(fr.cdata, [255 128 0]);
blackRows = findColorRows(fr.cdata, [0 0 0]);
asserts.istrue(mean(redRows) > mean(blueRows));
asserts.istrue(mean(orangeRows) > mean(blackRows));

f = figure('Visible', 'off');
set(f, 'Position', [100 100 240 240]);
imagesc(C, [0 4]);
axis ij;
axis off;
colormap([0 0.8 0; 1 0 0; 0 0 1; 1 0.5 0; 0 0 0]);
fr = getframe(f);
close(f);

redRows = findColorRows(fr.cdata, [255 0 0]);
blueRows = findColorRows(fr.cdata, [0 0 255]);
orangeRows = findColorRows(fr.cdata, [255 128 0]);
blackRows = findColorRows(fr.cdata, [0 0 0]);
asserts.istrue(mean(redRows) < mean(blueRows));
asserts.istrue(mean(orangeRows) < mean(blackRows));
%=============================================================================
function rows = findColorRows(cdata, rgb)
  tol = 12;
  mask = abs(double(cdata(:, :, 1)) - rgb(1)) <= tol ...
    & abs(double(cdata(:, :, 2)) - rgb(2)) <= tol ...
    & abs(double(cdata(:, :, 3)) - rgb(3)) <= tol;
  [rows, ~] = find(mask);
  asserts.istrue(~isempty(rows));
end
%=============================================================================

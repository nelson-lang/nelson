%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
%=============================================================================
close all
reset(groot());
[previousFigurePosition, previousFigureVisible] = graphics_test_reference_figure_position();
referenceFigureCleanup = onCleanup(@() set(groot(), 'DefaultFigurePosition', previousFigurePosition, 'DefaultFigureVisible', previousFigureVisible)); %#ok<NASGU>
%=============================================================================
f = figure();
surf(peaks);
drawnow();
F = getframe(f);
addpath([modulepath('graphics', 'tests')]);
render_assert_not_blank(F.cdata);
warmPixels = F.cdata(:, :, 1) > 180 & F.cdata(:, :, 2) > 150 & F.cdata(:, :, 3) < 120;
asserts.istrue(nnz(warmPixels) > 100);
ax = gca();
asserts.isequal(ax.XLim, [0 50]);
asserts.isequal(ax.XTick, 0:10:50);
asserts.isequal(ax.XTickLabel, {'0'; '10'; '20'; '30'; '40'; '50'});
asserts.isequal(ax.YLim, [0 50]);
asserts.isequal(ax.YTick, 0:10:50);
asserts.isequal(ax.YTickLabel, {'0'; '10'; '20'; '30'; '40'; '50'});
asserts.isequal(ax.ZLim, [-10 10]);
asserts.isequal(ax.ZTick, -10:5:10);
asserts.isequal(ax.ZTickLabel, {'-10'; '-5'; '0'; '5'; '10'});
asserts.isequal(char(ax.XGrid), 'on');
asserts.isequal(char(ax.YGrid), 'on');
asserts.isequal(char(ax.ZGrid), 'on');
asserts.isequal(char(ax.Box), 'off');
asserts.isequal(ax.BoxStyle, 'back');
asserts.isequal(ax.TickDir, 'out');
asserts.isequal(ax.TickDirMode, 'auto');
%=============================================================================
f = figure('Visible', 'off');
ax = subplot(1, 2, 1);
surf(ax, peaks);
drawnow();
asserts.isequal(ax.YLim, [0 60]);
asserts.isequal(ax.YTick, 0:20:60);
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 1074 647]);
subplot(2, 2, 1);
surf(peaks());
axis equal;
view(3);
drawnow();
ax = gca();
asserts.isequal(ax.ZTick, [-5 0 5]);
asserts.isequal(ax.ZTickLabel, {'-5'; '0'; '5'});
frame = getframe(f).cdata;
subplotCrop = frame(1:round(size(frame, 1) * 0.5), 1:round(size(frame, 2) * 0.5), :);
asserts.istrue(colored_pixel_count(subplotCrop) > numel(subplotCrop(:, :, 1)) * 0.01);
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 1074 647]);
t = tiledlayout(2, 2);
ax = nexttile(t);
surf(peaks());
axis equal;
view(3);
nexttile(t);
plot(1:10);
nexttile(t);
plot(1:10);
nexttile(t);
plot(1:10);
drawnow();
asserts.isequal(ax.ZTick, [-5 0 5]);
asserts.isequal(ax.ZTickLabel, {'-5'; '0'; '5'});
frame = getframe(f).cdata;
tileCrop = frame(1:round(size(frame, 1) * 0.5), 1:round(size(frame, 2) * 0.5), :);
asserts.istrue(colored_pixel_count(tileCrop) > numel(tileCrop(:, :, 1)) * 0.01);
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460]);
[x, y, z] = peaks(24);
surf(x, y, z);
view(35, 28);
drawnow();
ax = gca();
asserts.isequal(ax.XLim, [-4 4]);
asserts.isequal(ax.XTick, -4:2:4);
asserts.isequal(ax.YLim, [-4 4]);
asserts.isequal(ax.YTick, -4:2:4);
asserts.isequal(ax.ZLim, [-8 8]);
asserts.isequal(ax.ZTick, -8:2:8);
%=============================================================================
f = figure();
[X, Y] = meshgrid(1:0.5:10, 1:20);
Z = sin(X) + cos(Y);
S = surf(X, Y, Z);
%=============================================================================
P = properties(S);
REF = {'AlphaData';
  'AlphaDataMapping';
  'AmbientStrength';
  'BackFaceLighting';
  'BeingDeleted';
  'BusyAction';
  'CData';
  'CDataMapping';
  'CDataMode';
  'Children';
  'CreateFcn';
  'DeleteFcn';
  'DiffuseStrength';
  'EdgeAlpha';
  'EdgeColor';
  'EdgeLighting';
  'FaceAlpha';
  'FaceColor';
  'FaceLighting';
  'Interruptible';
  'LineStyle';
  'LineWidth';
  'Marker';
  'MarkerEdgeColor';
  'MarkerFaceColor';
  'MarkerSize';
  'MeshStyle';
  'Parent';
  'SpecularColorReflectance';
  'SpecularExponent';
  'SpecularStrength';
  'Tag';
  'Type';
  'UserData';
  'VertexNormals';
  'Visible';
  'XData';
  'XDataMode';
  'YData';
  'YDataMode';
  'ZData'};
asserts.istrue(all(ismember(REF, P)));
%=============================================================================
asserts.isequal(S.Type, 'surface');
%=============================================================================
f = figure('Visible', 'off');
Z = peaks(5);
S = surf(Z);
asserts.istrue(isgraphics(S, 'surface'));
asserts.isequal(size(S.ZData), size(Z));
asserts.isequal(size(S.CData), size(Z));
asserts.isequal(S.XData, 1:5);
asserts.isequal(S.YData, (1:5)');
%=============================================================================
f = figure('Visible', 'off');
C = Z .* 2;
S = surf(Z, C);
asserts.isequal(S.CData, C);
%=============================================================================
f = figure('Visible', 'off');
[X, Y] = meshgrid(1:5, 1:5);
CO = zeros([size(Z), 3]);
CO(:, :, 2) = 0.5;
CO(:, :, 3) = 1;
S = surf(X, Y, Z, CO);
asserts.isequal(size(S.CData), size(CO));
%=============================================================================
f = figure('Visible', 'off');
X = zeros(4, 4);
Y = zeros(4, 4);
Z = zeros(4, 4);
C = magic(3);
S = surf(X, Y, Z, C);
asserts.isequal(size(S.CData), [3 3]);
%=============================================================================
f = figure('Visible', 'off');
ax = gca();
S = surf(ax, X, Y, Z, 'FaceAlpha', 0.5, 'EdgeColor', 'none');
asserts.isequal(S.Parent, ax);
asserts.isapprox(S.FaceAlpha, 0.5, 1e-12);
asserts.isequal(S.EdgeColor, 'none');
%=============================================================================
assertSurfError('surf([1 2 3])', 'Nelson:surface:invalidZArgument');
assertSurfError('surf(1)', 'Nelson:surface:invalidZArgument');
assertSurfError('surf(ones(2, 2), ones(3, 2))', 'Nelson:dimagree:zCSize');
close all
assertSurfError('surf peaks', 'Nelson:graphics:invalidNameValueArguments');
asserts.istrue(isempty(groot().Children));
f = figure('Visible', 'off');
S = surf('ZData', Z);
asserts.isequal(size(S.ZData), size(Z));
close(f);
%=============================================================================
% complex coordinate/color data is drawn using its real part, with a warning
f = figure('Visible', 'off');
Zc = Z + 1i * ones(size(Z));
warning('off', 'Nelson:surface:imaginaryPartsIgnored');
lastwarn('');
S = surf(X, Y, Zc);
[~, warnId] = lastwarn();
warning('on', 'Nelson:surface:imaginaryPartsIgnored');
asserts.isequal(warnId, 'Nelson:surface:imaginaryPartsIgnored');
asserts.istrue(isreal(S.ZData));
asserts.isapprox(S.ZData, Z, 1e-12);
close(f);
%=============================================================================
%=============================================================================
function assertSurfError(command, identifier)
  try
    eval(command);
    asserts.fail([command, ' did not fail.']);
  catch ME
    asserts.isequal(ME.identifier, identifier);
  end
end
%=============================================================================
function n = colored_pixel_count(frame)
  colorSpread = max(frame, [], 3) - min(frame, [], 3);
  n = nnz(colorSpread > 40);
end
%=============================================================================

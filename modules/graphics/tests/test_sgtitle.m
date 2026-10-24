%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
[previousFigurePosition, previousFigureVisible] = graphics_test_reference_figure_position();
referenceFigureCleanup = onCleanup(@() set(groot(), 'DefaultFigurePosition', previousFigurePosition, 'DefaultFigureVisible', previousFigureVisible)); %#ok<NASGU>
f = figure('Visible', 'off');
t = tiledlayout(f, 2, 1);
h = sgtitle(t, 'Shared title', 'Color', 'red');
asserts.istrue(isgraphics(h));
asserts.isfalse(isprop(h, 'Type'));
asserts.isequal(t.Title, h);
asserts.isequal(h.String, 'Shared title');
asserts.isequal(h.Color, [1 0 0]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
t = tiledlayout(f, 1, 2);
nexttile(t);
plot(1:3);
h = sgtitle('Automatic target');
asserts.isequal(t.Title, h);
asserts.isequal(h.String, 'Automatic target');
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax1 = subplot(2, 2, 1);
title('First Subplot');
ax2 = subplot(2, 2, 2);
title('Second Subplot');
ax3 = subplot(2, 2, 3);
title('Third Subplot');
ax4 = subplot(2, 2, 4);
title('Fourth Subplot');
h = sgtitle('Subplot Grid Title');
drawnow();
layoutTolerance = 1e-9;
asserts.isequal(h.FontSize, 15);
asserts.isequal(h.FontSizeMode, 'auto');
asserts.isequal(ax1.FontSize, 9.5);
asserts.isapprox(ax1.Title.FontSize, 10.45, layoutTolerance);
asserts.isapprox(ax1.Position, [0.13 0.56097376913957209 0.334659090909091 0.32780263665714104], layoutTolerance);
asserts.isapprox(ax2.Position, [0.570340909090909 0.56097376913957209 0.334659090909091 0.32780263665714104], layoutTolerance);
asserts.isapprox(ax3.Position, [0.13 0.1056923293379875 0.334659090909091 0.32780263665714104], layoutTolerance);
asserts.isapprox(ax4.Position, [0.570340909090909 0.1056923293379875 0.334659090909091 0.32780263665714104], layoutTolerance);
asserts.isapprox(ax1.Title.Position, [0.500000851400051 1.012323113207547 0.5], layoutTolerance);
asserts.isapprox(ax3.Title.Position, [0.500000851400051 1.012323113207547 0.5], layoutTolerance);
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460]);
ax1 = subplot(2, 2, 1);
title('First Subplot');
ax2 = subplot(2, 2, 2);
title('Second Subplot');
ax3 = subplot(2, 2, 3);
title('Third Subplot');
ax4 = subplot(2, 2, 4);
title('Fourth Subplot');
h = sgtitle('Subplot Grid Title');
asserts.istrue(isgraphics(h));
asserts.isfalse(isgraphics(h, 'text'));
asserts.isfalse(isgraphics(h, 'subplottext'));
asserts.isequal(h.String, 'Subplot Grid Title');
asserts.isequal(h.Type, 'subplottext');
asserts.isequal(h.Tag, '');
asserts.isequal(h.Parent, f);
asserts.isequal(h.HorizontalAlignment, 'center');
asserts.isequal(h.FontSize, 13);
asserts.isequal(h.FontSizeMode, 'auto');
asserts.isequal(h.FontWeight, 'normal');
asserts.isequal(h.FontName, 'Helvetica');
asserts.isapprox(h.Color, [0.129411764705882 0.129411764705882 0.129411764705882], 1e-12);
asserts.isfalse(isprop(h, 'Position'));
asserts.isfalse(isprop(h, 'Units'));
asserts.isfalse(isprop(h, 'VerticalAlignment'));
expectedFieldnames = {'BackgroundColor'; 'Color'; 'EdgeColor'; 'FontAngle'; 'FontName'; 'FontSize'; 'FontWeight'; 'FontUnits'; 'Interpreter'; 'LineStyle'; 'LineWidth'; 'Margin'; 'String'; 'FontSizeMode'; 'HorizontalAlignment'; 'Children'; 'Parent'; 'Visible'; 'HandleVisibility'; 'Selected'; 'SelectionHighlight'; 'HitTest'; 'PickableParts'; 'ButtonDownFcn'; 'ContextMenu'; 'BusyAction'; 'BeingDeleted'; 'Interruptible'; 'CreateFcn'; 'DeleteFcn'; 'Type'; 'Tag'; 'UserData'};
asserts.isequal(fieldnames(h), expectedFieldnames);
asserts.isequal(sgtitle(), h);
asserts.isequal(ax1.Title.String, 'First Subplot');
asserts.isequal(ax2.Title.String, 'Second Subplot');
asserts.isequal(ax3.Title.String, 'Third Subplot');
asserts.isequal(ax4.Title.String, 'Fourth Subplot');
drawnow();
asserts.isequal(h.FontSize, 14.5);
asserts.isequal(h.FontSizeMode, 'auto');
expectedWidth = 0.334659090909091;
expectedHeight = 0.325892948018998;
asserts.isapprox(ax1.FontSize, 9.5, layoutTolerance);
asserts.isapprox(ax1.Title.FontSize, 10.45, layoutTolerance);
asserts.isapprox(ax1.Position, [0.13 0.557705689162709 expectedWidth expectedHeight], layoutTolerance);
asserts.isapprox(ax2.Position, [0.570340909090909 0.557705689162709 expectedWidth expectedHeight], layoutTolerance);
asserts.isapprox(ax3.Position, [0.13 0.105076594691879 expectedWidth expectedHeight], layoutTolerance);
asserts.isapprox(ax4.Position, [0.570340909090909 0.105076594691879 expectedWidth expectedHeight], layoutTolerance);
asserts.isapprox(ax1.OuterPosition, [0.0789601386481802 0.51828540086362 0.392614318090921 0.407668308781855], layoutTolerance);
asserts.isapprox(ax2.OuterPosition, [0.533351661636479 0.51828540086362 0.410565433926187 0.407668308781855], layoutTolerance);
asserts.isapprox(ax3.OuterPosition, [0.0789601386481802 0.0625326453610331 0.392614318090921 0.410791969813611], layoutTolerance);
asserts.isapprox(ax4.OuterPosition, [0.533351661636479 0.0625326453610331 0.410565433926187 0.410791969813611], layoutTolerance);
asserts.isapprox(ax1.XTick, [0 0.5 1], layoutTolerance);
asserts.isapprox(ax1.YTick, 0:0.2:1, layoutTolerance);
asserts.isapprox(ax1.Title.Position, [0.50000085241546 1.01397058823529 0.5], layoutTolerance);
asserts.isapprox(ax2.Title.Position, [0.500002108025275 1.01397058823529 0.5], layoutTolerance);
asserts.isapprox(ax3.Title.Position, [0.50000085241546 1.01389627659574 0.5], layoutTolerance);
asserts.isapprox(ax4.Title.Position, [0.500002108025275 1.01389627659574 0.5], layoutTolerance);
asserts.istrue(ax1.Position(2) + ax1.Position(4) < 0.95);
asserts.istrue(ax2.Position(2) + ax2.Position(4) < 0.95);
asserts.istrue(ax3.Position(2) + ax3.Position(4) < ax1.Position(2));
asserts.istrue(ax4.Position(2) + ax4.Position(4) < ax2.Position(2));
position1 = ax1.Position;
position2 = ax2.Position;
position3 = ax3.Position;
position4 = ax4.Position;
outerPosition1 = ax1.OuterPosition;
outerPosition2 = ax2.OuterPosition;
outerPosition3 = ax3.OuterPosition;
outerPosition4 = ax4.OuterPosition;
h2 = sgtitle('Updated Grid Title');
asserts.isfalse(h2 == h);
asserts.isfalse(isgraphics(h));
asserts.isequal(h2.String, 'Updated Grid Title');
asserts.isequal(sgtitle(), h2);
asserts.isequal(numel(findobj(f, 'Type', 'textbox', 'Tag', '__nelson_sgtitle__')), 0);
asserts.isequal(numel(findobj(f, 'Type', 'subplottext', 'Tag', '__nelson_sgtitle__')), 0);
asserts.isequal(numel(findobj(f, 'Type', 'subplottext')), 1);
asserts.isapprox(ax1.Position, position1, 1e-12);
asserts.isapprox(ax2.Position, position2, 1e-12);
asserts.isapprox(ax3.Position, position3, 1e-12);
asserts.isapprox(ax4.Position, position4, 1e-12);
asserts.isapprox(ax1.OuterPosition, outerPosition1, 1e-12);
asserts.isapprox(ax2.OuterPosition, outerPosition2, 1e-12);
asserts.isapprox(ax3.OuterPosition, outerPosition3, 1e-12);
asserts.isapprox(ax4.OuterPosition, outerPosition4, 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 620 460]);
ax1 = subplot(2, 1, 1);
title('First Subplot');
ax2 = subplot(2, 1, 2);
title('Second Subplot');
sgt = sgtitle('Subplot Grid Title', 'Color', 'red');
asserts.isequal(sgt.Color, [1 0 0]);
asserts.isequal(sgt.FontSize, 13);
asserts.isequal(sgt.FontSizeMode, 'auto');
sgt.FontSize = 20;
asserts.isequal(sgt.FontSize, 20);
asserts.isequal(sgt.FontSizeMode, 'manual');
asserts.isequal(sgt.Color, [1 0 0]);
asserts.isequal(sgt.String, 'Subplot Grid Title');
asserts.isapprox(ax1.FontSize, 9.5, layoutTolerance);
asserts.isapprox(ax1.Title.FontSize, 10.45, layoutTolerance);
asserts.isapprox(ax1.Position, [0.13 0.547209346211096 0.775 0.319759454647153], layoutTolerance);
asserts.isapprox(ax2.Position, [0.13 0.103098992534494 0.775 0.319759454647153], layoutTolerance);
asserts.isapprox(ax1.OuterPosition, [0 0.507789057565393 1 0.401534815756624], layoutTolerance);
asserts.isapprox(ax2.OuterPosition, [0 0.061355744884428 1 0.403857774760988], layoutTolerance);
asserts.isapprox(ax1.XTick, 0:0.1:1, layoutTolerance);
asserts.isapprox(ax1.YTick, 0:0.2:1, layoutTolerance);
asserts.isapprox(ax1.Title.Position, [0.500000638961792 1.01419836956522 0.5], layoutTolerance);
asserts.isapprox(ax2.Title.Position, [0.500000638961792 1.01419836956522 0.5], layoutTolerance);
asserts.istrue(ax1.Position(2) + ax1.Position(4) < 0.95);
close(f);
%=============================================================================

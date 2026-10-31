%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 1074 647]);
h = wordcloud({'alpha', 'beta', 'gamma'}, [5 3 1], 'MaxDisplayWords', 2, 'Color', [1 0 0]);
asserts.istrue(isgraphics(h, 'wordcloud'));
asserts.isequal(h.Type, 'wordcloud');
asserts.isequal(h.Parent, f);
asserts.isequal(size(h), [1 1]);
asserts.isequal(class(h.WordData), 'string');
asserts.isequal(size(h.WordData), [1 3]);
asserts.isequal(cellstr(h.WordData), {'alpha', 'beta', 'gamma'});
asserts.isequal(h.SizeData, [5 3 1]);
asserts.isequal(h.MaxDisplayWords, 2);
asserts.isapprox(h.Color, [1 0 0], 1e-12);
asserts.isapprox(h.HighlightColor, [0.752941176470588 0.298039215686275 0.043137254901961], 1e-12);
asserts.isequal(h.Shape, 'oval');
asserts.isequal(h.LayoutNum, 1);
asserts.isequal(h.SizePower, 0.5);
asserts.isequal(h.FontName, 'Helvetica');
asserts.isequal(char(h.Box), 'off');
asserts.isequal(h.Title, '');
asserts.isequal(h.TitleFontName, 'Helvetica');
asserts.isapprox(h.OuterPosition, [0 0 1 1], 1e-12);
asserts.isapprox(h.InnerPosition, [0 0 1 1], 1e-12);
asserts.isapprox(h.Position, [0 0 1 1], 1e-12);
asserts.isequal(h.Units, 'normalized');
asserts.isequal(h.PositionConstraint, 'outerposition');
asserts.isequal(h.WordVariable, '');
asserts.isequal(h.SizeVariable, '');
asserts.isequal(h.UserData.ChartType, 'wordcloud');
asserts.isequal(h.UserData.Words, {'alpha'; 'beta'; 'gamma'});
asserts.isequal(h.UserData.SizeData, [5 3 1]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = wordcloud(f, {'alpha', 'beta'}, [2 1], 'Position', [0.2 0.3 0.4 0.5]);
asserts.isapprox(h.Position, [0.2 0.3 0.4 0.5], 1e-12);
asserts.isapprox(h.InnerPosition, [0.2 0.3 0.4 0.5], 1e-12);
asserts.isapprox(h.OuterPosition, [0.2 0.3 0.4 0.5], 1e-12);
asserts.isequal(h.PositionConstraint, 'innerposition');
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = wordcloud(f, {'alpha', 'beta'}, [2 1], 'OuterPosition', [0.1 0.2 0.7 0.6]);
asserts.isapprox(h.OuterPosition, [0.1 0.2 0.7 0.6], 1e-12);
asserts.isapprox(h.InnerPosition, [0.1 0.2 0.7 0.6], 1e-12);
asserts.isapprox(h.Position, [0.1 0.2 0.7 0.6], 1e-12);
asserts.isequal(h.PositionConstraint, 'outerposition');
close(f);
%=============================================================================
terms = {'solver'; 'matrix'; 'plot'; 'graphics'; 'signal'; 'model'; 'tests'; 'data'; 'script'; 'analysis'};
counts = [42; 37; 31; 29; 22; 18; 16; 13; 11; 8];
T = table(terms, counts, 'VariableNames', {'Term', 'Count'});
f = figure('Visible', 'off', 'Position', [100 100 1074 647]);
h = wordcloud(f, T, 'Term', 'Count', 'MaxDisplayWords', 8, 'Shape', 'rectangle', 'Color', [0.066 0.443 0.745]);
asserts.isequal(h.Parent, f);
asserts.isequal(h.Type, 'wordcloud');
asserts.isapprox(f.Color, [1 1 1], 1e-12);
asserts.isequal(class(h.WordData), 'string');
asserts.isequal(size(h.WordData), [1 10]);
asserts.isequal(cellstr(h.WordData), terms');
asserts.isequal(h.SizeData, counts');
asserts.isequal(h.MaxDisplayWords, 8);
asserts.isequal(h.Shape, 'rectangle');
asserts.isequal(h.Title, 'Count');
asserts.istrue(istable(h.SourceTable));
asserts.isequal(h.SourceTable, T);
asserts.isequal(h.WordVariable, 'Term');
asserts.isequal(h.SizeVariable, 'Count');
asserts.isequal(h.TitleFontName, 'Helvetica');
asserts.isapprox(h.OuterPosition, [0 0 1 1], 1e-12);
asserts.isapprox(h.InnerPosition, [0 0 1 1], 1e-12);
asserts.isapprox(h.Position, [0 0 1 1], 1e-12);
asserts.isequal(h.Units, 'normalized');
asserts.isequal(h.PositionConstraint, 'outerposition');
asserts.isapprox(h.Color, [0.066 0.443 0.745], 1e-12);
asserts.isequal(h.UserData.Words, terms);
asserts.isequal(h.UserData.SizeData, counts');
texts = findall(f, 'Type', 'text');
solverText = texts(strcmp({texts.String}, 'solver'));
matrixText = texts(strcmp({texts.String}, 'matrix'));
plotText = texts(strcmp({texts.String}, 'plot'));
graphicsText = texts(strcmp({texts.String}, 'graphics'));
signalText = texts(strcmp({texts.String}, 'signal'));
modelText = texts(strcmp({texts.String}, 'model'));
testsText = texts(strcmp({texts.String}, 'tests'));
dataText = texts(strcmp({texts.String}, 'data'));
titleText = texts(strcmp({texts.String}, 'Count'));
asserts.isapprox(solverText.Position, [-50 105 0], 1e-12);
asserts.isapprox(solverText.FontSize, 87.9953188427153, 1e-9);
asserts.isapprox(matrixText.Position, [-321 -97 0], 1e-12);
asserts.isapprox(matrixText.FontSize, 79.6148122862662, 1e-9);
asserts.isapprox(plotText.Position, [-298 -226 0], 1e-12);
asserts.isapprox(graphicsText.Position, [-245 250 0], 1e-12);
asserts.isapprox(signalText.Position, [151 249 0], 1e-12);
asserts.isapprox(modelText.Position, [414 135 0], 1e-12);
asserts.isapprox(testsText.Position, [-200 2 0], 1e-12);
asserts.isapprox(dataText.Position, [-352 108 0], 1e-12);
asserts.isapprox(titleText.Position, [0.000510976140504 302.892529623905 0.5], 1e-9);
asserts.isequal(titleText.FontSize, 11);
asserts.isfalse(isempty(f.SizeChangedFcn));
figPosition = f.Position;
f.Position = [figPosition(1), figPosition(2), 1000, 360];
feval(f.SizeChangedFcn, f, []);
asserts.isapprox(solverText.Position, [-46.5 44 0], 1e-12);
asserts.isapprox(solverText.FontSize, 107.316363320265, 1e-9);
asserts.isapprox(testsText.Position, [-444.5 191 0], 1e-12);
asserts.isapprox(dataText.Position, [495.5 116 0], 1e-12);
asserts.isapprox(titleText.Position, [0.000637596130446 214.458333333333 0.5], 1e-9);
asserts.isequal(titleText.FontSize, 11);
close(f);
%=============================================================================
f = figure('Visible', 'off');
h = wordcloud(f, {'red', 'red', 'blue'}, 'Shape', 'rectangle');
asserts.isequal(h.Parent, f);
asserts.isequal(h.Type, 'wordcloud');
asserts.isapprox(h.Color, [0.380392156862745 0.380392156862745 0.380392156862745], 1e-12);
asserts.isequal(h.UserData.Options.Shape, 'rectangle');
asserts.isequal(h.UserData.SizeData, [2 1]);
close(f);
%=============================================================================
f = figure('Visible', 'off', 'Color', [0.2 0.3 0.4]);
h = wordcloud(f, {'red', 'red', 'blue'}, 'Shape', 'rectangle');
asserts.isequal(h.Parent, f);
asserts.isapprox(f.Color, [0.2 0.3 0.4], 1e-12);
close(f);
%=============================================================================
f = figure('Visible', 'off');
ax = axes();
asserts.checkerror('wordcloud(ax, {''red'', ''blue''}, [2 1]);', getString(message('Nelson:wordcloud:wordCloudChartCannotBeA')));
close(f);
%=============================================================================
asserts.checkerror('wordcloud();', getString(message('Nelson:narginchk:notEnoughInputs')));
asserts.checkerror('wordcloud({''a''}, {1});', getString(message('Nelson:validators:mustBeNumeric', 'Word sizes')));
asserts.checkerror('wordcloud({''a'', ''b''}, [1]);', getString(message('Nelson:dimagree:wordsSizesNumberElements')));
asserts.checkerror('wordcloud({''a''}, [1], ''BadName'', 1);', getString(message('Nelson:graphics:unsupportedProperty', 'wordcloud')));
%=============================================================================

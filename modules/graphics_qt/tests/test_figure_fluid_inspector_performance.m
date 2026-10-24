%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--GUI MODE-->
settingsPath = [fileparts(fileparts(fileparts(modulepath('graphics', 'tests')))) '/temp/qt-fluid-inspector.ini'];
previousSettings = getenv('NELSON_QT_FIGURE_SETTINGS');
settings = fopen(settingsPath, 'wt');
fprintf(settings, '[FigureWorkspace]\nproperties=true\nnavigation=false\ndetails=false\nwidth=320\n');
fclose(settings);
setenv('NELSON_QT_FIGURE_SETTINGS', settingsPath);
fluidAutoStart = true;
fluidFrameCount = 60;
profile('clear'); profile('on');
overall = tic();
run([modulepath('graphics', 'root') '/examples/fluid_simulation_2d.m']);
elapsed = toc(overall); profile('off');
stats = profile('info'); profile('clear');
queries = stats(strcmp({stats.FunctionName}, '__web_figure_state__'));
asserts.istrue(sum([queries.NumCalls]) <= 2);
asserts.isfalse(any(strcmp({stats.FunctionName}, '__web_figure_action__')));
asserts.istrue(elapsed < 15, sprintf('Fluid animation took %.3f seconds.', elapsed));
asserts.isequal(frameIndex, fluidFrameCount);
asserts.istrue(all(isfinite(fluid.vx(:))));
asserts.istrue(all(isfinite(fluid.vy(:))));
close(figureHandle);
setenv('NELSON_QT_FIGURE_SETTINGS', previousSettings);

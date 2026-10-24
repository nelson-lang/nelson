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
settingsPath = [fileparts(fileparts(fileparts(modulepath('graphics', 'tests')))) '/temp/qt-funnel-performance.ini'];
if isfile(settingsPath), delete(settingsPath); end
previousSettings = getenv('NELSON_QT_FIGURE_SETTINGS');
setenv('NELSON_QT_FIGURE_SETTINGS', settingsPath);
funnelFigureVisible = 'on';
funnelBatch = true;
funnelParticleCount = 500;
funnelFrameCount = 30;
funnelMaxFrames = 30;
profile('clear'); profile('on');
overall = tic();
run([modulepath('graphics', 'root') '/examples/particle_funnel.m']);
elapsed = toc(overall);
profile('off');
stats = profile('info'); profile('clear');
asserts.isfalse(any(strcmp({stats.FunctionName}, '__web_figure_state__')));
asserts.isfalse(any(strcmp({stats.FunctionName}, '__web_figure_action__')));
asserts.istrue(elapsed < 15, sprintf('Particle funnel took %.3f seconds.', elapsed));
asserts.isequal(size(particleXHistory), [500 30]);
asserts.istrue(all(isfinite(particleXHistory(:))));
asserts.istrue(all(isfinite(particleYHistory(:))));
asserts.isequal(startStopButton.String, 'Start');
close(figureHandle);
setenv('NELSON_QT_FIGURE_SETTINGS', previousSettings);

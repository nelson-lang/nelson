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
benchmarkProbePath = getenv('NELSON_QT_WORKSPACE_PROBE');
skip_testsuite(isempty(benchmarkProbePath) || ~isfile(benchmarkProbePath), 'Build the Qt workspace probe first.');
benchmarkLibrary = dlopen(benchmarkProbePath);
benchmarkProbe = dlsym(benchmarkLibrary, 'figureWorkspaceProbe', 'int32', {'cstring', 'cstring'});
benchmarkDirectory = fullfile(modulepath('graphics', 'root'), '..', '..', 'temp', 'qt-fluid-fps');
if ~isfolder(benchmarkDirectory), mkdir(benchmarkDirectory); end
benchmarkPreviousSettings = getenv('NELSON_QT_FIGURE_SETTINGS');
benchmarkSettingsPath = fullfile(benchmarkDirectory, 'workspace.ini');
benchmarkOrder = [false true; true false; false true];
benchmarkFps = zeros(3, 2);
benchmarkResults = cell(3, 2);
profile('off'); profile('clear');
try
  for benchmarkTrial = 1:3
    for benchmarkCase = 1:2
      benchmarkOpen = benchmarkOrder(benchmarkTrial, benchmarkCase);
      benchmarkNames = {'closed', 'open'};
      benchmarkName = benchmarkNames{1 + benchmarkOpen};
      benchmarkBooleans = {'false', 'true'};
      benchmarkSettings = fopen(benchmarkSettingsPath, 'wt');
      fprintf(benchmarkSettings, '[FigureWorkspace]\nproperties=%s\nnavigation=false\ndetails=false\nwidth=320\n', benchmarkBooleans{1 + benchmarkOpen});
      fclose(benchmarkSettings);
      setenv('NELSON_QT_FIGURE_SETTINGS', benchmarkSettingsPath);
      benchmarkPath = fullfile(benchmarkDirectory, sprintf('trial-%d-%s.json', benchmarkTrial, benchmarkName));
      if isfile(benchmarkPath), delete(benchmarkPath); end
      benchmarkRequest = struct('operation', 'armFrames', 'path', benchmarkPath, ...
        'canvasWidth', 760, 'canvasHeight', 480, 'warmupFrames', 60, 'measuredFrames', 300);
      asserts.isequal(dlcall(benchmarkProbe, '2D fluid simulation', jsonencode(benchmarkRequest)), int32(1));
      fluidAutoStart = true;
      fluidFigureVisible = 'on';
      fluidFrameCount = 600;
      fluidGridSize = 128;
      fluidJacobiIterations = 20;
      fluidObstacle = 'Vertical plate';
      fluidJetCount = 1;
      fluidColorMode = 'Smoke';
      fluidFineSmoke = true;
      run([modulepath('graphics', 'root') '/examples/fluid_simulation_2d.m']);
      benchmarkSimulationSeconds = toc(clockStart);
      asserts.istrue(isfile(benchmarkPath), 'Presentation sample did not complete.');
      benchmarkReport = jsondecode(fileread(benchmarkPath));
      asserts.istrue(isempty(benchmarkReport.error), benchmarkReport.error);
      asserts.isequal(benchmarkReport.properties, benchmarkOpen);
      asserts.isequal([benchmarkReport.canvasWidth benchmarkReport.canvasHeight], [760 480]);
      asserts.isequal(benchmarkReport.frames, 300);
      asserts.istrue(benchmarkReport.fps > 0 && isfinite(benchmarkReport.fps));
      asserts.isequal(frameIndex, fluidFrameCount);
      benchmarkReport.trial = benchmarkTrial;
      benchmarkReport.simulationFrames = frameIndex;
      benchmarkReport.simulationSeconds = benchmarkSimulationSeconds;
      benchmarkReport.simulationFps = frameIndex / benchmarkSimulationSeconds;
      benchmarkOutput = fopen(benchmarkPath, 'wt');
      fprintf(benchmarkOutput, '%s\n', jsonencode(benchmarkReport));
      fclose(benchmarkOutput);
      benchmarkFps(benchmarkTrial, 1 + benchmarkOpen) = benchmarkReport.fps;
      benchmarkResults{benchmarkTrial, 1 + benchmarkOpen} = benchmarkReport;
      fprintf('Fluid trial %d properties %s: %.3f presented frames/s, %.3f simulation steps/s\n', ...
        benchmarkTrial, benchmarkName, benchmarkReport.fps, benchmarkReport.simulationFps);
      close(figureHandle);
      drawnow(); pause(0.2);
    end
  end
  benchmarkClosed = median(benchmarkFps(:, 1));
  benchmarkOpen = median(benchmarkFps(:, 2));
  benchmarkPairedLoss = benchmarkFps(:, 1) - benchmarkFps(:, 2);
  benchmarkSummary = struct('closedFps', benchmarkClosed, 'openFps', benchmarkOpen, ...
    'lossFps', benchmarkClosed - benchmarkOpen, 'trials', benchmarkFps, ...
    'pairedLossFps', benchmarkPairedLoss, 'medianPairedLossFps', median(benchmarkPairedLoss), ...
    'canvasWidth', 760, 'canvasHeight', 480, 'warmupFrames', 60, ...
    'measuredFrames', 300, 'profilerEnabled', false);
  benchmarkOutput = fopen(fullfile(benchmarkDirectory, 'summary.json'), 'wt');
  fprintf(benchmarkOutput, '%s\n', jsonencode(benchmarkSummary));
  fclose(benchmarkOutput);
  fprintf('Median: closed %.3f FPS, open %.3f FPS, loss %.3f FPS\n', benchmarkClosed, benchmarkOpen, benchmarkClosed - benchmarkOpen);
catch benchmarkFailure
  setenv('NELSON_QT_FIGURE_SETTINGS', benchmarkPreviousSettings);
  rethrow(benchmarkFailure);
end
setenv('NELSON_QT_FIGURE_SETTINGS', benchmarkPreviousSettings);
dlclose(benchmarkLibrary);

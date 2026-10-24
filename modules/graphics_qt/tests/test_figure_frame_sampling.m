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
probePath = getenv('NELSON_QT_WORKSPACE_PROBE');
skip_testsuite(isempty(probePath) || ~isfile(probePath), 'Build the Qt workspace probe first.');
library = dlopen(probePath);
probe = dlsym(library, 'figureWorkspaceProbe', 'int32', {'cstring', 'cstring'});
samplePath = fullfile(modulepath('graphics', 'root'), '..', '..', 'temp', 'qt-frame-sampling.json');
if isfile(samplePath), delete(samplePath); end
request = struct('operation', 'armFrames', 'path', samplePath, ...
  'canvasWidth', 760, 'canvasHeight', 480, 'warmupFrames', 2, 'measuredFrames', 10);
invalid = request; invalid.measuredFrames = 0;
asserts.isequal(dlcall(probe, 'Qt frame sampling', jsonencode(invalid)), int32(-4));
asserts.isequal(dlcall(probe, 'Qt frame sampling', jsonencode(request)), int32(1));
f = figure('Name', 'Qt frame sampling', 'NumberTitle', 'off', 'Visible', 'on');
curve = plot(1:10);
for index = 1:150
  curve.YData = sin((1:10) + index);
  drawnow(); pause(0.02);
  if isfile(samplePath), break; end
end
asserts.istrue(isfile(samplePath));
sample = jsondecode(fileread(samplePath));
asserts.istrue(isempty(sample.error), sample.error);
asserts.isequal(sample.frames, 10);
asserts.isequal([sample.canvasWidth sample.canvasHeight], [760 480]);
asserts.istrue(sample.seconds > 0 && sample.fps > 0);
asserts.isapprox(sample.fps, sample.frames / sample.seconds, 1e-8);
close(f); drawnow(); pause(0.1);
dlclose(library);

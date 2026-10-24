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
settingsPath = [fileparts(fileparts(fileparts(modulepath('graphics', 'tests')))) '/temp/qt-colorbar-performance.ini'];
if isfile(settingsPath), delete(settingsPath); end
previousSettings = getenv('NELSON_QT_FIGURE_SETTINGS');
setenv('NELSON_QT_FIGURE_SETTINGS', settingsPath);
library = dlopen(probePath);
probe = dlsym(library, 'figureWorkspaceProbe', 'int32', {'cstring', 'cstring'});
tic();
run([modulepath('graphics', 'root'), '/examples/colorbar/demo_colorbar.m']);
creationSeconds = toc();
asserts.istrue(creationSeconds < 30, sprintf('Colorbar creation took %.3f seconds.', creationSeconds));
f = gcf();
f.Name = 'Qt Colorbar performance';
f.NumberTitle = 'off';
titleText = 'Qt Colorbar performance';
drawnow();
dlcall(probe, titleText, '{"operation":"action","name":"figurePropertiesAction"}');
dlcall(probe, titleText, '{"operation":"refresh"}');
for index = 1:300
  drawnow(); pause(0.01);
  if index >= 10 && dlcall(probe, titleText, '{"operation":"idle"}') == 1
    break;
  end
end
asserts.isequal(dlcall(probe, titleText, '{"operation":"idle"}'), int32(1));
asserts.isequal(dlcall(probe, titleText, '{"operation":"visible","name":"figureProperties"}'), int32(1));
handle = __web_graphics_handle__(f, 'number');
tic();
for index = 1:20
  state = __web_figure_state__(handle);
end
snapshotSeconds = toc();
asserts.istrue(snapshotSeconds < 5, sprintf('Colorbar snapshots took %.3f seconds.', snapshotSeconds));
asserts.isequal(numel(state.axes), 4);
close(f); dlclose(library);
setenv('NELSON_QT_FIGURE_SETTINGS', previousSettings);

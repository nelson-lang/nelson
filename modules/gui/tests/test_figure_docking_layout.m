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
skip_testsuite(isempty(probePath) || ~isfile(probePath), 'Build the Qt workspace probe and set NELSON_QT_WORKSPACE_PROBE.');
library = dlopen(probePath);
probe = dlsym(library, 'figureWorkspaceProbe', 'int32', {'cstring', 'cstring'});
titleText = 'Qt docking layout';
figureObject = figure('Name', titleText, 'NumberTitle', 'off', 'WindowStyle', 'normal', 'Position', [100 100 1000 600]);
surf(peaks(12));
for name = {'figurePropertiesAction', 'figureShowNavigationAction', 'figureShowDetailsAction'}
  dlcall(probe, titleText, jsonencode(struct('operation', 'action', 'name', name{1})));
end
settle();
request = jsonencode(struct('operation', 'layout', 'path', ...
  [fileparts(fileparts(fileparts(modulepath('graphics', 'tests')))) '/temp/qt-docking-layout-state.json']));
asserts.isequal(dlcall(probe, titleText, request), int32(1));
initialGeometry = __figure_native_geometry__(figureObject);
ax = gca();
initialView = ax.View;
for cycle = 1:3
  if cycle == 3
    figureObject.WindowStyle = 'docked';
  else
    asserts.isequal(dlcall(probe, titleText, '{"operation":"action","name":"figureDockAction"}'), int32(1));
  end
  settle();
  asserts.isequal(dlcall(probe, titleText, request), int32(1));
  if cycle == 1
    asserts.isequal(dlcall(probe, titleText, '{"operation":"undockMenu"}'), int32(1));
  elseif cycle == 2
    asserts.isequal(dlcall(probe, titleText, '{"operation":"action","name":"figureDockAction"}'), int32(1));
  else
    figureObject.WindowStyle = 'normal';
  end
  settle();
  asserts.isequal(dlcall(probe, titleText, request), int32(1));
  restored = __figure_native_geometry__(figureObject);
  asserts.isapprox(restored(3:4), initialGeometry(3:4), 3);
  asserts.isequal(ax.View, initialView);
  asserts.isequal(dlcall(probe, titleText, '{"operation":"visible","name":"figurePropertiesDock"}'), int32(1));
  asserts.isequal(dlcall(probe, titleText, '{"operation":"visible","name":"figureNavigationToolbar"}'), int32(1));
  asserts.isequal(dlcall(probe, titleText, '{"operation":"visible","name":"figureDetails"}'), int32(1));
  asserts.isequal(dlcall(probe, titleText, '{"operation":"visible","name":"figureLegendToolbar"}'), int32(1));
  asserts.isequal(dlcall(probe, titleText, '{"operation":"sliderValue","name":"figureAzimuthSlider"}'), int32(round(initialView(1) * 2)));
  asserts.isequal(dlcall(probe, titleText, '{"operation":"sliderValue","name":"figureElevationSlider"}'), int32(round(initialView(2) * 2)));
end
dlcall(probe, titleText, jsonencode(struct('operation', 'snapshot', 'path', ...
  [fileparts(fileparts(fileparts(modulepath('graphics', 'tests')))) '/temp/qt-docking-layout.png'])));
close(figureObject);
dlclose(library);

function settle()
  for index = 1:40
    drawnow();
    pause(0.01);
  end
end

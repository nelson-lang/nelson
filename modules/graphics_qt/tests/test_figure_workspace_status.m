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
probePath = getenv('NELSON_QT_WORKSPACE_PROBE');
skip_testsuite(isempty(probePath) || ~isfile(probePath), 'Build the Qt workspace probe and set NELSON_QT_WORKSPACE_PROBE.');
library = dlopen(probePath);
probe = dlsym(library, 'figureWorkspaceProbe', 'int32', {'cstring', 'cstring'});
titleText = 'Qt workspace refresh status';
settingsPath = [fileparts(fileparts(fileparts(modulepath('graphics', 'tests')))) '/temp/qt-figure-status.ini'];
settings = fopen(settingsPath, 'wt');
fprintf(settings, '[FigureWorkspace]\nnavigation=false\ndetails=false\nproperties=false\nwidth=320\n');
fclose(settings);
previousSettings = getenv('NELSON_QT_FIGURE_SETTINGS');
setenv('NELSON_QT_FIGURE_SETTINGS', settingsPath);
figureObject = figure('Name', titleText, 'NumberTitle', 'off', 'WindowStyle', 'normal');
surf(peaks(8));
invoke(probe, titleText, struct('operation', 'action', 'name', 'figurePropertiesAction'));
drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'statusEmpty')), int32(1));
invoke(probe, titleText, struct('operation', 'select', 'handle', __web_graphics_handle__(get(gca(), 'Title'), 'number')));
drain(probe, titleText);
invoke(probe, titleText, struct('operation', 'propertyRetained', 'name', 'String', 'mark', true));
title(gca(), 'Live fluid title');
invoke(probe, titleText, struct('operation', 'refresh')); drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'propertyRetained', 'name', 'String')), int32(1));
for index = 1:8
  asserts.isequal(invoke(probe, titleText, struct('operation', 'refresh')), int32(1));
  drain(probe, titleText);
  asserts.isequal(invoke(probe, titleText, struct('operation', 'statusEmpty')), int32(1));
end
invoke(probe, titleText, struct('operation', 'select', 'handle', __web_graphics_handle__(gca(), 'number')));
drain(probe, titleText);
invoke(probe, titleText, struct('operation', 'property', 'name', 'XScale', 'value', 'invalid'));
asserts.isequal(invoke(probe, titleText, struct('operation', 'statusEmpty')), int32(0));
drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'statusEmpty')), int32(0));
asserts.isequal(invoke(probe, titleText, struct('operation', 'refresh')), int32(1));
drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'statusEmpty')), int32(0));
invoke(probe, titleText, struct('operation', 'property', 'name', 'XScale', 'value', 'log'));
drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'statusEmpty')), int32(1));
asserts.isequal(gca().XScale, 'log');
previousView = gca().View;
view(gca(), 15, 25);
invoke(probe, titleText, struct('operation', 'backgroundRefresh')); drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'sliderValue', 'name', 'figureAzimuthSlider', 'expected', round(previousView(1) * 2))), int32(1));
invoke(probe, titleText, struct('operation', 'refresh')); drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'sliderValue', 'name', 'figureAzimuthSlider')), int32(30));
invoke(probe, titleText, struct('operation', 'action', 'name', 'figurePropertiesAction'));
view(gca(), 30, 40);
asserts.isequal(invoke(probe, titleText, struct('operation', 'backgroundRefresh')), int32(0));
invoke(probe, titleText, struct('operation', 'action', 'name', 'figurePropertiesAction'));
drain(probe, titleText);
asserts.isequal(invoke(probe, titleText, struct('operation', 'sliderValue', 'name', 'figureAzimuthSlider')), int32(60));
close(figureObject);
dlclose(library);
setenv('NELSON_QT_FIGURE_SETTINGS', previousSettings);

function result = invoke(probe, titleText, request)
  result = dlcall(probe, titleText, jsonencode(request));
  asserts.istrue(result >= 0, sprintf('Qt probe failed: %s (%d)', jsonencode(request), result));
end

function drain(probe, titleText)
  for index = 1:300
    drawnow();
    pause(0.01);
    if index >= 3 && dlcall(probe, titleText, jsonencode(struct('operation', 'idle'))) == 1
      return;
    end
  end
  error('Qt figure action did not settle.');
end

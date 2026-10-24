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
run([modulepath('graphics', 'root') '/examples/uicontrol/complete_uicontrol_demo.m']);
titleText = 'Complete UI controls';
controls = [state.amplitudeEdit state.frequencySlider state.markerCheckbox state.colorList];
positions = zeros(numel(controls), 4);
for index = 1:numel(controls), positions(index, :) = controls(index).Position; end
for cycle = 1:4
  dlcall(probe, titleText, '{"operation":"action","name":"figurePropertiesAction"}');
  for index = 1:30, drawnow(); pause(0.01); end
  width = figureHandle.Position(3);
  asserts.istrue(state.axes.Position(1) * width >= 330 - 1e-8);
  asserts.istrue(state.statusText.Position(1) + state.statusText.Position(3) <= width);
  for index = 1:numel(controls)
    asserts.isequal(controls(index).Position, positions(index, :));
  end
end
close(figureHandle); dlclose(library);

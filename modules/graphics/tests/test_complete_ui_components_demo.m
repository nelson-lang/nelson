%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
% <--ADV-CLI MODE-->
examplePath = [modulepath('graphics', 'root'), ...
  '/examples/uicontrol/complete_ui_components_demo.m'];
run(examplePath);
figureHandle = findobj('Type', 'figure', 'Tag', 'completeUiComponentsFigure');
assert_istrue(isscalar(figureHandle));
state = figureHandle.UserData;
assert_istrue(isgraphics(state.axes, 'axes'));
assert_isequal(numel(findobj(figureHandle, 'Type', 'uicontextmenu')), 1);
assert_istrue(numel(findobj(figureHandle, 'Type', 'uimenu')) >= 3);

state.amplitudeField.Value = 2;
state.frequencySlider.Value = 3;
state.waveformDropdown.Value = 'Cosine';
completeUiComponentsUpdate(state.frequencySlider, []);
assert_isapprox(state.plot.YData(1), 2, 1e-12);
assert_isequal(state.frequencyGauge.Value, 3);
assert_istrue(contains(state.statusLabel.Text, 'Cosine'));

completeUiComponentsReset(state.resetButton, []);
assert_isequal(state.amplitudeField.Value, 1);
assert_isequal(state.frequencySlider.Value, 1);
assert_isequal(state.waveformDropdown.Value, 'Sine');
delete(state.componentFigure);
delete(figureHandle);
%=============================================================================

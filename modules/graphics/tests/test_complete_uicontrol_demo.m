%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
examplePath = [modulepath('graphics', 'root'), ...
  '/examples/uicontrol/complete_uicontrol_demo.m'];
run(examplePath);
figureHandle = findobj('Type', 'figure', 'Tag', 'completeUiControlFigure');
assert_istrue(isscalar(figureHandle));
state = figureHandle.UserData;
assert_istrue(isgraphics(state.plot, 'line'));

state.amplitudeEdit.String = '2.5';
state.frequencySlider.Value = 3;
state.cosineRadio.Value = 1;
completeUiControlUpdate(state.cosineRadio, []);
assert_isequal(state.sineRadio.Value, 0);
assert_isequal(state.cosineRadio.Value, 1);
assert_isapprox(state.plot.YData(1), 2.5, 1e-12);
assert_istrue(contains(state.statusText.String, 'cosine'));

completeUiControlReset(state.resetButton, []);
assert_isequal(state.frequencySlider.Value, 1);
assert_isequal(state.sineRadio.Value, 1);
assert_isequal(state.plot.LineWidth, 2);
for width = [595 920 1200]
  position = figureHandle.Position;
  position(3) = width;
  figureHandle.Position = position;
  completeUiControlLayout(figureHandle, []);
  currentWidth = figureHandle.Position(3);
  assert_istrue(state.axes.Position(1) * currentWidth >= 330 - 1e-9);
  assert_istrue((state.axes.Position(1) + state.axes.Position(3)) * currentWidth <= currentWidth);
  assert_istrue(state.statusText.Position(1) + state.statusText.Position(3) <= currentWidth);
end
close(figureHandle);
%=============================================================================

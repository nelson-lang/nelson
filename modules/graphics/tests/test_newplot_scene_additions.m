%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 6-->
% <--ADV-CLI MODE-->
%=============================================================================
% A call that adds to an existing scene must never start a new plot: it draws
% on top of whatever is already there, whatever the NextPlot mode says. Only a
% call that does start a new plot applies the NextPlot policy, and 'add' (that
% is, hold on) then keeps the existing children in place.
%
% The witness is found back by Tag and never by handle: a freed handle id is
% handed out again to the next object created, so isgraphics() on a stale
% handle answers for whatever object now holds that id.
%
% The calls go through eval() so that they run with nargout == 0: inside the
% body of an anonymous function they would run with nargout == 1, and the ones
% that can return coordinates would never draw anything.
%=============================================================================
[XX, YY] = meshgrid(1:4, 1:4);
UU = ones(4, 4);
VV = ones(4, 4);
[X3, Y3, Z3] = meshgrid(1:4, 1:4, 1:4);
U3 = ones(4, 4, 4);
V3 = ones(4, 4, 4);
W3 = ones(4, 4, 4);
V3D = reshape(1:64, [4 4 4]);
modes = {'replace', 'add', 'replacechildren'};
%=============================================================================
sceneAdditions = {...
  'streamline(ax1, {[1 1; 2 2]})', ...
  'streamline(ax1, XX, YY, UU, VV, 1.5, 1.5)', ...
  'coneplot(ax1, X3, Y3, Z3, U3, V3, W3, 2, 2, 2)', ...
  'contourslice(ax1, V3D, 2, 2, 2)', ...
  'streamribbon(ax1, X3, Y3, Z3, U3, V3, W3, 1.5, 1.5, 1.5)', ...
  'streamtube(ax1, X3, Y3, Z3, U3, V3, W3, 1.5, 1.5, 1.5)', ...
  'streamslice(ax1, XX, YY, UU, VV)'};
%=============================================================================
% Handed an axes, a scene addition leaves that axes populated in every mode.
for k = 1:numel(sceneAdditions)
  statement = sceneAdditions{k};
  for m = 1:numel(modes)
    mode = modes{m};
    f = figure('Visible', 'off');
    ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
    ax2 = axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
    set(ax1, 'NextPlot', mode);
    line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
    line('Parent', ax2, 'XData', [0 1], 'YData', [0 1], 'Tag', 'BYSTANDERW');
    eval(statement);
    assert_isequal(numel(findobj(ax1, 'Tag', 'TARGETW')), 1, ...
      [statement, ' must not clear the axes it was given (NextPlot ', mode, ').']);
    assert_isequal(numel(findobj(ax2, 'Tag', 'BYSTANDERW')), 1, ...
      [statement, ' must not touch the current axes (NextPlot ', mode, ').']);
    assert_istrue(numel(ax1.Children) >= 2, ...
      [statement, ' must draw in the axes it was given (NextPlot ', mode, ').']);
    assert_isequal(gca(), ax2, ...
      [statement, ' must not change the current axes (NextPlot ', mode, ').']);
    close(f);
  end
end
%=============================================================================
% Handed nothing, a scene addition adds to the current axes and clears nothing.
untargetedAdditions = {...
  'streamline({[1 1; 2 2]})', ...
  'streamline(XX, YY, UU, VV, 1.5, 1.5)', ...
  'coneplot(X3, Y3, Z3, U3, V3, W3, 2, 2, 2)', ...
  'contourslice(V3D, 2, 2, 2)', ...
  'streamribbon(X3, Y3, Z3, U3, V3, W3, 1.5, 1.5, 1.5)', ...
  'streamtube(X3, Y3, Z3, U3, V3, W3, 1.5, 1.5, 1.5)', ...
  'streamslice(XX, YY, UU, VV)'};
%=============================================================================
for k = 1:numel(untargetedAdditions)
  statement = untargetedAdditions{k};
  f = figure('Visible', 'off');
  ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
  line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
  eval(statement);
  assert_isequal(numel(findobj(ax1, 'Tag', 'TARGETW')), 1, ...
    [statement, ' must not clear the current axes.']);
  assert_istrue(numel(ax1.Children) >= 2, ...
    [statement, ' must draw in the current axes.']);
  close(f);
end
%=============================================================================
% streamribbon and streamtube trace their stream lines through the same helper
% as streamline, then throw the lines away. Tracing them in the current axes
% wiped an axes the call was never handed, and stretched its limits on the way
% out; they are traced in the target itself instead. The witness here spans
% less than the stream data on purpose, so a leak would show up in the limits.
f = figure('Visible', 'off');
ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
ax2 = axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
line('Parent', ax2, 'XData', [0 0.5], 'YData', [0 0.5], 'Tag', 'BYSTANDERW');
limitsBefore = [ax2.XLim, ax2.YLim];
streamribbon(ax1, X3, Y3, Z3, U3, V3, W3, 1.5, 1.5, 1.5);
streamtube(ax1, X3, Y3, Z3, U3, V3, W3, 1.5, 1.5, 1.5);
assert_isequal(numel(findobj(ax2, 'Tag', 'BYSTANDERW')), 1);
assert_isequal([ax2.XLim, ax2.YLim], limitsBefore, ...
  'streamribbon and streamtube must not rescale an axes they were not given.');
close(f);
%=============================================================================
% pcolor does start a new plot, so it follows the NextPlot policy: hold on
% keeps the existing children, the other modes clear them.
f = figure('Visible', 'off');
ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
set(ax1, 'NextPlot', 'add');
line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
pcolor(ax1, peaks(5));
assert_isequal(numel(findobj(ax1, 'Tag', 'TARGETW')), 1);
assert_istrue(numel(ax1.Children) >= 2);
close(f);
%=============================================================================
for m = {'replace', 'replacechildren'}
  mode = m{1};
  f = figure('Visible', 'off');
  ax1 = axes('Parent', f, 'Position', [0.1 0.1 0.3 0.3]);
  ax2 = axes('Parent', f, 'Position', [0.5 0.5 0.3 0.3]);
  set(ax1, 'NextPlot', mode);
  line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
  line('Parent', ax2, 'XData', [0 1], 'YData', [0 1], 'Tag', 'BYSTANDERW');
  pcolor(ax1, peaks(5));
  assert_istrue(isempty(findobj(ax1, 'Tag', 'TARGETW')), ...
    ['pcolor must clear the axes it was given (NextPlot ', mode, ').']);
  assert_isequal(numel(findobj(ax2, 'Tag', 'BYSTANDERW')), 1);
  assert_isequal(gca(), ax2);
  close(f);
end
%=============================================================================
% hold on is the everyday spelling of NextPlot 'add'.
f = figure('Visible', 'off');
ax1 = axes('Parent', f);
line('Parent', ax1, 'XData', [0 1], 'YData', [0 1], 'Tag', 'TARGETW');
hold('on');
pcolor(peaks(5));
assert_isequal(numel(findobj(ax1, 'Tag', 'TARGETW')), 1);
close(f);
%=============================================================================

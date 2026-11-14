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
%=============================================================================
T = table({'a'; 'b'; 'a'; 'b'}, {'x'; 'x'; 'y'; 'y'}, [1; 2; 3; 4], 'VariableNames', {'u', 'v', 'w'});
% a figure as first argument or as 'Parent', with every data form
forms = {@(f) heatmap(f, magic(4)), @(f) heatmap(f, {'a', 'b'}, {'c', 'd'}, [1 2; 3 4]), ...
  @(f) heatmap(f, T, 'u', 'v'), @(f) heatmap(f, T, 'u', 'v', 'ColorVariable', 'w'), ...
  @(f) heatmap(f, magic(4), 'Title', 't'), @(f) heatmap(magic(4), 'Parent', f), ...
  @(f) heatmap(magic(4), 'Title', 't', 'Parent', f)};
for k = 1:numel(forms)
  f = figure('Visible', 'off');
  h = forms{k}(f);
  asserts.isequal(class(h), 'nelson.graphics.chart.HeatmapChart');
  % the chart reports its container, not the axes that draws it
  asserts.isequal(h.Parent, f);
  asserts.isequal(ancestor(h, 'figure'), f);
  asserts.istrue(isempty(ancestor(h, 'axes')));
  asserts.isequal(h.UserData.Axes.Parent, f);
  close(f);
end
%=============================================================================
% the chart goes to the given figure; the current one is left untouched
f = figure('Visible', 'off');
g = figure('Visible', 'off');
h = heatmap(f, magic(4));
asserts.isequal(h.Parent, f);
asserts.isequal(gcf(), g);
asserts.isequal(numel(allchild(g)), 0);
close(f);
close(g);
%=============================================================================
% in every container the chart takes the place of the current axes, with its legend
% and colorbar; the other children are kept
makers = {@(f) f, @(f) uipanel(f), @(f) uitab(uitabgroup(f)), @(f) uibuttongroup(f)};
for k = 1:numel(makers)
  f = figure('Visible', 'off');
  c = makers{k}(f);
  other = axes('Parent', c, 'Position', [0.05 0.1 0.3 0.8]);
  ax = axes('Parent', c, 'Position', [0.5 0.1 0.4 0.8]);
  plot(ax, 1:3);
  legend(ax, 'a');
  colorbar(ax);
  button = uicontrol(c, 'Style', 'pushbutton');
  h = heatmap(c, magic(4));
  asserts.isequal(h.Parent, c);
  asserts.isequal(h.UserData.Axes, ax);
  asserts.istrue(isgraphics(other, 'axes'));
  asserts.istrue(isgraphics(button));
  asserts.isequal(childTypes(c), {'axes', 'heatmap', 'uicontrol'});
  % a second chart replaces the first one
  h2 = heatmap(c, magic(3));
  asserts.isequal(h2.UserData.Axes, ax);
  asserts.isequal(h2.ColorData, magic(3));
  asserts.isequal(childTypes(c), {'axes', 'heatmap', 'uicontrol'});
  close(f);
end
%=============================================================================
% without an axes in the container a new one is created
f = uifigure('Visible', 'off');
gl = uigridlayout(f, [1 1]);
h = heatmap(gl, magic(4));
asserts.isequal(h.Parent, gl);
asserts.isequal(h.UserData.Axes.Parent, gl);
close(f);
f = uifigure('Visible', 'off');
h = heatmap(f, magic(4));
asserts.isequal(h.Parent, f);
close(f);
%=============================================================================
% the chart starts at the position of the axes it replaces
f = figure('Visible', 'off');
kept = subplot(1, 2, 1, 'Parent', f);
replaced = subplot(1, 2, 2, 'Parent', f);
position = replaced.Position;
h = heatmap(f, magic(4));
asserts.istrue(isgraphics(kept, 'axes'));
asserts.isapprox(h.Position, position, 1e-12);
close(f);
%=============================================================================
% a tiled layout: the current tile is replaced, nexttile moves to another one
f = figure('Visible', 'off');
tl = tiledlayout(f, 1, 2);
h1 = heatmap(tl, magic(4));
firstAxes = h1.UserData.Axes;
asserts.isequal(h1.Parent, tl);
h2 = heatmap(tl, magic(3));
asserts.isequal(h2.UserData.Axes, firstAxes);
nexttile(tl);
h3 = heatmap(tl, magic(2));
asserts.isfalse(isequal(h3.UserData.Axes, firstAxes));
asserts.isequal(h3.Parent, tl);
% no parent given: the current tile
h4 = heatmap(magic(2));
asserts.isequal(h4.UserData.Axes, h3.UserData.Axes);
asserts.isequal(h4.Parent, tl);
close(f);
%=============================================================================
% assigning Parent moves the chart, its axes and its colorbar
f = figure('Visible', 'off');
g = figure('Visible', 'off');
h = heatmap(f, magic(4));
h.Parent = g;
asserts.isequal(h.Parent, g);
asserts.isequal(numel(f.Children), 0);
asserts.isequal(childTypes(g), {'heatmap'});
p = uipanel(g);
set(h, 'Parent', p);
asserts.isequal(h.Parent, p);
asserts.isequal(childTypes(g), {'uipanel'});
asserts.isequal(childTypes(p), {'heatmap'});
% the moved chart still follows its properties
h.Title = 'moved';
asserts.isequal(h.UserData.Axes.Parent, p);
asserts.isequal(childTypes(p), {'heatmap'});
invalidParent = getString(message('Nelson:heatmap:invalidParent'));
ax = axes('Parent', f);
assertInvalid(@() set(h, 'Parent', ax), invalidParent);
assertInvalid(@() set(h, 'Parent', groot), invalidParent);
asserts.isequal(h.Parent, p);
%=============================================================================
% an axes, a group, a tab group or a control is not a parent
assertInvalid(@() heatmap(ax, magic(4)), invalidParent);
assertInvalid(@() heatmap(magic(4), 'Parent', ax), invalidParent);
assertInvalid(@() heatmap(hggroup('Parent', ax), magic(4)), invalidParent);
assertInvalid(@() heatmap(uitabgroup(f), magic(4)), invalidParent);
assertInvalid(@() heatmap(uicontrol(f), magic(4)), invalidParent);
assertInvalid(@() heatmap([f g], magic(4)), invalidParent);
assertInvalid(@() heatmap(groot, magic(4)), invalidParent);
d = figure('Visible', 'off');
delete(d);
assertInvalid(@() heatmap(d, magic(4)), getString(message('Nelson:heatmap:deletedParent')));
assertInvalid(@() heatmap(f), getString(message('Nelson:heatmap:colorDataExpected')));
close(f);
close(g);
%=============================================================================
% the chart stands for the axes that draws it and for its colorbar
f = figure('Visible', 'off');
p = uipanel(f);
h = heatmap(p, magic(4));
asserts.isequal(p.Children, h);
asserts.isequal(allchild(p), h);
asserts.isequal(findobj(f, 'Type', 'heatmap'), h);
asserts.istrue(isempty(findobj(f, 'Type', 'axes')));
asserts.istrue(isempty(findall(f, 'Type', 'colorbar')));
asserts.isequal(f.CurrentAxes, h);
figure(f);
set(f, 'Visible', 'off');
asserts.isequal(gca(), h);
% its position is the position of that axes
ax = h.UserData.Axes;
asserts.isequal(h.Units, 'normalized');
h.Position = [0.2 0.2 0.5 0.5];
asserts.isapprox(h.Position, [0.2 0.2 0.5 0.5], 1e-12);
asserts.isapprox(ax.Position, [0.2 0.2 0.5 0.5], 1e-12);
h.OuterPosition = [0 0 0.5 1];
asserts.isapprox(h.OuterPosition, [0 0 0.5 1], 1e-12);
asserts.isapprox(ax.OuterPosition, [0 0 0.5 1], 1e-12);
% a Children list read from the container can be assigned back
button = uicontrol(p, 'Style', 'pushbutton');
p.Children = p.Children([2 1]);
asserts.istrue(isgraphics(ax, 'axes'));
asserts.isequal(childTypes(p), {'heatmap', 'uicontrol'});
asserts.isequal(h.UserData.Axes, ax);
% deleting the chart removes its axes and its colorbar
delete(h);
asserts.isequal(childTypes(p), {'uicontrol'});
asserts.isequal(numel(allchild(p)), 1);
asserts.istrue(isempty(f.CurrentAxes));
close(f);
%=============================================================================
% functions that act on the current axes act on the chart, or are refused
unsupported = 'Nelson:heatmap:unsupportedFunction';
f = figure('Visible', 'off');
h = heatmap(f, magic(4));
title('t');
xlabel('x');
ylabel('y');
asserts.isequal(char(h.Title), 't');
asserts.isequal(char(h.XLabel), 'x');
asserts.isequal(char(h.YLabel), 'y');
title(h, 'u');
asserts.isequal(char(h.Title), 'u');
grid('off');
asserts.isequal(char(h.GridVisible), 'off');
grid('on');
asserts.isequal(char(h.GridVisible), 'on');
colormap(gray(8));
asserts.isequal(h.Colormap, gray(8));
colormap(h, gray(4));
asserts.isequal(colormap(), gray(4));
clim([0 20]);
asserts.isequal(h.ColorLimits, [0 20]);
asserts.isequal(clim(), [0 20]);
colorbar('off');
asserts.isequal(char(h.ColorbarVisible), 'off');
colorbar();
asserts.isequal(char(h.ColorbarVisible), 'on');
asserts.isequal(axes(h), h);
assertInvalid(@() hold('on'), getString(message(unsupported, 'hold')));
assertInvalid(@() hold(h, 'on'), getString(message(unsupported, 'hold')));
assertInvalid(@() axis('tight'), getString(message(unsupported, 'axis')));
assertInvalid(@() cla(), getString(message(unsupported, 'cla')));
assertInvalid(@() legend('a'), getString(message(unsupported, 'legend')));
assertInvalid(@() line([0 1], [0 1]), getString(message('Nelson:heatmap:invalidChild', 'line')));
assertInvalid(@() text(1, 1, 'a'), getString(message('Nelson:heatmap:invalidChild', 'text')));
asserts.isequal(f.Children, h);
% a plot takes the place of the chart
plot(1:3);
asserts.isequal(childTypes(f), {'axes'});
% a heatmap cannot be added to a held axes
hold('on');
assertInvalid(@() heatmap(magic(4)), getString(message('Nelson:heatmap:holdOn')));
hold('off');
h = heatmap(magic(4));
asserts.isequal(childTypes(f), {'heatmap'});
subplot(1, 2, 1);
asserts.isequal(childTypes(f), {'axes'});
h = heatmap(f, magic(4));
clf(f);
asserts.isequal(numel(allchild(f)), 0);
close(f);
%=============================================================================
% line and hggroup given a Parent do not create an axes in the current figure
f = figure('Visible', 'off');
g = figure('Visible', 'off');
ax = axes('Parent', f);
l = line('Parent', ax, 'XData', [0 1], 'YData', [0 1]);
grp = hggroup('Parent', ax);
asserts.isequal(ancestor(l, 'axes'), ax);
asserts.isequal(ancestor(grp, 'axes'), ax);
asserts.isequal(numel(allchild(g)), 0);
close(f);
close(g);
%=============================================================================
function types = childTypes(container)
  children = container.Children;
  types = cell(1, numel(children));
  for k = 1:numel(children)
    types{k} = children(k).Type;
  end
  types = sort(types);
end
%=============================================================================
function assertInvalid(fn, expected)
  msg = '';
  try
    fn();
  catch
    e = lasterror();
    msg = e.message;
  end
  asserts.istrue(contains(msg, expected), msg);
end
%=============================================================================

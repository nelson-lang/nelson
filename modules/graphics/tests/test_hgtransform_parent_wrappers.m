%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
%=============================================================================
[X, Y, Z] = peaks(8);
makers = {
  @(t) surface('Parent', t, 'XData', X, 'YData', Y, 'ZData', Z), ...
  @(t) surface(t, X, Y, Z), ...
  @(t) surf(X, Y, Z, 'Parent', t), ...
  @(t) mesh(X, Y, Z, 'Parent', t), ...
  @(t) image(magic(8), 'Parent', t), ...
  @(t) fill([0 1 1], [0 0 1], 'r', 'Parent', t), ...
  @(t) fill3([0 1 1], [0 0 1], [0 0 1], 'g', 'Parent', t), ...
  @(t) fill3(t, [0 1 1], [0 0 1], [0 0 1], 'g'), ...
  @(t) patch([0 1 1 0], [0 0 1 1], [0.1 0.4 0.7 0.9], 'Parent', t), ...
  @(t) plot([0 1], [1 0], 'Parent', t), ...
  @(t) plot3([0 1], [1 0], [0 1], 'Parent', t), ...
  @(t) text(0.5, 0.5, 'hello', 'Parent', t), ...
  @(t) scatter3([1 2 3], [4 5 6], [7 8 9], 20, [0.1 0.5 0.9], 'Parent', t)
};
for k = 1:numel(makers)
  f = figure('Visible', 'off');
  ax = axes('Parent', f);
  t = hgtransform('Parent', ax);
  h = makers{k}(t);
  asserts.istrue(isvalid(t));
  asserts.istrue(~isempty(h));
  for j = 1:numel(h)
    % the transform must still be alive (not wiped by newplot and its handle
    % recycled by the new object), and own the object.
    asserts.isequal(h(j).Parent.Type, 'hgtransform');
    asserts.isequal(h(j).Parent, t);
    asserts.istrue(any(t.Children == h(j)));
  end
  asserts.isequal(t.Parent, ax);
  asserts.isequal(ancestor(h(1), 'axes'), ax);
  % the figure still renders through the transform
  drawnow();
  fr = getframe(f);
  asserts.istrue(size(fr.cdata, 3) == 3);
  close(f);
end
%=============================================================================
f = figure('Visible', 'off');
ax = axes('Parent', f);
t = hgtransform('Parent', ax);
surf(X, Y, Z, 'Parent', t);
v = get(ax, 'View');
asserts.istrue(abs(v(2) - 90) > 1);
close(f);
%=============================================================================

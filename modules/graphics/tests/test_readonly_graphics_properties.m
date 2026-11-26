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
% Computed graphics properties are read-only: set() fails even with their own
% value. Read-only handle properties refuse another object, while a nested
% assignment (h.Label.String = ...) still works. Neighbour properties stay
% settable.
%=============================================================================
f = figure('Visible', 'off');
ax = axes(f, 'Position', [0.1 0.1 0.35 0.8]);
axContour = axes(f, 'Position', [0.55 0.1 0.35 0.8]);
b = bar(ax, 1:3);
l = line(ax, 1:3, 1:3);
c = colorbar(ax);
t = text(ax, 0.5, 0.5, 'hi');
u = uicontrol(f);
readOnly = {ax, 'CurrentPoint'; ax, 'TightInset'; ax, 'NextSeriesIndex'; ...
  t, 'Extent'; u, 'Extent'; b, 'XEndPoints'; b, 'YEndPoints'};
for k = 1:size(readOnly, 1)
  h = readOnly{k, 1};
  name = readOnly{k, 2};
  assert_istrue(setFails(h, name, get(h, name)));
end
[~, ct] = contour(axContour, peaks(10));
assert_istrue(setFails(ct, 'ContourMatrix', ct.ContourMatrix));
%=============================================================================
other = text(ax, 0.1, 0.1, 'other');
handleReadOnly = {c, 'Label'; b, 'BaseLine'; l, 'Annotation'; l, 'DataTipTemplate'; ax, 'Legend'};
for k = 1:size(handleReadOnly, 1)
  assert_istrue(setFails(handleReadOnly{k, 1}, handleReadOnly{k, 2}, other));
end
c.Label.String = 'Temperature';
assert_isequal(c.Label.String, 'Temperature');
%=============================================================================
assert_isfalse(setFails(ax, 'XLim', [0 10]));
assert_isfalse(setFails(t, 'Position', [0.2 0.3 0]));
assert_isfalse(setFails(l, 'Color', [1 0 0]));
assert_isfalse(setFails(f, 'CurrentPoint', [10 20]));
delete(f);
%=============================================================================
function failed = setFails(h, name, value)
  failed = false;
  try
    set(h, name, value);
  catch
    failed = true;
  end
end
%=============================================================================

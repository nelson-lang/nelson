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
f = figure('Visible', 'off');
%=============================================================================
kinds = {'arrow', 'doublearrow', 'ellipse', 'line', 'rectangle', 'textarrow', 'textbox'};
common = {'ButtonDownFcn', 'ContextMenu', 'HandleVisibility', 'HitTest', ...
  'PickableParts', 'Selected', 'SelectionHighlight'};
%=============================================================================
for k = 1:numel(kinds)
  h = annotation(kinds{k});
  p = properties(h);
  asserts.istrue(all(ismember(common, p)));
  % default values must match the reference common object semantics
  asserts.isequal(char(h.HandleVisibility), 'on');
  asserts.isequal(char(h.HitTest), 'on');
  asserts.isequal(h.PickableParts, 'visible');
  asserts.isequal(char(h.Selected), 'off');
  asserts.isequal(char(h.SelectionHighlight), 'on');
  asserts.isequal(h.ButtonDownFcn, '');
  % settable
  h.HitTest = 'off';
  asserts.isequal(char(h.HitTest), 'off');
  h.Selected = 'on';
  asserts.isequal(char(h.Selected), 'on');
  h.SelectionHighlight = 'off';
  asserts.isequal(char(h.SelectionHighlight), 'off');
  h.HandleVisibility = 'off';
  asserts.isequal(char(h.HandleVisibility), 'off');
  h.PickableParts = 'none';
  asserts.isequal(h.PickableParts, 'none');
  h.ButtonDownFcn = 'disp(1)';
  asserts.isequal(h.ButtonDownFcn, 'disp(1)');
end
%=============================================================================

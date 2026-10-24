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
plot(1:10);
%=============================================================================
headStyles = annotation_head_styles();
%=============================================================================
da = annotation('doublearrow', [0.1 0.8], [0.8 0.2], ...
  'Head1Style', 'vback1', 'Head2Style', 'cback3', ...
  'Head1Length', 12, 'Head2Width', 14);
asserts.isequal(da.Type, 'doubleendarrowshape');
asserts.isequal(da.Head1Style, 'vback1');
asserts.isequal(da.Head2Style, 'cback3');
asserts.isequal(da.Head1Length, 12);
asserts.isequal(da.Head2Width, 14);
for k = 1:numel(headStyles)
  da.Head1Style = headStyles{k};
  da.Head2Style = headStyles{k};
  asserts.isequal(da.Head1Style, headStyles{k});
  asserts.isequal(da.Head2Style, headStyles{k});
end
%=============================================================================

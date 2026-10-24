%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
f = figure('Visible', 'off');
a1 = subplot(2, 2, 1); plot(1:5); title('Signal');
a2 = subplot(2, 2, 2); plot3([0 1 2], [0 1 0], [0 1 2]); title('Helix');
a3 = subplot(2, 2, 3); h = surf(peaks(25)); title('Surface');
a4 = subplot(2, 2, 4); plot(5:-1:1); title('Oscillation');
drawnow();
filled = __web_display_list__(f);
h.FaceColor = 'none';
drawnow();
wireframe = __web_display_list__(f);
asserts.istrue(contains(wireframe, 'data:image/png;base64,'));
asserts.isequal(numel(strfind(wireframe, '"kind":"text"')), ...
  numel(strfind(filled, '"kind":"text"')));
asserts.istrue(numel(strfind(wireframe, '"kind":"text"')) > 20);
for name = {'Signal', 'Helix', 'Surface', 'Oscillation'}
  asserts.istrue(contains(wireframe, name{1}));
end
asserts.isequal(a1.XTickMode, 'auto');
asserts.isequal(a4.YTickMode, 'auto');
close(f);

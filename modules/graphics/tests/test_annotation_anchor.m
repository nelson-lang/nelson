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
fAnchor = figure('Visible', 'off', 'Position', [100 100 200 200], 'Color', [1 1 1]);
annotation('rectangle', [0.2 0.2 0.2 0.1], 'FaceColor', 'yellow', ...
  'Color', 'none', 'Rotation', 90);
drawnow();
anchorFrame = getframe(fAnchor);
anchorData = double(anchorFrame.cdata) / 255;
anchorYellow = anchorData(:, :, 1) > 0.8 & anchorData(:, :, 2) > 0.8 ...
  & anchorData(:, :, 3) < 0.3;
asserts.istrue(any(anchorYellow(:)));
close(fAnchor);
%=============================================================================

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
figure();
ha = {'left', 'center', 'right'};
va = {'bottom', 'middle', 'top'};
color = {'red', 'green', 'blue'};
x = [0.25 0.5 0.75];
y = x;
for t = 0:45:359;
  for nh = 1:numel(ha)
    for nv = 1:numel(va)
      text(x(nh), y(nv), 'Nelson', ...
        'Rotation', t, ...
        'HorizontalAlignment', ha{nh}, ...
        'VerticalAlignment', va{nv}, ...
        'Color', color{nv});
    end
  end
end
axis([0 1 0 1]);
title(_('Text alignment and rotation (0:45:360 degrees)'));
xlabel(_('Horizontal alignment'));
ylabel(_('Vertical alignment'));
close all;
%=============================================================================

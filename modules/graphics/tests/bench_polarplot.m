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
tic();
f = figure('Visible', 'off', 'InnerPosition', [100 100 560 420]);
theta = linspace(0, 24 * pi, 12000)';
rho = [sin(theta) + 2, cos(theta / 2) + 2.5, theta / max(theta) * 4];
polarplot(theta, rho, 'LineWidth', 0.75);
hold on
polarplot(theta, 2 + 0.5 * sin(8 * theta), '--');
hold off
drawnow();
close(f);
toc();
%=============================================================================
tic();
for k = 1:5
  f = figure('Visible', 'off', 'InnerPosition', [100 100 560 420]);
  theta = 0:0.01:2 * pi;
  rho = sin(2 * theta) .* cos(2 * theta);
  polarplot(theta, rho);
  drawnow();
  close(f);
end
toc();
%=============================================================================

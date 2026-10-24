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
rho = 10:5:70;
p = polarplot(rho, '-o');
asserts.isequal(p.RData(:), rho(:));
asserts.isapprox(p.ThetaData(:), linspace(0, 2 * pi, numel(rho))', 1e-12);
asserts.isequal(rlim(), [0 70]);
close(f);
%=============================================================================
f = figure('Visible', 'off');
z = [2 + 3i 2 -1 + 4i 3 - 4i];
p = polarplot(z, '*');
asserts.isapprox(p.ThetaData(:), angle(z(:)), 1e-12);
asserts.isapprox(p.RData(:), abs(z(:)), 1e-12);
asserts.isequal(p.LineStyle, 'none');
asserts.isequal(p.Marker, '*');
close(f);
%=============================================================================
f = figure('Visible', 'off');
theta = linspace(0, 6 * pi, 20)';
rho = [theta / 10 theta / 12];
p = polarplot(theta, rho);
asserts.isequal(size(p), [2 1]);
hold on
p2 = polarplot(theta, theta / 14, '--');
hold off
asserts.isequal(numel([p; p2]), 3);
asserts.isequal(p2.LineStyle, '--');
close(f);
%=============================================================================

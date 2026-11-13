%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
% <--ADV-CLI MODE-->
%=============================================================================
f = figure('Visible', 'off', 'Position', [100 100 560 420]);
ax = axes(f);
t = title('A');
drawnow();
asserts.isapprox(t.Position, [0.5 1.00642523364486 0.5], 1e-4);
ax.Position = [0.2 0.2 0.5 0.3];
drawnow();
asserts.isapprox(t.FontSize, 9.35, 1e-12);
asserts.isapprox(t.Position, [0.5 1.01479430379747 0.5], 1e-4);
t.FontSize = 20;
drawnow();
asserts.isapprox(t.Position, [0.5 1.03164556962025 0.5], 1e-4);
t.Position = [0.3 1.2 0];
drawnow();
ax.Position = [0.1 0.1 0.6 0.6];
drawnow();
asserts.isequal(t.Position, [0.3 1.2 0]);
t.String = 'BBB';
drawnow();
asserts.isequal(t.Position, [0.3 1.2 0]);
close(f);
%=============================================================================

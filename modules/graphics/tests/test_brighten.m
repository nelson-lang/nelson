%=============================================================================
% Copyright (c) 2026 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
m = jet(16);
%=============================================================================
% beta > 0 brightens (values do not decrease), beta < 0 darkens.
b = brighten(m, 0.5);
assert_istrue(all(b(:) >= m(:) - 1e-12));
d = brighten(m, -0.5);
assert_istrue(all(d(:) <= m(:) + 1e-12));
%=============================================================================
% beta == 0 is the identity.
assert_isapprox(brighten(m, 0), m, 1e-12);
%=============================================================================
% result stays a valid colormap in [0, 1].
assert_istrue(all(b(:) >= 0 & b(:) <= 1));
assert_istrue(all(d(:) >= 0 & d(:) <= 1));
%=============================================================================
% the no-output form changes the current figure's colormap.
f = figure('Visible', 'off');
colormap(jet(16));
brighten(0.5);
cm = colormap();
assert_isapprox(cm, brighten(jet(16), 0.5), 1e-12);
close(f);
%=============================================================================
% beta outside (-1, 1) is rejected.
assert_checkerror('brighten(jet(8), 1.5)', 'BETA must be a scalar in the interval (-1, 1).');
%=============================================================================

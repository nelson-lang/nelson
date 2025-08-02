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
f = figure();
rng default
t = linspace(-pi,pi,100);
X = sin(t) + (0.25 * rand(size(t)));
windowSize = 5; 
b = (1/windowSize)*ones(1,windowSize);
a = 1;
y = filter(b, a, X);
plot(t, X)
hold on
plot(t, y)
legend(_('Input Data'), _('Filtered Data'));

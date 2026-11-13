%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Plot the frequency response of a low-pass transfer function.
system = tf(25, [1, 4, 25]);
% bode opens its own figure with the magnitude and phase plots.
bode(system);
set(gcf(), 'Name', 'Low-pass frequency response', 'NumberTitle', 'off');

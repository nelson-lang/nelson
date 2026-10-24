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
figure();
bode(system);
title('Low-pass frequency response');

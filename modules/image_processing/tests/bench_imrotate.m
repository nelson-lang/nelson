%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--INDEX 64 BIT REQUIRED-->
%=============================================================================
addpath([fileparts(nfilename('fullpathext'), 'path'), '/private']);
I = rand(2048, 2048, 3);
timerStart = tic();
J = imrotate(I, 33);
elapsed = toc(timerStart);
ip_bench_report('imrotate_rgb2048_nearest_loose', elapsed, 12.0);
asserts.isequal(size(J, 3), 3);
asserts.isequal(class(J), 'double');
clear I J timerStart elapsed;
%=============================================================================

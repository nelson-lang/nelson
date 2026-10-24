%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([fileparts(nfilename('fullpathext'), 'path'), '/private']);
RGB64 = uint8(reshape(mod(1:(64 * 64 * 3), 256), [64 64 3]));
timerStart = tic();
for k = 0:359
  J64 = imrotate(RGB64, k, 'bilinear', 'crop');
end
elapsed = toc(timerStart);
ip_bench_report('imrotate_uint8_rgb64_bilinear_crop_360', elapsed, 2.0);
asserts.isequal(size(J64), [64 64 3]);
asserts.isequal(class(J64), 'uint8');
clear RGB64 J64 k timerStart elapsed;
%=============================================================================

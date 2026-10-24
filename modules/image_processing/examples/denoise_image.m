%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Remove impulse noise from a synthetic grayscale image.
imageData = repmat(linspace(0, 1, 160), 120, 1);
imageData(20:20:end, 15:23:end) = 1;
filtered = medfilt2(imageData, [3, 3], 'symmetric');
figure();
subplot(1, 2, 1); imshow(imageData); title('Noisy');
subplot(1, 2, 2); imshow(filtered); title('Median filtered');

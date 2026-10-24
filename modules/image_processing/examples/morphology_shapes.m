%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Clean and outline binary shapes with morphology.
binaryImage = false(100, 140);
binaryImage(20:75, 25:60) = true;
binaryImage(35:85, 80:120) = true;
binaryImage(10, 10) = true;
cleaned = bwmorph(binaryImage, 'clean');
outline = bwperim(cleaned);
figure();
imshow(outline);
title('Detected shape boundaries');

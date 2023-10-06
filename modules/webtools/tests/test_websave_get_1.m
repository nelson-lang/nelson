%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
url = 'https://apod.nasa.gov/apod/image/2310/MoValleyEclipse.jpg';
filename = [tempdir(), 'MoValleyEclipse_1.jpg'];
outfilename = websave(filename, url);
%=============================================================================

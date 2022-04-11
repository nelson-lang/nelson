%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('websave'), -1);
assert_isequal(nargout('websave'), 1);
%=============================================================================
url = 'https://www.nasa.gov/sites/default/files/styles/full_width/public/thumbnails/image/stsci-h-p2016a-m-2000x1374.png';
filename = [tempdir(), 'image.jpg'];
destination_filename = websave(filename, url);
assert_istrue(isfile(destination_filename))
%=============================================================================

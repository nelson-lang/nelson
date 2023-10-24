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
url = 'https://apod.nasa.gov/apod/image/2310/MoValleyEclipse.jpg';
filename = [tempdir(), 'MoValleyEclipse_2.jpg'];
destination_filename = websave(filename, url);
assert_istrue(isfile(destination_filename))
%=============================================================================

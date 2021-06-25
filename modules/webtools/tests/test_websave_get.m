%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
url = 'https://www.nasa.gov/sites/default/files/styles/full_width/public/thumbnails/image/stsci-h-p2016a-m-2000x1374.png';
filename = [tempdir(), 'p0714aa.jpg'];
outfilename = websave(filename, url);
%=============================================================================
o = weboptions('RequestMethod', 'get');
filename = [tempdir(), 'test_websave_get.json'];
fullname = websave(filename, 'http://httpbin.org/get', o);
R = jsondecode(fileread(fullname));
assert_isequal(R.url, 'http://httpbin.org/get');
%=============================================================================
filename = [tempdir(), 'test_websave_get.json'];
fullname = websave(filename, 'http://httpbin.org/get');
R = jsondecode(fileread(fullname));
assert_isequal(R.url, 'http://httpbin.org/get');
%=============================================================================

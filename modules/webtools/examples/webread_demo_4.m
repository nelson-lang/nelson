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
width = 1024;
height = 768;
%=============================================================================
minimum_age = 35;
gender = 'female';
%=============================================================================
for i = 1:1000
    info = webread('https://fakeface.rest/face/json', 'gender', gender, 'minimum_age', minimum_age)
    result_filename = websave([tempdir(), info.filename], info.image_url);
    html_line = ['<img src="', result_filename, '" width="', int2str(width), '" height="', int2str(height), '">'];
    clc
    inserthtml(html_line)
end


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
h5_directory = [modulepath('hdf5'), '/tests/h5'];
h5_filename = [h5_directory, '/', 'example.h5'];
R = h5ls(h5_filename);
REF = {
 '/', 'Group';
 'comment', 'Attribute';
 'mldata', 'Attribute';
 'name', 'Attribute';
 '/data', 'Group';
 '/data/data', 'Dataset';
 '/data/label', 'Dataset';
 '/data_descr', 'Group';
 '/data_descr/names', 'Dataset';
 '/data_descr/ordering', 'Dataset';
};
assert_isequal(R, REF);
%=============================================================================
R = h5ls(h5_filename, '/data_descr');
REF = {
'/names', 'Dataset';
'/ordering', 'Dataset';
};
assert_isequal(R, REF);
%=============================================================================
h5ls(h5_filename);
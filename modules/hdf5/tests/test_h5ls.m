%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
h5_directory = [modulepath('hdf5', 'tests'), '/h5'];
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
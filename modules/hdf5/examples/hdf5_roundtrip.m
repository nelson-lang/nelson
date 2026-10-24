%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Store a numeric dataset in HDF5 and read it back.
filename = [tempdir(), 'nelson-example.h5'];
if isfile(filename), rmfile(filename); end
values = reshape(1:12, 3, 4);
h5create(filename, '/measurements', size(values));
h5write(filename, '/measurements', values);
restored = h5read(filename, '/measurements');
disp(restored);

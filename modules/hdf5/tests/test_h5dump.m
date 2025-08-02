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
h5_filename = 'example.h5';
cd(h5_directory);
R = h5dump(h5_filename);
IDX_NEWLINE_R = strfind(R, newline);
R = R(IDX_NEWLINE_R(1): end);
REF = fileread([modulepath('hdf5', 'tests'), '/h5dump.ref']);
IDX_NEWLINE_REF = strfind(REF, newline);
REF = REF(IDX_NEWLINE_REF(1): end);
assert_isequal(R, REF);
%=============================================================================
h5dump(h5_filename)
%=============================================================================

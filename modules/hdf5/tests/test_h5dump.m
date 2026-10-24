%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.istrue(isbuiltin('h5dump'));
asserts.isfalse(ismacro('h5dump'));
asserts.isequal(nargout('h5dump'), 1);
asserts.isequal(nargin('h5dump'), 2);
%=============================================================================
h5_filename = [tempname(), '.h5'];
h5write(h5_filename, '/values', [1 2 3; 4 5 6]);
h5writeatt(h5_filename, '/', 'title', 'demo');
%=============================================================================
R = h5dump(h5_filename);
asserts.istrue(contains(R, 'HDF5 "'));
asserts.istrue(contains(R, 'GROUP "/"'));
asserts.istrue(contains(R, 'ATTRIBUTE "title"'));
asserts.istrue(contains(R, 'DATASET "values"'));
asserts.istrue(contains(R, 'DATATYPE  H5T_IEEE_F64'));
asserts.istrue(contains(R, 'DATASPACE  SIMPLE'));
asserts.istrue(contains(R, 'DATA {'));
%=============================================================================
R = h5dump(h5_filename, '/values');
asserts.istrue(contains(R, 'DATASET "values"'));
asserts.isfalse(contains(R, 'ATTRIBUTE "title"'));
%=============================================================================
h5dump(h5_filename);
%=============================================================================
asserts.checkerror('h5dump()', _('Not enough input arguments.'));
asserts.checkerror('h5dump(h5_filename, ''values'')', _('Second argument must start with ''/''.'));
asserts.checkerror('h5dump(h5_filename, ''/missing'')', _('Specified HDF5 object location could not be opened.'));
%=============================================================================

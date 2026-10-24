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
asserts.istrue(isbuiltin('h5ls'));
asserts.isfalse(ismacro('h5ls'));
asserts.isequal(nargout('h5ls'), 1);
asserts.isequal(nargin('h5ls'), 2);
%=============================================================================
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
asserts.isequal(R, REF);
%=============================================================================
R = h5ls(h5_filename, '/data_descr');
REF = {
  '/names', 'Dataset';
  '/ordering', 'Dataset';
};
asserts.isequal(R, REF);
%=============================================================================
h5ls(h5_filename);
%=============================================================================
asserts.checkerror('h5ls()', _('Not enough input arguments.'));
asserts.checkerror('h5ls(h5_filename, ''data_descr'')', _('Second argument must start with ''/''.'));
% the location-path condition also carries a stable identifier
asserts.checkerror('h5ls(h5_filename, ''data_descr'')', _('Second argument must start with ''/''.'), 'Nelson:hdf5:invalidLocationPath');

%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
v = ver();
assert_istrue(isstruct(v));
assert_isequal(fieldnames(v), {'Name'; 'Version'; 'Release'; 'Date'});
assert_isequal(v.Name, 'Nelson');
assert_isequal(v.Version, strtok(version(), ' '));
%=============================================================================
v = ver('Nelson');
assert_isequal(v.Name, 'Nelson');
%=============================================================================
v = ver('nElSoN');
assert_isequal(v.Name, 'Nelson');
%=============================================================================
% an unknown product returns an empty struct array with the same fields
v = ver('SomeUnknownProduct');
assert_istrue(isempty(v));
assert_isequal(fieldnames(v), {'Name'; 'Version'; 'Release'; 'Date'});
%=============================================================================
assert_checkerror('ver(1, 2)', _('Too many input arguments.'));
%=============================================================================

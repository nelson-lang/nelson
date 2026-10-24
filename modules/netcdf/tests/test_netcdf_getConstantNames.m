%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
asserts.isequal(nargin('netcdf.getConstantNames'), -1);
asserts.isequal(nargout('netcdf.getConstantNames'), -1);
ok = false; try; netcdf.getConstantNames(1); catch; ok = true; end; asserts.istrue(ok);
names = netcdf.getConstantNames();
asserts.istrue(iscell(names));
asserts.istrue(any(strcmp(names, 'NC_GLOBAL')));
asserts.istrue(any(strcmp(names, 'NC_DOUBLE')));
%=============================================================================





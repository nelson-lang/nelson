%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ispc() && ~havecompiler()
  configuremsvc();
end
%=============================================================================
tmpPath = tempname();
mkdir(tmpPath);
status = copyfile('mexInterleavedComplex.c', tmpPath);
assert_istrue(status);
cd(tmpPath);
%=============================================================================
mex('-output', 'mexInterleavedComplexOn', '-R2018a', 'mexInterleavedComplex.c' );
mex('-output', 'mexInterleavedComplexOff', '-R2017b', 'mexInterleavedComplex.c' );
addpath(pwd())
%=============================================================================
CplxOn = mexInterleavedComplexOn();
assert_isequal(CplxOn, complex(1.53, 1.63));
%=============================================================================
CplxOff = mexInterleavedComplexOff();
assert_isequal(CplxOff, complex(1.73, 4.76));
%=============================================================================

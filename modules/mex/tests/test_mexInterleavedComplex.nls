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
tmpPath = tempname();
mkdir(tmpPath);
status = copyfile('mexInterleavedComplex.c', tmpPath);
assert_istrue(status);
cd(tmpPath);
%=============================================================================
mex('-output', 'mexInterleavedComplexOn', '-R2018a', 'mexInterleavedComplex.c' );
mex('-output', 'mexInterleavedComplexOff', '-R2017b', 'mexInterleavedComplex.c' );
run('loader.m');
%=============================================================================
CplxOn = mexInterleavedComplexOn();
assert_isequal(CplxOn, complex(1.53, 1.63));
%=============================================================================
CplxOff = mexInterleavedComplexOff();
assert_isequal(CplxOff, complex(1.73, 4.76));
%=============================================================================

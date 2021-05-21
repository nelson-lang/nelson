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
dlgenerategateway(tempdir(), 'test_gw', {{'cpp_sum', 1, 2, 'CPP_BUILTIN'}, {'cpp_sub', 3, 4}, {'cpp_sub2', 3, 4, 'CPP_BUILTIN_WITH_EVALUATOR'}});
assert_istrue(isfile([tempdir(), 'Gateway.cpp']));
txt = fileread([tempdir(), 'Gateway.cpp']);
assert_istrue(contains(txt, '{ "cpp_sum", (void*)cpp_sumBuiltin, 1, 2, CPP_BUILTIN },'));
assert_istrue(contains(txt, '{ "cpp_sub", (void*)cpp_subBuiltin, 3, 4, CPP_BUILTIN },'));
assert_istrue(contains(txt, '{ "cpp_sub2", (void*)cpp_sub2Builtin, 3, 4, CPP_BUILTIN_WITH_EVALUATOR },'));
%=============================================================================